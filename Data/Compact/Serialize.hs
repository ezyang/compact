{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Compact.Serialize
    ( writeCompact, unsafeReadCompact
    , hPutCompact, hUnsafeGetCompact
    ) where

import Type.Reflection
import Control.Monad
import Data.Monoid
import Data.Word
import System.IO

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import qualified Data.Binary as B
import qualified Data.ByteString.Lazy as BSL

import GHC.Compact
import GHC.Compact.Serialized

newtype CompactFile a = CompactFile (SerializedCompact a)

-- NB: This instance cannot be put on SerializedCompact as
-- ghc-compact does not have a binary dependency
instance (Typeable a) => B.Binary (CompactFile a) where
    get = do
        magic <- B.get
        when (magic /= magicNumber) $
            fail "Data.Compact.Serialized: bad magic number"
        SomeTypeRep tyrep <- B.get
        case tyrep `eqTypeRep` typeRep @a of
          Just HRefl -> CompactFile <$> getSerializedCompact
          Nothing -> fail $
            "Data.Compact.Serialized: expected " ++ show (typeRep @a) ++
            " but got " ++ show tyrep
    put (CompactFile a) = B.put magicNumber >> B.put (typeRep @a) >> putSerializedCompact a

-- Serves as a very rudimentary integrity check.
-- Chosen at random by roll of a 2^64-sided die; it was a sight to behold.
magicNumber :: Word64
magicNumber = 0x7c155e7a53f094f2

putPtr :: Ptr a -> B.Put
putPtr = B.put @Word64 . fromIntegral . ptrToWordPtr

getPtr :: B.Get (Ptr a)
getPtr = wordPtrToPtr . fromIntegral <$> B.get @Word64

getList :: B.Get a -> B.Get [a]
getList getElem = do
    n <- B.get @Int
    replicateM n getElem

putList :: (a -> B.Put) -> [a] -> B.Put
putList putElem xs = do
    B.put @Int (length xs)
    mapM_ putElem xs

getSerializedCompact :: B.Get (SerializedCompact a)
getSerializedCompact = SerializedCompact <$> getList getBlock <*> getPtr
  where
    getBlock :: B.Get (Ptr a, Word)
    getBlock = (,) <$> getPtr <*> B.get

putSerializedCompact :: SerializedCompact a -> B.Put
putSerializedCompact (SerializedCompact a b) = putList putBlock a <> putPtr b
  where
    putBlock :: (Ptr a, Word) -> B.Put
    putBlock (ptr, len) = putPtr ptr <> B.put len

-- | Write a compact region to a file.  The resulting file can
-- be read back into memory using 'unsafeReadCompact'.
--
writeCompact :: Typeable a => FilePath -> Compact a -> IO ()
writeCompact fname compact =
    withFile fname WriteMode $ \h -> hPutCompact h compact

-- | Write a compact region to a 'Handle'.  The compact region
-- can be read out of the handle by using 'hUnsafeGetCompact'.
--
hPutCompact :: Typeable a => Handle -> Compact a -> IO ()
hPutCompact hdl compact =
    withSerializedCompact compact $ \scompact -> do
        let bs = B.encode $ CompactFile scompact
        -- By writing out the length of the metadata segment, we
        -- can read out a single word, read out the metadata segment,
        -- and then immediately start blasting further data from
        -- the handle into the memory region where the compact
        -- is going to go.  Otherwise, we have to indirect through
        -- a lazy bytestring which has a cost.
        hPutStorable hdl (fromIntegral (BSL.length bs) :: Int)
        BSL.hPut hdl bs
        let putBlock (ptr, len) = hPutBuf hdl ptr (fromIntegral len)
        mapM_ putBlock (serializedCompactBlockList scompact)

-- | Read out a compact region that was serialized to a file.
-- See 'hUnsafeGetCompact' for safety considerations when using this function.
--
unsafeReadCompact :: Typeable a => FilePath -> IO (Either String (Compact a))
unsafeReadCompact fname =
    withFile fname ReadMode $ \hdl -> hUnsafeGetCompact hdl

-- | Read out a compact region from a handle.
--
-- Compact regions written to handles this way are subject to some
-- restrictions:
--
--  * Our binary representation contains direct pointers to the info
--    tables of objects in the region.  This means that the info tables
--    of the receiving process must be laid out in exactly the same
--    way as from the original process; in practice, this means using
--    static linking, using the exact same binary and turning off ASLR.  This
--    API does NOT do any safety checking and will probably segfault if you
--    get it wrong.  DO NOT run this on untrusted input.
--
--  * You must read out the value at the correct type.  We will
--    check this for you and raise an error if the types do not match
--    (this is what the 'Typeable' constraint is for).
--
hUnsafeGetCompact
    :: forall a. Typeable a
    => Handle -> IO (Either String (Compact a))
hUnsafeGetCompact hdl = do
    l <- hGetStorable hdl
    mbs <- BSL.hGet hdl (l :: Int)
    case B.decodeOrFail @(CompactFile a) mbs of
      Left (_, _, err) -> return $ Left err
      Right (rest, _, CompactFile scompact)
        | not (BSL.null rest) -> return . Left
            $ "Had " ++ show (BSL.length rest) ++ " bytes of leftover metadata"
        | otherwise  -> do
          res <- importCompact scompact $ \ptr l ->
                    void $ hGetBuf hdl ptr (fromIntegral l)
          case res of
            Nothing -> return $ Left "Failed to import compact"
            Just compact -> return $ Right compact

hPutStorable :: forall a. Storable a => Handle -> a -> IO ()
hPutStorable h a =
    alloca $ \ptr -> do
        poke ptr a
        hPutBuf h ptr (sizeOf (undefined :: a))

hGetStorable :: forall a. Storable a => Handle -> IO a
hGetStorable h =
    alloca $ \ptr -> do
        hGetBuf h ptr (sizeOf (undefined :: a))
        peek ptr
