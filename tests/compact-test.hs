import Data.Compact
import Data.Compact.Serialize
import Control.Exception
import System.IO
import System.Directory
import System.Mem
import Control.Monad

main = do
    tmp <- getTemporaryDirectory
    let val = ("hello", 1, 42, 42, Just 42) :: (String, Int, Int, Integer, Maybe Int)
    c <- compact val
    (fp, h) <- openTempFile tmp "compact.bin"
    hPutCompact h c
    hClose h
    performMajorGC
    r <- unsafeReadCompact fp
    c' <- case r of
            Left err -> fail err
            Right x -> return x
    removeFile fp
    print (getCompact c' :: (String, Int, Int, Integer, Maybe Int))
    when (val /= getCompact c') $ fail "did not match"
    putStrLn "OK"
