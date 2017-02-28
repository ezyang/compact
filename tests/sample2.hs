{-# LANGUAGE TypeApplications #-}
import Data.Compact
import Data.Compact.Serialize
main = do
    orig_c <- compact ("I want to serialize this", True)
    writeCompact @(String, Bool) "somefile" orig_c
    res <- unsafeReadCompact @(String, Bool) "somefile"
    case res of
        Left err -> fail err
        Right c -> print (getCompact c)
