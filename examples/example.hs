import Data.List
import Foreign.Marshal.Array
import RTLSDR

bufSize    = 16384
sampleRate = 1280000
frequency  = 90200000

main = do
    dev <- open 0
    case dev of
        Nothing -> putStrLn "Error opening device"
        Just dev -> do
            setSampleRate    dev sampleRate
            setCenterFreq    dev frequency
            setTunerGainMode dev False
            resetBuffer      dev
            allocaArray bufSize $ \ptr -> do
                res <- readSync dev ptr bufSize
                case res of 
                    False -> putStrLn "Error reading from device"
                    True  -> do
                        res <- peekArray bufSize ptr
                        putStrLn $ intercalate " " $ map show res
