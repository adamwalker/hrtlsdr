# HRTLSDR

Haskell bindings to librtlsdr. 
Turns your Realtek RTL2832 based DVB dongle into a SDR receiver. 
For more information, see: http://sdr.osmocom.org/trac/wiki/rtl-sdr

# Installation

hrtlsdr is available on [Hackage](https://hackage.haskell.org/package/rtlsdr). Install with `cabal install rtlsdr`.

# Documentation

This library is a straightforward wrapper around the C library functions. See the C library documentation: https://github.com/steve-m/librtlsdr/blob/master/include/rtl-sdr.h

# Usage

```haskell
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
```
