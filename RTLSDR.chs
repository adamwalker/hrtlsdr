{-# LANGUAGE EmptyDataDecls #-}
module RTLSDR (
    getDeviceCount,
    getDeviceName,
    getDeviceUSBString,
    GIBSError(..),
    getIndexBySerial,
    RTLSDR,
    open,
    close,
    setXtalFreq,
    getXtalFreq,
    getUSBStrings,
    EEPROMError(..),
    writeEEPROM,
    readEEPROM,
    setCenterFreq,
    getCenterFreq,
    setFreqCorrection,
    getFreqCorrection,
    Tuner(..),
    getTunerType,
    getTunerGains,
    setTunerGain,
    getTunerGain,
    setTunerIFGain,
    setTunerGainMode,
    setSampleRate,
    getSampleRate,
    setTestMode,
    setAGCMode,
    DirectSamplingMode(..),
    setDirectSampling,
    getDirectSampling,
    setOffsetTuning,
    getOffsetTuning,
    resetBuffer,
    readSync,
    ReadCallback,
    readAsync,
    cancelAsync
    ) where

#include <rtl-sdr.h>

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Data.Word
import Data.Int
import Control.Monad
import Data.Array.MArray
import Data.Array.Storable

foreign import ccall unsafe "rtlsdr_get_device_count"
    c_getDeviceCount :: IO CUInt

getDeviceCount :: IO Word32
getDeviceCount = liftM fromIntegral c_getDeviceCount

foreign import ccall unsafe "rtlsdr_get_device_name"
    c_rtlsdrGetDeviceName :: CUInt -> IO CString

getDeviceName :: Word32 -> IO String
getDeviceName index = c_rtlsdrGetDeviceName (fromIntegral index) >>= peekCString 

foreign import ccall unsafe "rtlsdr_get_device_usb_strings"
    c_getDeviceUSBStrings :: CUInt -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt

getDeviceUSBString :: Word32 -> IO (Maybe (String, String, String))
getDeviceUSBString index = 
    allocaArray 256 $ \mp -> 
    allocaArray 256 $ \pp -> 
    allocaArray 256 $ \sp -> do
        res <- c_getDeviceUSBStrings (fromIntegral index) mp pp sp 
        case res of 
            0 -> do
                m <- peekCString mp
                p <- peekCString pp
                s <- peekCString sp
                return $ Just (m, p, s)
            _ -> return Nothing
        
foreign import ccall unsafe "rtlsdr_get_index_by_serial"
    c_getIndexBySerial :: CString -> IO CInt

data GIBSError = NameNull
               | NoDevices
               | NoMatching
               deriving (Show)

toGIBSError :: Int -> GIBSError
toGIBSError (-1) = NameNull
toGIBSError (-2) = NoDevices
toGIBSError (-3) = NoMatching

getIndexBySerial :: String -> IO (Either GIBSError Int)
getIndexBySerial serial = liftM (func . fromIntegral) $ withCString serial c_getIndexBySerial 
    where func res 
            | res < 0   = Left (toGIBSError res)
            | otherwise = Right res

data CRTLSDR
data RTLSDR = RTLSDR (Ptr CRTLSDR)

foreign import ccall unsafe "rtlsdr_open"
    c_open :: Ptr (Ptr CRTLSDR) -> CInt -> IO CInt

open :: Word32 -> IO (Maybe RTLSDR)
open index = alloca $ \ptr -> do
    res <- c_open ptr (fromIntegral index)
    case res < 0 of
        True  -> return Nothing
        False -> do
            res <- peek ptr
            return $ Just $ RTLSDR res

foreign import ccall unsafe "rtlsdr_close"
    c_close :: Ptr CRTLSDR -> IO CInt

close :: RTLSDR -> IO ()
close (RTLSDR ptr) = void $ c_close ptr

foreign import ccall unsafe "rtlsdr_set_xtal_freq"
    c_setXtalFreq :: Ptr CRTLSDR -> CUInt -> CUInt -> IO CInt

setXtalFreq :: RTLSDR -> Word32 -> Word32 -> IO Bool
setXtalFreq (RTLSDR ptr) rtlFreq tunerFreq = liftM (==0) $ c_setXtalFreq ptr (fromIntegral rtlFreq) (fromIntegral tunerFreq)

foreign import ccall unsafe "rtlsdr_get_xtal_freq"
    c_getXtalFreq :: Ptr CRTLSDR -> Ptr CUInt -> Ptr CUInt -> IO CInt

getXtalFreq :: RTLSDR -> IO (Maybe (Word32, Word32))
getXtalFreq (RTLSDR ptr) = 
    alloca $ \rp -> 
    alloca $ \tp -> do
        res <- c_getXtalFreq ptr rp tp
        case res of
            0 -> return Nothing
            _ -> do
                r <- peek rp
                t <- peek tp
                return $ Just (fromIntegral r, fromIntegral t)

foreign import ccall unsafe "rtlsdr_get_usb_strings"
    c_getUSBStrings :: Ptr CRTLSDR -> Ptr CChar -> Ptr CChar -> Ptr CChar -> IO CInt

getUSBStrings :: RTLSDR -> IO (Maybe (String, String, String))
getUSBStrings (RTLSDR ptr) = 
    allocaArray 256 $ \mp -> 
    allocaArray 256 $ \pp -> 
    allocaArray 256 $ \sp -> do
        res <- c_getUSBStrings ptr mp pp sp 
        case res of 
            0 -> do
                m <- peekCString mp
                p <- peekCString pp
                s <- peekCString sp
                return $ Just (m, p, s)
            _ -> return Nothing

data EEPROMError = InvalidHandle
                 | SizeExceeded
                 | NoEEPROM
                 deriving(Show)

toEEPROMError :: Int -> EEPROMError
toEEPROMError (-1) = InvalidHandle
toEEPROMError (-2) = SizeExceeded
toEEPROMError (-3) = NoEEPROM

foreign import ccall unsafe "rtlsdr_write_eeprom"
    c_writeEEPROM :: Ptr CRTLSDR -> Ptr CUChar -> CUChar -> CUShort -> IO CInt

writeEEPROM :: RTLSDR -> [Word8] -> Int -> IO (Maybe EEPROMError)
writeEEPROM (RTLSDR ptr) dataa offset = 
    liftM (func . fromIntegral) $ withArrayLen (map fromIntegral dataa) $ \size ptrd -> c_writeEEPROM ptr ptrd (fromIntegral offset) (fromIntegral size)
    where func x
            | x < 0       = Just $ toEEPROMError x
            | otherwise = Nothing

foreign import ccall unsafe "rtlsdr_read_eeprom"
    c_readEEPROM :: Ptr CRTLSDR -> Ptr CUChar -> CUChar -> CUShort -> IO CInt

readEEPROM :: RTLSDR -> Int -> Int -> IO (Either EEPROMError [Word8])
readEEPROM (RTLSDR ptr) offset len = allocaArray len $ \ptrd -> do
    res <- c_readEEPROM ptr ptrd (fromIntegral offset) (fromIntegral len)
    case res < 0 of
        True  -> return $ Left $ toEEPROMError $ fromIntegral res
        False -> do
            res <- peekArray len ptrd
            return $ Right $ map fromIntegral res

foreign import ccall unsafe "rtlsdr_set_center_freq"
    c_setCenterFreq :: Ptr CRTLSDR -> CUInt -> IO CInt

setCenterFreq :: RTLSDR -> Word32 -> IO Bool
setCenterFreq (RTLSDR ptr) freq = liftM (==0) $ c_setCenterFreq ptr (fromIntegral freq)

foreign import ccall unsafe "rtlsdr_get_center_freq"
    c_getCenterFreq :: Ptr CRTLSDR -> IO CUInt

getCenterFreq :: RTLSDR -> IO (Maybe Word32)
getCenterFreq (RTLSDR ptr) = liftM func $ c_getCenterFreq ptr
    where func 0 = Nothing
          func x = Just $ fromIntegral x

foreign import ccall unsafe "rtlsdr_set_freq_correction"
    c_setFreqCorrection :: Ptr CRTLSDR -> CInt -> IO CInt

setFreqCorrection :: RTLSDR -> Int32 -> IO Bool
setFreqCorrection (RTLSDR ptr) ppm = liftM (==0) $ c_setFreqCorrection ptr (fromIntegral ppm)

foreign import ccall unsafe "rtlsdr_get_freq_correction"
    c_getFreqCorrection :: Ptr CRTLSDR -> IO CInt

getFreqCorrection :: RTLSDR -> IO Int32 
getFreqCorrection (RTLSDR ptr) = liftM fromIntegral $ c_getFreqCorrection ptr

{#enum rtlsdr_tuner as Tuner {underscoreToCase} deriving (Show, Eq) #}

foreign import ccall unsafe "rtlsdr_get_tuner_type"
    c_getTunerType :: Ptr CRTLSDR -> IO CInt

getTunerType :: RTLSDR -> IO Tuner
getTunerType (RTLSDR ptr) = liftM (toEnum . fromIntegral) $ c_getTunerType ptr

foreign import ccall unsafe "rtlsdr_get_tuner_gains"
    c_getTunerGains :: Ptr CRTLSDR -> Ptr CInt -> IO CInt

getTunerGains :: RTLSDR -> IO (Maybe [Int])
getTunerGains (RTLSDR ptr) = do
    num <- c_getTunerGains ptr nullPtr 
    case num < 0 of
        True -> return Nothing
        False -> allocaArray (fromIntegral num) $ \ptrg -> do
            c_getTunerGains ptr ptrg
            res <- peekArray (fromIntegral num) ptrg
            return $ Just $ map fromIntegral res

foreign import ccall unsafe "rtlsdr_set_tuner_gain"
    c_setTunerGain :: Ptr CRTLSDR -> CInt -> IO CInt

setTunerGain :: RTLSDR -> Int32 -> IO Bool
setTunerGain (RTLSDR ptr) gain = liftM (==0) $ c_setTunerGain ptr (fromIntegral gain)

foreign import ccall unsafe "rtlsdr_get_tuner_gain"
    c_getTunerGain :: Ptr CRTLSDR -> IO CInt

getTunerGain :: RTLSDR -> IO (Maybe Int32)
getTunerGain (RTLSDR ptr) = liftM func $ c_getTunerGain ptr
    where func 0 = Nothing
          func x = Just $ fromIntegral x

foreign import ccall unsafe "rtlsdr_set_tuner_if_gain"
    c_setTunerIFGain :: Ptr CRTLSDR -> CInt -> CInt -> IO CInt

setTunerIFGain :: RTLSDR -> Int -> Int -> IO Bool
setTunerIFGain (RTLSDR ptr) stage gain = liftM (==0) $ c_setTunerIFGain ptr (fromIntegral stage) (fromIntegral gain)

foreign import ccall unsafe "rtlsdr_set_tuner_gain_mode"
    c_setTunerGainMode :: Ptr CRTLSDR -> CInt -> IO CInt

setTunerGainMode :: RTLSDR -> Bool -> IO Bool
setTunerGainMode (RTLSDR ptr) mode = liftM (==0) $ c_setTunerGainMode ptr (b2int mode)
    where b2int False = 0
          b2int True  = 1

foreign import ccall unsafe "rtlsdr_set_sample_rate"
    c_setSampleRate :: Ptr CRTLSDR -> CUInt -> IO CInt

setSampleRate :: RTLSDR -> Word32 -> IO Bool
setSampleRate (RTLSDR ptr) rate = liftM (==0) $ c_setSampleRate ptr (fromIntegral rate)

foreign import ccall unsafe "rtlsdr_get_sample_rate"
    c_getSampleRate :: Ptr CRTLSDR -> IO CUInt

getSampleRate :: RTLSDR -> IO (Maybe Word32)
getSampleRate (RTLSDR ptr) = liftM func $ c_getSampleRate ptr
    where func 0 = Nothing
          func x = Just $ fromIntegral x

foreign import ccall unsafe "rtlsdr_set_testmode"
    c_setTestmode :: Ptr CRTLSDR -> CInt -> IO CInt

setTestMode :: RTLSDR -> Bool -> IO Bool
setTestMode (RTLSDR ptr) on = liftM (==0) $ c_setTestmode ptr (b2int on)
    where b2int False = 0
          b2int True  = 1

foreign import ccall unsafe "rtlsdr_set_agc_mode"
    c_setAGCMode :: Ptr CRTLSDR -> CInt -> IO CInt

setAGCMode :: RTLSDR -> Bool -> IO Bool
setAGCMode (RTLSDR ptr) on = liftM (==0) $ c_setAGCMode ptr (b2int on)
    where b2int False = 0
          b2int True  = 1

data DirectSamplingMode = DSDisabled
                        | DSI
                        | DSQ
                        deriving (Enum, Show, Eq)

foreign import ccall unsafe "rtlsdr_set_direct_sampling"
    c_setDirectSampling :: Ptr CRTLSDR -> CInt -> IO CInt

setDirectSampling :: RTLSDR -> DirectSamplingMode -> IO Bool
setDirectSampling (RTLSDR ptr) mode = liftM (==0) $ c_setDirectSampling ptr (fromIntegral $ fromEnum mode)

foreign import ccall unsafe "rtlsdr_get_direct_sampling"
    c_getDirectSampling :: Ptr CRTLSDR -> IO CInt

getDirectSampling :: RTLSDR -> IO (Maybe DirectSamplingMode)
getDirectSampling (RTLSDR ptr) = do
    res <- c_getDirectSampling ptr
    case res < 0 of
        True  -> return Nothing
        False -> return $ Just $ toEnum $ fromIntegral res

foreign import ccall unsafe "rtlsdr_set_offset_tuning"
    c_setOffsetTuning :: Ptr CRTLSDR -> CInt -> IO CInt

setOffsetTuning :: RTLSDR -> Bool -> IO Bool
setOffsetTuning (RTLSDR ptr) on = liftM (==0) $ c_setOffsetTuning ptr (b2int on)
    where b2int False = 0
          b2int True  = 1

foreign import ccall unsafe "rtlsdr_get_offset_tuning"
    c_getOffsetTuning :: Ptr CRTLSDR -> IO CInt

getOffsetTuning :: RTLSDR -> IO (Maybe Bool)
getOffsetTuning (RTLSDR ptr) = do
    res <- c_getOffsetTuning ptr
    case res < 0 of
        True -> return Nothing
        False -> case res of
            0 -> return $ Just False
            1 -> return $ Just True

foreign import ccall unsafe "rtlsdr_reset_buffer"
    c_resetBuffer :: Ptr CRTLSDR -> IO CInt

resetBuffer :: RTLSDR -> IO Int
resetBuffer (RTLSDR ptr) = liftM fromIntegral $ c_resetBuffer ptr

foreign import ccall unsafe "rtlsdr_read_sync"
    c_readSync :: Ptr CRTLSDR -> Ptr CUChar -> CInt -> Ptr CInt -> IO CInt

readSync :: RTLSDR -> Ptr CUChar -> Int -> IO Bool
readSync (RTLSDR ptr) aptr len = do
    res <- alloca $ c_readSync ptr aptr (fromIntegral len) 
    return $ res >= 0 

type ReadCallback = Ptr CUChar -> Word32 -> Ptr CInt -> IO ()

foreign import ccall "wrapper"
    wrap :: ReadCallback -> IO (FunPtr ReadCallback)

foreign import ccall safe "rtlsdr_read_async"
    c_readAsync :: Ptr CRTLSDR -> FunPtr ReadCallback -> Ptr () -> CUInt -> CUInt -> IO CInt

readAsync :: RTLSDR -> Word32 -> Word32 -> (Ptr CUChar -> Int -> IO ()) -> IO Bool
readAsync (RTLSDR ptr) bufNum bufLen callback = do
    cb <- wrap f
    res <- c_readAsync ptr cb nullPtr (fromIntegral bufNum) (fromIntegral bufLen)
    return $ g res
    where
    f buf len ctx = callback buf (fromIntegral len)
    g 0 = True
    g _ = False

foreign import ccall unsafe "rtlsdr_cancel_async"
    c_cancelAsync :: Ptr CRTLSDR -> IO CInt

cancelAsync :: RTLSDR -> IO Bool
cancelAsync (RTLSDR ptr) = liftM (==0) $ c_cancelAsync ptr

