{-# LANGUAGE TupleSections #-}

module Sound.Novation.Launchpad where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as Char8
import           Data.Char
import           Data.Foldable
import           Data.Int
import           Data.List
import           Foreign.C.Types
import           Sound.PortMidi

data Launchpad
  = Launchpad PMStream PMStream
  deriving (Eq, Ord, Show)

data LaunchpadError
  = LaunchpadPortMidiError PMError
  | LaunchpadNotFoundError
  | LaunchpadUnknownEvent PMMsg
  deriving (Eq, Show)

data NoteState
  = NotePressed
  | NoteReleased
  deriving (Eq, Ord, Show)

data NoteEvent
  = NoteEvent Int64 NoteState
  deriving (Eq, Ord, Show)

data Coord
  = Coord Int64 Int64
  deriving (Eq, Ord, Show)

data CoordEvent
  = CoordEvent Coord NoteState
  deriving (Eq, Ord, Show)

data Brightness
  = LowBrightness
  | MediumBrightness
  | FullBrightness
  deriving (Eq, Ord, Show)

data Color
  = Color (Maybe Brightness) (Maybe Brightness)
  deriving (Eq, Ord, Show)

red :: Color
red =
  Color (Just FullBrightness) Nothing

orange :: Color
orange =
  Color (Just FullBrightness) (Just FullBrightness)

green :: Color
green =
  Color Nothing (Just FullBrightness)

noColor :: Color
noColor =
  Color Nothing Nothing

swapEither :: Either a b -> Either b a
swapEither =
  either Right Left

findLaunchpad :: ExceptT LaunchpadError IO Launchpad
findLaunchpad = do
  c <- lift countDevices
  ns <- lift $ traverse (\n -> (n,) <$> getDeviceInfo n) [0..c - 1]
  let ns' = filter (\(_, n) -> "Launchpad" `isPrefixOf` name n) ns
  i <- find' (input . snd) ns'
  o <- find' (output . snd) ns'
  connectLaunchpad i o
  where
    find' f =
      maybe (throwE LaunchpadNotFoundError) (pure . fst) . find f

connectLaunchpad :: DeviceID -> DeviceID -> ExceptT LaunchpadError IO Launchpad
connectLaunchpad i o = do
  i' <- portMidi $ openInput i
  o' <- portMidi $ openOutput o 0
  pure $ Launchpad i' o'
  where
    swap =
      either Right (Left . LaunchpadPortMidiError)
    portMidi =
      ExceptT . fmap swap

disconnectLaunchpad :: Launchpad -> IO PMError
disconnectLaunchpad (Launchpad i o) = do
  close i
  close o

resetLaunchpad :: Launchpad -> IO PMError
resetLaunchpad (Launchpad _ o) =
  sendSysEx (BS.pack [176, 0, 0]) o

turnOnAllLeds :: Brightness -> Launchpad -> IO PMError
turnOnAllLeds b (Launchpad _ o) =
  sendSysEx (BS.pack [176, 0, 125 + f b]) o
  where
    f LowBrightness =
      0
    f MediumBrightness =
      1
    f FullBrightness =
      2

coordOn :: Coord -> Color -> Launchpad -> IO PMError
coordOn (Coord x y) =
  noteOn (y * 16 + x)

regionOn :: Coord -> Coord -> Color -> Launchpad -> IO [PMError]
regionOn (Coord x y) (Coord x' y') c l =
  traverse (\o -> coordOn o c l) [Coord x'' y'' | x'' <- [x..x'], y'' <- [y..y']]

readCoords :: Launchpad -> ExceptT LaunchpadError IO [CoordEvent]
readCoords =
  (fmap . fmap) f . readNotes
  where
    f (NoteEvent a b) =
      CoordEvent (coord a) b
    coord a =
      Coord (a `mod` 16) (a `div` 16)

readNotes :: Launchpad -> ExceptT LaunchpadError IO [NoteEvent]
readNotes (Launchpad i _) = do
  events <- ExceptT $ swapEither . fmap LaunchpadPortMidiError <$> readEvents i
  ExceptT . pure $ traverse (readNote . decodeMsg . message) events
  where
    readNote (PMMsg 144 a b) =
      Right . NoteEvent (fromIntegral a) $ noteState b
    readNote m =
      Left $ LaunchpadUnknownEvent m
    noteState b =
      if b > 0 then NotePressed else NoteReleased

noteOn :: Int64 -> Color -> Launchpad -> IO PMError
noteOn key c (Launchpad _ o) =
  writeShort o $ PMEvent (encodeMsg $ PMMsg 144 (fromIntegral key) (color c)) 0

liveControl :: Int64 -> Color -> Launchpad -> IO PMError
liveControl key c (Launchpad _ o) =
  writeShort o $ PMEvent (encodeMsg $ PMMsg 176 (fromIntegral key) (color c)) 0

color :: Color -> CLong
color (Color r g) =
  n r .|. (n g * 16)
  where
    n Nothing =
      0
    n (Just LowBrightness) =
      1
    n (Just MediumBrightness) =
      2
    n (Just FullBrightness) =
      3

sendSysEx :: BS.ByteString -> PMStream -> IO PMError
sendSysEx m n =
  writeSysEx n 0 ([chr 0xf0] ++ Char8.unpack m ++ [chr 0xf7])

overTime :: Int -> (Int -> IO a) -> Launchpad -> IO ()
overTime delay f l =
  traverse_ (\i -> resetLaunchpad l *> f i *> threadDelay delay) [0..]

plot :: Int -> (Int64 -> Int64) -> (Coord -> IO a) -> Launchpad -> IO ()
plot delay f g l =
  overTime delay (for_ [0..7] . h) l
  where
    h x n =
      g . Coord n . f $ fromIntegral x + n

wave :: Int -> Color -> Launchpad -> IO ()
wave delay color l =
  plot delay f (\c -> coordOn c color l) l
  where
    f n =
      floor $ sin (fromIntegral n / 3) * 4 + 4

drawByteString :: BS.ByteString -> Color -> Launchpad -> IO ()
drawByteString bs color l =
  for_ (zip [0..] $ BS.unpack bs) $ \(y, b) ->
    for_ [0..7] $ \x ->
      when (testBit b $ fromIntegral x) . void $
        coordOn (Coord x y) color l

scrollBetween :: Int -> BS.ByteString -> BS.ByteString -> Color -> Launchpad -> IO ()
scrollBetween delay bs bs' color l =
  for_ [0..7] $ \x -> do
    resetLaunchpad l
    drawByteString (BS.map (`shiftR` x) bs) color l
    drawByteString (BS.map (`shiftL` (8 - x)) bs') color l
    threadDelay delay

scrollAll :: Int -> [BS.ByteString] -> Color -> Launchpad -> IO ()
scrollAll delay (a:b:bs) color l = do
  scrollBetween delay a b color l
  scrollAll delay (b:bs) color l
scrollAll _ bs color l =
  for_ bs $ \b -> resetLaunchpad l *> drawByteString b color l
