{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

-- | Track typing.
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Conduit.XInput
import qualified Data.Text as T
import Data.Text.Encoding as T
import Data.Time
import Formatting
import Formatting.Time
import Prelude hiding (last)
import System.Environment
import System.IO

-- | Main dispatcher.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("report":path:_) -> report path
    (device:path:_) -> startLogging device path
    _ -> error "Bad arguments"

-- | Start logging keys.
startLogging :: String -> FilePath -> IO ()
startLogging device path =
  runResourceT
    (void (xinputSource (Device (read device))) $=
     CL.mapMaybeM
       (\(event, code) -> do
          d <- liftIO getCurrentTime
          let date = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S+%q" d
          return
            (do guard (event == Press)
                key <- lookup code mapping
                return (T.encodeUtf8 (T.pack (date ++ " " ++ show key ++ "\n"))))) $$
     (CB.sinkIOHandle (openFile path AppendMode)))

-- | Parse a day date.
parseDay :: ByteString -> Maybe Day
parseDay (S8.unpack -> (y0:y1:y2:y3:m1:m2:d1:d2:_mm1:_mm2:_s1:_s2:_qs)) =
  parseTimeM
    True
    defaultTimeLocale
    "%Y-%m-%d"
    [y0, y1, y2, y3, '-', m1, m2, '-', d1, d2]
parseDay _ = Nothing

-- | Parse a day date.
parseDay' :: String -> Maybe Day
parseDay' (y0:y1:y2:y3:m1:m2:d1:d2:_mm1:_mm2:_s1:_s2:_qs) =
  parseTimeM
    True
    defaultTimeLocale
    "%Y-%m-%d"
    [y0, y1, y2, y3, '-', m1, m2, '-', d1, d2]
parseDay' _ = Nothing

-- | Make a report page.
report :: FilePath -> IO ()
report path = do
  dayCounts <-
    runResourceT
      (CB.sourceFile path $= CB.lines $= CL.mapMaybe parseDay $=
       countGroupBy (==) $$
       CL.consume)
  forM_
    dayCounts
    (\(day', count) -> do fprint (dateDash % ": " % commas % "\n") day' count)

-- | Count the elements in each group by the given predicate.
countGroupBy :: Monad m => (i -> i -> Bool) -> Conduit i m (i, Int)
countGroupBy cmp = go Nothing
  where
    go mlast = do
      mi <- await
      case mi of
        Nothing ->
          case mlast of
            Nothing -> return ()
            Just last -> yield last
        Just this ->
          case mlast of
            Nothing -> go (Just (this, 1))
            Just (last, !count) ->
              if cmp this last
                then go (Just (last, count + 1))
                else do
                  yield (last, count)
                  go (Just (this, 1))

-- | Key type.
data Key
  = CtrlL
  | CtrlR
  | AltL
  | AltR
  | ShiftL
  | ShiftR
  | RET
  | SuperL
  | SuperR
  | CapsLock
  | SPC
  | F Int
  | Escape
  | Backspace
  | Insert
  | Delete
  | Home
  | Prior
  | Next
  | End
  | UpArr
  | DownArr
  | LeftArr
  | RightArr
  | PrintScreen
  | Menu
  | TAB
  | Plain String
  | Unknown
  deriving (Show, Ord, Eq, Read)

-- | Mapping from keycodes to something for humans to read.
mapping :: [(KeyCode, Key)]
mapping =
  [ (50, ShiftL)
  , (62, ShiftR)
  , (37, CtrlL)
  , (105, CtrlR)
  , (64, AltL)
  , (108, AltR)
  , (133, SuperL)
  , (134, SuperR)
  , (24, Plain "'")
  , (25, Plain ",")
  , (26, Plain ".")
  , (27, Plain "p")
  , (28, Plain "y")
  , (29, Plain "f")
  , (30, Plain "g")
  , (31, Plain "c")
  , (32, Plain "r")
  , (33, Plain "l")
  , (34, Plain "/")
  , (35, Plain "=")
  , (51, Plain "\\")
  , (38, Plain "a")
  , (39, Plain "o")
  , (40, Plain "e")
  , (41, Plain "u")
  , (42, Plain "i")
  , (43, Plain "d")
  , (44, Plain "h")
  , (45, Plain "t")
  , (46, Plain "n")
  , (47, Plain "s")
  , (48, Plain "-")
  , (36, RET)
  , (52, Plain ";")
  , (53, Plain "q")
  , (54, Plain "j")
  , (55, Plain "k")
  , (56, Plain "x")
  , (57, Plain "b")
  , (58, Plain "m")
  , (59, Plain "w")
  , (60, Plain "v")
  , (61, Plain "z")
  , (66, CapsLock)
  , (49, Plain "`")
  , (10, Plain "1")
  , (11, Plain "2")
  , (12, Plain "3")
  , (13, Plain "4")
  , (14, Plain "5")
  , (15, Plain "6")
  , (16, Plain "7")
  , (17, Plain "8")
  , (18, Plain "9")
  , (19, Plain "0")
  , (20, Plain "[")
  , (21, Plain "]")
  , (65, SPC)
  , (67, F 1)
  , (68, F 2)
  , (69, F 3)
  , (70, F 4)
  , (71, F 5)
  , (72, F 6)
  , (73, F 7)
  , (74, F 8)
  , (75, F 9)
  , (76, F 10)
  , (95, F 11)
  , (96, F 12)
  , (9, Escape)
  , (22, Backspace)
  , (118, Insert)
  , (119, Delete)
  , (110, Home)
  , (112, Prior)
  , (117, Next)
  , (115, End)
  , (111, UpArr)
  , (116, DownArr)
  , (113, LeftArr)
  , (114, RightArr)
  , (135, Menu)
  , (107, PrintScreen)
  , (23, TAB)
  ]
