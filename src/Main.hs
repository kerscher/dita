-- | Track typing.

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.XInput
import           Data.Monoid
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           Data.Time.Clock
import           Formatting
import           Formatting.Time
import           System.Environment
import           System.IO

main :: IO ()
main =
  do (device:path:_) <- getArgs
     runResourceT
       (void (xinputSource (Device (read device))) $=
        CL.mapMaybeM
          (\(event,code) ->
             do d <- liftIO getCurrentTime
                let date = formatToString (year <> month <> dayOfMonth <> hour12 <> minute <> second <> pico)
                                          d
                return (do guard (event == Press)
                           key <- lookup code mapping
                           return (T.encodeUtf8 (T.pack (date ++ " " ++ show key ++ "\n"))))) $$
        (CB.sinkIOHandle (openFile path AppendMode)))

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
  | Plain
  | Unknown
  deriving (Show,Ord,Eq,Read)

-- | Mapping from keycodes to something for humans to read.
mapping :: [(KeyCode, Key)]
mapping =
  [(50,ShiftL)
  ,(62,ShiftR)
  ,(37,CtrlL)
  ,(105,CtrlR)
  ,(64,AltL)
  ,(108,AltR)
  ,(133,SuperL)
  ,(134,SuperR)
  ,(24,Plain)
  ,(25,Plain)
  ,(26,Plain)
  ,(27,Plain)
  ,(28,Plain)
  ,(29,Plain)
  ,(30,Plain)
  ,(31,Plain)
  ,(32,Plain)
  ,(33,Plain)
  ,(34,Plain)
  ,(35,Plain)
  ,(51,Plain)
  ,(38,Plain)
  ,(39,Plain)
  ,(40,Plain)
  ,(41,Plain)
  ,(42,Plain)
  ,(43,Plain)
  ,(44,Plain)
  ,(45,Plain)
  ,(46,Plain)
  ,(47,Plain)
  ,(48,Plain)
  ,(36,RET)
  ,(52,Plain)
  ,(53,Plain)
  ,(54,Plain)
  ,(55,Plain)
  ,(56,Plain)
  ,(57,Plain)
  ,(58,Plain)
  ,(59,Plain)
  ,(60,Plain)
  ,(61,Plain)
  ,(66,CapsLock)
  ,(49,Plain)
  ,(10,Plain)
  ,(11,Plain)
  ,(12,Plain)
  ,(13,Plain)
  ,(14,Plain)
  ,(15,Plain)
  ,(16,Plain)
  ,(17,Plain)
  ,(18,Plain)
  ,(19,Plain)
  ,(20,Plain)
  ,(21,Plain)
  ,(65,SPC)
  ,(67,F 1)
  ,(68,F 2)
  ,(69,F 3)
  ,(70,F 4)
  ,(71,F 5)
  ,(72,F 6)
  ,(73,F 7)
  ,(74,F 8)
  ,(75,F 9)
  ,(76,F 10)
  ,(95,F 11)
  ,(96,F 12)
  ,(9,Escape)
  ,(22,Backspace)
  ,(118,Insert)
  ,(119,Delete)
  ,(110,Home)
  ,(112,Prior)
  ,(117,Next)
  ,(115,End)
  ,(111,UpArr)
  ,(116,DownArr)
  ,(113,LeftArr)
  ,(114,RightArr)
  ,(135,Menu)
  ,(107,PrintScreen)
  ,(23,TAB)]
