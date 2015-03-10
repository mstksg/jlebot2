{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Arrows #-}

module Module.Greet (greetBot) where

import Auto.Chatbot
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Collection
import Control.Monad.Random
import Data.List
import Data.Time
import Instances               ()
import Prelude hiding          ((.), id)
import Util

memory :: NominalDiffTime
memory = 30 * (24 * 60 * 60)

greetBot :: Monad m => StdGen -> ChatBotRoom m
greetBot = sealRandom greetBotRandom
  where
    greetBotRandom :: Monad m => ChatBotRoom (RandT StdGen m)
    greetBotRandom = proc (InMessage nick msg _ time) -> do
      greetBlip <- emitOn isGreeting -< msg
      perBlip (mux individualGreetBot) -< (nick, time) <$ greetBlip

    isGreeting :: Message -> Bool
    isGreeting str = "jlebot" `isInfixOf` str && hasGreeting str

    individualGreetBot :: Monad m
                       => Nick
                       -> Auto (RandT StdGen m) UTCTime [Message]
    individualGreetBot nick = proc time -> do
        history <- accum addHistory [] -< time
        let greetingClass = min 3 (length history `div` 3)
            greetingPool = personalizedGreetings !! greetingClass
        greeting <- arrM uniform -< greetingPool
        id -< [greeting]
      where
        personalizedGreetings = greetings nick

    addHistory :: [UTCTime] -> UTCTime -> [UTCTime]
    addHistory oldEvents newEvent = newEvent : takeWhile notOld oldEvents
      where
        notOld oldEvent = diffUTCTime newEvent oldEvent <= memory

hasGreeting :: String -> Bool
hasGreeting (words -> strwords) =
    any (`elem` strwords ) [ "hi", "hello"
                           , "hey", "sup"
                           , "hola", "oy"
                           , "yo", "wb"
                           , "o/", "\\o"
                           ]

greetings :: String -> [[String]]
greetings nick = (map . map) f glist
  where
    f str = let (x, y) = break (== '%') str
            in  x ++ nick ++ drop 1 y
    glist = [ [ "hi %"
              , "sup %"
              , "ahoy %"
              , "hi %!"
              , "yo %"
              , "hello %"
              , "greetings %"
              , "howdy do %"
              , "hey %"
              , "hey % :)"
              , "o/ %"
              ]
            , [ "hi again %"
              , "hey, how are you doing %?"
              , "whats new %"
              , "hey % pal"
              , "hey % buddy"
              , "%, sup bro"
              , "%, my man/woman"
              , "% how u!"
              , "% hows it hangin"
              , "%, lookin good"
              ]
            , [ "hey, %, my bestie"
              , "i love you %"
              , "how is my bff %"
              , "when are we going to hang, %?"
              , "% i miss you dearly"
              , "how would you touch me, %?"
              , "% you are the light of my world"
              , "%, missed you so much!"
              , "% you bring out the best in me"
              , "oh % <3"
              , "so nice to see your lovely face again %"
              ]
            , [ "please stop calling me %"
              , "stop bothering me %"
              , "i'm going to call the cops %"
              , "stop please %"
              , "% you used to be cool"
              , "okay % you're just getting creepy now"
              , "i'm pretty sure i got a restraining order on you %"
              , "i hate you %"
              , "words cannot describe how much you annoy me %"
              , "%, you're so annoying"
              , "%, we're over"
              , "%, we're through."
              , "% can we just move on please..."
              ]
            ]
