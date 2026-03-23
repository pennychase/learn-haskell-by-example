{-# LANGUAGE NoFieldSelectors #-}

module Composition.Performance
	( Event (..),
	  start,
	  end
	)
where
	
import Util.Types

data Event
	= Tone {freq :: Hz, start :: Seconds, duration :: Seconds}
	| Silence {start :: Seconds, duration :: Seconds}
	
start :: Event -> Seconds
start (Tone _ s _) = s
start (Silence s _) =s

duration :: Event -> Seconds
duration (Tone _ _ d) = d
duration (Silence _ d) = d

end :: Event -> Seconds
end e = start e + duration e

isTone :: Event -> Bool
isTone Tone {} = True
isTone _ = False

isSilence :: Event -> Bool
isSilence Silence {} = True
isSilence _ = False