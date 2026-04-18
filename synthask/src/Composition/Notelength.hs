module Composition.Notelength
	( Notelength,
	  whole,
	  half,
	  quarter,
	  eighth,
	  sixteenth,
	  wn,
	  hn,
	  qn,
	  en,
	  sn,
	  dots,
	  dotted,
	  doubleDotted,
	  tripleDotted,
	  tuplet,
	  triplet,
	  quintuplet,
	  BPM,
	  TempoInfo (..),
	  timePerBeat,
	  timePerNoteLength
	)
where

import Data.Ratio (Ratio, (%), denominator, numerator)
import Numeric.Natural (Natural)
import Util.Types (Seconds)

-- Note lengths

type Notelength = Ratio Natural

whole :: Notelength
whole = 1 % 1

half :: Notelength
half = 1 % 2

quarter :: Notelength
quarter = 1 % 4

eighth :: Notelength
eighth = 1 % 8

sixteenth :: Notelength
sixteenth = 1 % 16

wn :: Notelength
wn = whole

hn :: Notelength
hn = half

qn :: Notelength
qn = quarter

en :: Notelength
en = eighth

sn :: Notelength
sn = sixteenth

-- Note duration modifiers

dots :: Natural -> Notelength -> Notelength
dots n x = x + x * (1 % 2 ^ n)

dotted :: Notelength -> Notelength
dotted = dots 1

doubleDotted :: Notelength -> Notelength
doubleDotted = dots 2

tripleDotted :: Notelength -> Notelength
tripleDotted = dots 3

tuplet :: Natural -> Notelength -> Notelength
tuplet n x = x * (2 % n)

triplet :: Notelength -> Notelength
triplet = tuplet 3

quintuplet :: Notelength -> Notelength
quintuplet = tuplet 5 

-- Tempo

type BPM = Double

data TempoInfo = TempoInfo
	{ beatsPerMinute :: BPM,
	  beatsPerWholeNote :: Double
	}

timePerBeat :: BPM -> Seconds
timePerBeat bpm = 60.0 / bpm

timePerNoteLength :: TempoInfo -> Notelength -> Seconds
timePerNoteLength
	(TempoInfo beatsPerMinute beatsPerWholeNote)
	noteLength =
		let beatsForNoteLength = beatsPerWholeNote * toDouble noteLength
		in  beatsForNoteLength * timePerBeat beatsPerMinute
		where 
			toDouble :: Ratio Natural -> Double
			toDouble r = (fromInteger . toInteger $ numerator r) / (fromInteger . toInteger $ denominator r)
