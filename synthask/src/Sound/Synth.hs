module Sound.Synth
    ( Sound.Synth.sin,
      sqw,
      saw,
      tri,
      silence,
      tone,
      ADSR (..),
      adsr,
      Oscillator (..),
      osc,
      piano,
      ocarina,
      violin,
      pluck,
      bass,
      modulate,
      tremolo
    ) 
where

import Prelude hiding (sin)
import qualified Prelude

import Composition.Performance
import Util.Types

-- Waveforms

type Wave = Double -> Sample

sin :: Wave
sin t = Prelude.sin $ 2 * pi * t

sqw :: Wave
sqw t
    | t <= 0.5 = -1
    | otherwise = 1

saw :: Wave
saw t
    | t < 0 = -1
    | t > 1 = 1
    | otherwise = (2 * t) -1

tri :: Wave
tri t
    | t < 0 = -1
    | t > 1 = -1
    | t < 0.5 = 4 * t - 1
    | otherwise = -4 * t + 3

pulse :: Double -> Wave
pulse p t
    | t <= p = -1
    | otherwise = 1

-- Produce signals from waveforms

silence :: Seconds -> Signal
silence t = replicate (samplesPerSecond t) 0

tone :: Wave -> Hz -> Seconds -> Signal
tone wave freq t = map wave periodValues
    where
        numSamples = samplesPerPeriod freq
        periodValues =
            map (\x -> fromIntegral (x `mod` numSamples) / fromIntegral numSamples)
                [0 .. samplesPerSecond t]

tone' :: Wave -> Hz -> Seconds -> Signal
tone' wave freq t = take (samplesPerSecond t) $ cycle waveform
  where
    numSamples = fromIntegral $ samplesPerPeriod freq
    waveform = map (wave . (/ numSamples)) [0.0 .. numSamples]

-- Attack Decay Sustain Release Envelope

data ADSR = ADSR
    { attack :: Seconds,
      decay :: Seconds,
      systain :: Double,
      release :: Seconds    
    }
    deriving (Show)

adsr :: ADSR -> Signal -> Signal
adsr (ADSR a d s r) signal =
    zipWith3 (\adsCurve rCurve sample ->
                adsCurve * rCurve * sample)
             (att ++ dec ++ sus)
             rel
             signal
    where
        attackSamples = fromIntegral $ samplesPerSecond a
        decaySamples = fromIntegral $ samplesPerSecond d
        releaseSamples = fromIntegral $ samplesPerSecond r

        att = map (/ attackSamples) [0.0 .. attackSamples]

        dec = reverse $ map (\x -> ((x / decaySamples) * (1 - s)) + s)
                            [0.0 .. decaySamples - 1]

        sus = repeat s

        rel = reverse $ take (length signal)
                             (map (/ releaseSamples)
                                  [0.0 .. releaseSamples] ++ repeat 1.0)
  
-- Oscillators

newtype Oscillator = Osc {playEvent :: Event -> Signal}

osc :: Wave -> ADSR -> Oscillator
osc wave adsrParams = Osc oscf
	where
		oscf (Silence _ t) = silence t
		oscf (Tone f _ t) = adsr adsrParams $ tone wave f t

piano :: Oscillator
piano = osc saw $ ADSR 0.01 0.6 0.3 0.2

ocarina :: Oscillator
ocarina = osc sin $ ADSR 0.01 0.3 0.7 0.01

violin :: Oscillator
violin = osc saw $ ADSR 2 2 0.1 0.2

pluck :: Oscillator
pluck = osc sqw $ ADSR 0.01 0.05 0.0 0.01

bass :: Oscillator
bass = osc tri $ ADSR 0.001 0.2 0.9 0.1

-- Modulation

modulate :: (Signal -> Signal) -> Oscillator -> Oscillator
modulate modF (Osc playEvent) = Osc (modF . playEvent)

tremolo :: Wave -> Hz -> Oscillator -> Oscillator
tremolo waveform frequency = modulate modF
  where
    modWave =
      tone'
        waveform
        frequency
        (sampleRate / fromIntegral (samplesPerPeriod frequency))
    modF = zipWith (*) (cycle modWave)






