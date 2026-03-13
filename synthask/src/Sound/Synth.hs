module Sound.Synth
    ( Sound.Synth.sin,
      sqw,
      saw,
      tri,
      silence,
      tone
    ) 
where

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