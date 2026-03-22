module Sound.Synth
    ( Sound.Synth.sin,
      sqw,
      saw,
      tri,
      silence,
      tone,
      ADSR (..),
      adsr
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





