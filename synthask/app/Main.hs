module Main where

import Data.Array.Unboxed (listArray)
import qualified Codec.Wav
import Data.Audio
import Data.Int (Int16)

import Util.Types
import Sound.Synth

signalToSampleData :: Signal -> SampleData Int16
signalToSampleData signal =
  listArray (0, n) $ map fromSample signal
  where
    n = length signal - 1

limit :: Signal -> Signal
limit = map (min threshold . max (-threshold) . (* threshold))
  where 
    threshold = 0.9

writeWav :: FilePath -> Signal -> IO ()
writeWav filePath signal = do
  putStrLn $
    "Writing " ++ show (length signal) ++ " samples to " ++ filePath
  let sampleData = signalToSampleData $ limit signal
      audio = 
        Audio
          {
            Data.Audio.sampleRate = round Util.Types.sampleRate,
            channelNumber = 1,
            sampleData = sampleData
          }
  Codec.Wav.exportFile filePath audio


main :: IO ()
main = return ()

