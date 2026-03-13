module Util.Types
    (
        Sample,
        Signal,
        Hz,
        Seconds,
        sampleRate,
        samplesPerPeriod,
        samplesPerSecond
    )
    where

type Sample = Double

type Signal = [Sample]

type Hz = Double

type Seconds = Double

sampleRate :: Double
sampleRate = 44100

samplesPerPeriod :: Hz -> Int
samplesPerPeriod hz = round $ sampleRate / hz

samplesPerSecond :: Seconds -> Int
samplesPerSecond duration = round $ duration * sampleRate