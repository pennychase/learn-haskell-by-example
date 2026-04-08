{-# LANGUAGE TypeSynonymInstances #-}

module Composition.Pitch
  ( 
    Pitchable (..),
    Semitone,
    Chromatic (..),
    ChromaticName (..),
    chromaticToSemitone,
    a,
    as,
    b,
    c,
    cs,
    d,
    ds,
    e,
    f,
    fs,
    g,
    gs,
  )
where

import Numeric.Natural (Natural)

import Util.Types

-- Pitch

class Pitchable a where
  toFrequency :: a -> Hz

instance Pitchable Hz where
  toFrequency = id

type Semitone = Integer

instance Pitchable Semitone where
  toFrequency semitone = 440 * (2.0 ** (fromInteger semitone / 12))

-- Chromatic Notes

data ChromaticName = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs
  deriving (Show, Eq)

data Chromatic = Chromatic ChromaticName Natural
  deriving (Show, Eq)

a :: Natural -> Chromatic
a = Chromatic A

as :: Natural -> Chromatic
as = Chromatic As

b :: Natural -> Chromatic
b = Chromatic B

c :: Natural -> Chromatic
c = Chromatic C

cs :: Natural -> Chromatic
cs = Chromatic Cs

d :: Natural -> Chromatic
d = Chromatic D

ds :: Natural -> Chromatic
ds = Chromatic Ds

e :: Natural -> Chromatic
e = Chromatic E

f :: Natural -> Chromatic
f = Chromatic F

fs :: Natural -> Chromatic
fs = Chromatic Fs

g :: Natural -> Chromatic
g = Chromatic G

gs :: Natural -> Chromatic
gs = Chromatic Gs

chromaticToSemitone :: Chromatic -> Semitone
chromaticToSemitone (Chromatic name oct) =
  (12 * (fromIntegral oct - 4)) + noteOffset name
  where 
    noteOffset C  = -9
    noteOffset Cs = -8 
    noteOffset D  = -7
    noteOffset Ds = -6
    noteOffset E  = -5
    noteOffset F  = -4
    noteOffset Fs = -3
    noteOffset G  = -2
    noteOffset Gs = -1
    noteOffset A  = 0
    noteOffset As = 1 
    noteOffset B  = 2

instance Pitchable Chromatic where
  toFrequency = toFrequency . chromaticToSemitone
