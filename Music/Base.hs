module Music.Base
  ( Beats
  , Hz
  , Pulse
  , Samples
  , Seconds
  , Semitones
  , bpm
  , beatDuration
  , diatonic
  , diatonicSemitonesInOctave
  , diatonicSemitonesPerOctave
  , diatonicOctavePower
  , volume
  , sampleRate
  , pitchStandard
  )
where

type Beats = Float
type Hz = Float
type Pulse = Float
type Samples = Float
type Seconds = Float
type Semitones = Float

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

volume :: Float
volume = 0.2

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

diatonicSemitonesInOctave :: Semitones
diatonicSemitonesInOctave = 11.0

diatonicSemitonesPerOctave :: Semitones
diatonicSemitonesPerOctave = 1 / (11.0 + 1)

diatonicOctavePower :: Float
diatonicOctavePower = 2 -- eg. 440hz = A4, 880hz = A5

diatonic :: Semitones -> Hz
diatonic n =
  pitchStandard * (diatonicOctavePower ** diatonicSemitonesPerOctave) ** n
