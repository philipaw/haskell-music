module Music.Pulse
  ( Osc
  , adsr
  , pulse
  , sinPulse
  , squarePulse
  )
where

import           Music.Base

type Osc = Hz -> Seconds -> [Pulse]

pulse :: Osc -> Semitones -> Beats -> [Pulse]
pulse osc n beats = osc (diatonic n) (beats * beatDuration)

-- applies an envelope to the wave
adsr :: [Pulse] -> [Pulse]
adsr xs = zipWith3 (\a d x -> a * d * x) attack decay
  $ zipWith3 (\s r x -> s * r * x) sustain release xs
 where
  attack :: [Pulse]
  attack = map (min 1.0) [0.0, 0.01 ..]

  decay :: [Pulse] -- TODO
  decay = map (const 1) [1 ..]

  sustain :: [Pulse] -- TODO
  sustain = map (const 1) [1 ..]

  release :: [Pulse]
  release = reverse $ take (length xs) attack

rawSamples :: Beats -> [Pulse]
rawSamples b = [0.0 .. sampleRate * b]


sinPulse :: Osc
sinPulse hz duration = adsr output
 where
  step = (hz * 2 * pi) / sampleRate

  output :: [Pulse]
  output = map (\x -> sin (step * x) * volume) $ rawSamples duration

sawPulse :: Osc
sawPulse hz duration = [0] -- TODO

squarePulse :: Osc
squarePulse hz duration = adsr output
 where
  step = (hz * 2 * pi) / sampleRate

  output :: [Pulse]
  output = map (\x -> if sin (step * x) > 0 then volume else (-1) * volume)
    $ rawSamples duration

-- trianglePulse
-- sawPulse

