module Music.Pulse
  ( Osc
  , pulse
  , sinPulse
  )
where

import           Music.Base

type Osc = Hz -> Seconds -> [Pulse]

pulse :: Osc -> Semitones -> Beats -> [Pulse]
pulse osc n beats = osc (diatonic n) (beats * beatDuration)


sinPulse :: Osc
sinPulse hz duration = zipWith3 (\x y z -> x * y * z) release attack output
 where
  step = (hz * 2 * pi) / sampleRate

  attack :: [Pulse]
  attack = map (min 1.0) [0.0, 0.01 ..]

  release :: [Pulse]
  release = reverse $ take (length output) attack

  output :: [Pulse]
  output = map (\x -> sin (step * x) * volume) [0.0 .. sampleRate * duration]

