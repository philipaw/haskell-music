import qualified Data.ByteString.Lazy          as B
import qualified Data.ByteString.Builder       as B
import           Data.Foldable
import           System.Process
import           Text.Printf
import           Music.Base
import           Music.Pulse
import           Music.Song.Base
import           Music.Song.Darude

outputFilePath :: FilePath
outputFilePath = "output.bin"

save :: Song -> IO ()
save song =
  B.writeFile "output.bin" $ B.toLazyByteString $ foldMap B.floatLE $ foldMap
    (uncurry $ pulse sinPulse)
    song

play :: IO ()
play = do
  save darude
  _ <- runCommand
    $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()
