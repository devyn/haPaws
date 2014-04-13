import           Test.Hspec

import qualified Spec.Language.HaPaws.Hello

-- | A list of all specs.
specs :: [Spec]
specs =
  [Spec.Language.HaPaws.Hello.spec]

-- | Runs all of the specs.
main :: IO ()
main = hspec $ foldl1 (>>) specs
