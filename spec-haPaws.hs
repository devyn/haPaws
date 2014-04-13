import           Test.Hspec

import qualified Spec.Language.HaPaws.Hello
import qualified Spec.Language.HaPaws.CPaws

-- | A list of all specs.
specs :: [Spec]
specs =
  [Spec.Language.HaPaws.Hello.spec
  ,Spec.Language.HaPaws.CPaws.spec]

-- | Runs all of the specs.
main :: IO ()
main = hspec $ foldl1 (>>) specs
