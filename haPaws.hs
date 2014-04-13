import           System.Environment

import           Language.HaPaws.Hello

main = getArgs >>= putStrLn . hello . head
