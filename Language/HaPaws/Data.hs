{-# LANGUAGE Rank2Types, DeriveDataTypeable, ExistentialQuantification #-}

module Language.HaPaws.Data where

import           Control.Concurrent.MVar
import qualified Data.Atom.Simple as Atom
import           Data.Dynamic
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable

data Object = Object { objectData :: Dynamic }

type ObjectRef = MVar Object

new :: Typeable a => a -> IO ObjectRef
new a = newMVar $ Object { objectData = toDyn a }

clone :: ObjectRef -> IO ObjectRef
clone ref = readMVar ref >>= newMVar

newtype Symbol = Symbol Atom.Symbol
                 deriving (Eq, Ord, Show, Typeable)

newSymbol :: Text -> IO ObjectRef
newSymbol = new . Symbol . Atom.intern . Text.unpack

data Execution = Execution { executionRoot    :: Script
                           , isPristine       :: Bool
                           , instructionStack :: [[Node]]
                           , dataStack        :: [ObjectRef]
                           }
               deriving Typeable

newExecution :: Script -> IO ObjectRef
newExecution script = new $ Execution { executionRoot    = script
                                      , isPristine       = True
                                      , instructionStack = []
                                      , dataStack        = []
                                      }

data Script = Script [Node]

data Node = ExpressionNode [Node]
          | ObjectNode ObjectRef
