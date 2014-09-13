module Proof where

import Parser
import Control.Monad.ST
import Data.HashTable.ST.Cuckoo

type Fail = String
type Annotation = String
type CheckedProof = Either Fail [(Logic, Annotation)]

compareAsTemplate :: Logic -> Logic -> Bool
compareAsTemplate = undefined

proofCheck :: [Logic] -> ST s
