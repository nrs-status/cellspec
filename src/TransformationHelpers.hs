module TransformationHelpers where

import CellSpec
import MyLib
import Control.Monad

keepImmediatePairs :: [CellSpec] -> [CellSpec]
keepImmediatePairs l =
    let tuplicate = tuplize 2 l in
    let predicate1 = (== '=') . char . head in
    let predicate2 = (== ';') . char . (!! 1) in
    let filtering = filter (liftM2 (&&) predicate1 predicate2) tuplicate in
    concat filtering
    
