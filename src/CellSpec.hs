{-# LANGUAGE LambdaCase #-}

module CellSpec where

import System.Console.ANSI
import Data.Maybe (catMaybes)
import Numeric.Natural
import MyLib
import Control.Lens.Combinators (takingWhile)
import Control.Lens.Traversal (traversed)


data CellSpecPlus = MkCsp
  { cspPos :: Natural
  , cspFg :: Color
  , cspBg :: Color
  , cspChar :: Char }
  deriving Show

data DefaultCellSpec = MkDcs
  { dcsPos :: Natural
  , dcsChar :: Char }
  deriving Show

data CellSpec = Default DefaultCellSpec | Plus CellSpecPlus
  deriving Show

pos :: CellSpec -> Natural
pos (Default dcs) = dcsPos dcs
pos (Plus csp) = cspPos csp

char :: CellSpec -> Char
char (Default dcs) = dcsChar dcs
char (Plus csp) = cspChar csp


cellSpec_wf_1 :: [CellSpec] -> Bool
cellSpec_wf_1 lcs =
  let onlyPositions = map pos lcs in
  let tuplication = tuplize 2 onlyPositions in
  let taking = takeWhile (\l -> head l < last l) tuplication in
  null taking

maybeCspToCs :: Natural -> Maybe CellSpecPlus -> CellSpec
maybeCspToCs _ (Just csp) = Plus csp
maybeCspToCs n Nothing = Default $ MkDcs n ' '

fill :: [CellSpecPlus] -> [CellSpec]
fill lcsp = 
  let finalPos = cspPos $ last lcsp in
  let posify = map cspPos lcsp in
  let existsPos = map (`elem` posify) [0..finalPos] in
  let maybefy = zipWith (\n b -> if b then Just (head (dropWhile ((/= n) . cspPos) lcsp)) else Nothing) [0..] existsPos in
  zipWith maybeCspToCs [0..] maybefy

unfill :: [CellSpec] -> [CellSpecPlus]
unfill lcs = 
  let maybefy = map (\case Plus csp -> Just csp; Default _ -> Nothing) lcs in
  catMaybes maybefy

mkPrinting :: CellSpec -> IO ()
mkPrinting (Plus (MkCsp _ fgc bgc char)) = do
  setSGR [SetColor Foreground Vivid fgc]
  setSGR [SetColor Background Vivid bgc]
  putChar char
  setSGR [Reset]
mkPrinting (Default (MkDcs _ char)) = putChar char

defaultOfIntCharList :: [(Int, Char)] -> [DefaultCellSpec]
defaultOfIntCharList = map (\(int, c) -> MkDcs (fromIntegral int) c)

plusOfIntCharList :: Color -> Color -> [(Int, Char)] -> [CellSpecPlus]
plusOfIntCharList fgc bgc = map (\(int, c) -> MkCsp (fromIntegral int) fgc bgc c)

type Transformation = [CellSpec] -> [CellSpec]

type Pipeline = [Transformation]

-- myCellSpecs :: [CellSpecPlus]
-- myCellSpecs = [
--   MkCsp 10 Red White 'x',
--   MkCsp 11 Green Red 'k',
--   MkCsp 20 Blue Green '7']
--
-- main :: IO ()
-- main = do
--   mapM_ mkPrinting (fill myCellSpecs)

