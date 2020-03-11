-- |
--
-- Common utility functions.
module Moo.GeneticAlgorithm.Utilities
  ( -- * Non-deterministic functions
    getRandomGenomes,
    doCrossovers,
    doNCrossovers,
  )
where

import Control.Monad (replicateM)
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Types

-- | Generate @n@ random genomes made of elements in the
-- hyperrectangle ranges @[(from_i,to_i)]@. Return a list of genomes
-- and a new state of random number generator.
randomGenomes ::
  (Random a, Ord a) =>
  -- | random number generator
  PureMT ->
  -- | n, number of genomes to generate
  Int ->
  -- | ranges for individual genome elements
  [(a, a)] ->
  ([Genome a], PureMT)
randomGenomes rng n ranges =
  let sortRange (r1, r2) = (min r1 r2, max r1 r2)
      ranges' = map sortRange ranges
   in flip runRand rng
        $ replicateM n
        $ mapM getRandomR ranges'

-- | Generate @n@ uniform random genomes with individual genome
-- elements bounded by @ranges@. This corresponds to random uniform
-- sampling of points (genomes) from a hyperrectangle with a bounding
-- box @ranges@.
getRandomGenomes ::
  (Random a, Ord a) =>
  -- | @n@, how many genomes to generate
  Int ->
  -- | ranges for individual genome elements
  [(a, a)] ->
  -- | random genomes
  Rand [Genome a]
getRandomGenomes n ranges =
  liftRand $ \rng -> randomGenomes rng n ranges

-- | Crossover all available parents. Parents are not repeated.
doCrossovers :: [Genome a] -> CrossoverOp a -> Rand [Genome a]
doCrossovers [] _ = return []
doCrossovers parents xover = do
  (children', parents') <- xover parents
  if null children'
    then return parents'
    else do
      rest <- doCrossovers parents' xover
      return $ children' ++ rest

-- | Produce exactly @n@ offsprings by repeatedly running the @crossover@
-- operator between randomly selected parents (possibly repeated).
doNCrossovers ::
  -- | @n@, number of offsprings to generate
  Int ->
  -- | @parents@' genomes
  [Genome a] ->
  -- | @crossover@ operator
  CrossoverOp a ->
  Rand [Genome a]
doNCrossovers _ [] _ = return []
doNCrossovers n parents xover =
  doAnotherNCrossovers n []
  where
    doAnotherNCrossovers i children
      | i <= 0 = return . take n . concat $ children
      | otherwise = do
        (children', _) <- xover =<< shuffle parents
        if null children'
          then doAnotherNCrossovers 0 children -- no more children
          else doAnotherNCrossovers (i - length children') (children' : children)
