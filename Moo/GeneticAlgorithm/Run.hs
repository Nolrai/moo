{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- |
--
-- Helper functions to run genetic algorithms and control iterations.
module Moo.GeneticAlgorithm.Run
  ( -- * Running algorithm
    runGA,
    runIO,
    nextGeneration,
    nextSteadyState,
    makeStoppable,

    -- * Iteration control
    loop,
    loopWithLog,
    loopIO,
    Cond (..),
    LogHook (..),
    IOHook (..),
  )
where

import Control.Monad (liftM, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Selection (bestFirst)
import Moo.GeneticAlgorithm.StopCondition
import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Utilities (doCrossovers, doNCrossovers)

-- | Helper function to run the entire algorithm in the 'Rand' monad.
-- It takes care of generating a new random number generator.
runGA ::
  -- | function to create initial population
  Rand [Genome a] ->
  -- | genetic algorithm, see also 'loop' and 'loopWithLog'
  ([Genome a] -> Rand b) ->
  -- | final population
  IO b
runGA initialize ga = do
  rng <- newPureMT
  let (genomes0, rng') = runRand initialize rng
  return $ evalRand (ga genomes0) rng'

-- | Helper function to run the entire algorithm in the 'IO' monad.
runIO ::
  -- | function to create initial population
  Rand [Genome a] ->
  -- | genetic algorithm, see also 'loopIO'
  (IORef PureMT -> [Genome a] -> IO (Population a)) ->
  -- | final population
  IO (Population a)
runIO initialize gaIO = do
  rng <- newPureMT
  let (genomes0, rng') = runRand initialize rng
  rngref <- newIORef rng'
  gaIO rngref genomes0

-- | Construct a single step of the genetic algorithm.
--
-- See "Moo.GeneticAlgorithm.Binary" and "Moo.GeneticAlgorithm.Continuous"
-- for the building blocks of the algorithm.
nextGeneration ::
  (ObjectiveFunction objectivefn a) =>
  -- | a type of the optimization @problem@
  ProblemType ->
  -- | objective function
  objectivefn ->
  -- | selection operator
  SelectionOp a ->
  -- | @elite@, the number of genomes to keep intact
  Int ->
  -- | crossover operator
  CrossoverOp a ->
  -- | mutation operator
  MutationOp a ->
  StepGA Rand a
nextGeneration problem objective selectOp elite xoverOp mutationOp =
  makeStoppable objective $ \pop -> do
    genomes' <- liftM (map takeGenome) $ withElite problem elite selectOp pop
    let top = take elite genomes'
    let rest = drop elite genomes'
    genomes' <- shuffle rest -- just in case if @selectOp@ preserves order
    genomes' <- doCrossovers genomes' xoverOp
    genomes' <- mapM mutationOp genomes'
    return $ evalObjective objective (top ++ genomes')

-- | Construct a single step of the incremental (steady-steate) genetic algorithm.
-- Exactly @n@ worst solutions are replaced with newly born children.
--
-- See "Moo.GeneticAlgorithm.Binary" and "Moo.GeneticAlgorithm.Continuous"
-- for the building blocks of the algorithm.
nextSteadyState ::
  (ObjectiveFunction objectivefn a) =>
  -- | @n@, number of worst solutions to replace
  Int ->
  -- | a type of the optimization @problem@
  ProblemType ->
  -- | objective function
  objectivefn ->
  -- | selection operator
  SelectionOp a ->
  -- | crossover operator
  CrossoverOp a ->
  -- | mutation operator
  MutationOp a ->
  StepGA Rand a
nextSteadyState n problem objective selectOp crossoverOp mutationOp =
  makeStoppable objective $ \pop -> do
    let popsize = length pop
    parents <- liftM (map takeGenome) (selectOp pop)
    children <- mapM mutationOp =<< doNCrossovers n parents crossoverOp
    let sortedPop = bestFirst problem pop
    let cpop = evalObjective objective children
    return . take popsize $ cpop ++ sortedPop

-- | Wrap a population transformation with pre- and post-conditions
-- to indicate the end of simulation.
--
-- Use this function to define custom replacement strategies
-- in addition to 'nextGeneration' and 'nextSteadyState'.
makeStoppable ::
  (ObjectiveFunction objectivefn a, Monad m) =>
  objectivefn ->
  (Population a -> m (Population a)) -> -- single step
  StepGA m a
makeStoppable objective onestep stop input = do
  let pop = either (evalObjective objective) id input
  if isGenomes input && evalCond stop pop
    then return $ StopGA pop -- stop before the first iteration
    else do
      newpop <- onestep pop
      return $
        if evalCond stop newpop
          then StopGA newpop
          else ContinueGA newpop
  where
    isGenomes (Left _) = True
    isGenomes (Right _) = False

-- | Select @n@ best genomes, then select more genomes from the
-- /entire/ population (elite genomes inclusive). Elite genomes will
-- be the first in the list.
withElite :: ProblemType -> Int -> SelectionOp a -> SelectionOp a
withElite problem n select = \population -> do
  let elite = take n . eliteGenomes $ population
  selected <- select population
  return (elite ++ selected)
  where
    eliteGenomes = bestFirst problem

-- | Run strict iterations of the genetic algorithm defined by @step@.
-- Return the result of the last step.  Usually only the first two
-- arguments are given, and the result is passed to 'runGA'.
{-# INLINE loop #-}
loop ::
  (Monad m) =>
  -- | termination condition @cond@
  Cond a ->
  -- | @step@ function to produce the next generation
  StepGA m a ->
  -- | initial population
  [Genome a] ->
  -- | final population
  m (Population a)
loop cond step genomes0 = go cond (Left genomes0)
  where
    go cond !x = do
      x' <- step cond x
      case x' of
        (StopGA pop) -> return pop
        (ContinueGA pop) -> go (updateCond pop cond) (Right pop)

-- | GA iteration interleaved with the same-monad logging hooks.
-- Usually only the first three arguments are given, and the result is
-- passed to 'runGA'.
{-# INLINE loopWithLog #-}
loopWithLog ::
  (Monad m, Monoid w) =>
  -- | periodic logging action
  LogHook a m w ->
  -- | termination condition @cond@
  Cond a ->
  -- | @step@ function to produce the next generation
  StepGA m a ->
  -- | initial population
  [Genome a] ->
  -- | final population
  m (Population a, w)
loopWithLog hook cond step genomes0 = go cond 0 mempty (Left genomes0)
  where
    go cond !i !w !x = do
      x' <- step cond x
      case x' of
        (StopGA pop) -> return (pop, w)
        (ContinueGA pop) -> do
          let w' = mappend w (runHook i pop hook)
          let cond' = updateCond pop cond
          go cond' (i + 1) w' (Right pop)
    runHook !i !x (WriteEvery n write)
      | (rem i n) == 0 = write i x
      | otherwise = mempty

-- | GA iteration interleaved with IO (for logging or saving the
-- intermediate results); it takes and returns the updated random
-- number generator via an IORef. Usually only the first three
-- arguments are given, and the result is passed to 'runIO'.
{-# INLINE loopIO #-}
loopIO ::
  -- | input-output actions, special and time-dependent stop conditions
  [IOHook a] ->
  -- | termination condition @cond@
  Cond a ->
  -- | @step@ function to produce the next generation
  StepGA Rand a ->
  -- | reference to the random number generator
  IORef PureMT ->
  -- | initial population @pop0@
  [Genome a] ->
  -- | final population
  IO (Population a)
loopIO hooks cond step rngref genomes0 = do
  rng <- readIORef rngref
  start <- realToFrac `liftM` getPOSIXTime
  (pop, rng') <- go start cond 0 rng (Left genomes0)
  writeIORef rngref rng'
  return pop
  where
    go start cond !i !rng !x = do
      stop <- (any id) `liftM` (mapM (runhook start i x) hooks)
      if (stop || either (const False) (evalCond cond) x)
        then return (asPopulation x, rng)
        else do
          let (x', rng') = runRand (step cond x) rng
          case x' of
            (StopGA pop) -> return (pop, rng')
            (ContinueGA pop) ->
              do
                let i' = i + 1
                let cond' = updateCond pop cond
                go start cond' i' rng' (Right pop)
    -- runhook returns True to terminate the loop
    runhook _ i x (DoEvery n io) = do
      when ((rem i n) == 0) (io i (asPopulation x))
      return False
    runhook _ _ _ (StopWhen iotest) = iotest
    runhook start _ _ (TimeLimit limit) = do
      now <- realToFrac `liftM` getPOSIXTime
      return (now >= start + limit)
    -- assign dummy objective value to a genome
    dummyObjective :: Genome a -> Phenotype a
    dummyObjective g = (g, 0.0)
    asPopulation = either (map dummyObjective) id

-- | Logging to run every @n@th iteration starting from 0 (the first parameter).
-- The logging function takes the current generation count and population.
data LogHook a m w where
  WriteEvery ::
    (Monad m, Monoid w) =>
    Int ->
    (Int -> Population a -> w) ->
    LogHook a m w

-- | Input-output actions, interactive and time-dependent stop conditions.
data IOHook a
  = -- | action to run every @n@th iteration, starting from 0;
    -- initially (at iteration 0) the objective value is zero.
    DoEvery {io'n :: Int, io'action :: (Int -> Population a -> IO ())}
  | -- | custom or interactive stop condition
    StopWhen (IO Bool)
  | -- | terminate iteration after @t@ seconds
    TimeLimit {io't :: Double}
