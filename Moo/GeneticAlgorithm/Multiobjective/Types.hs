{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Moo.GeneticAlgorithm.Multiobjective.Types
  ( SingleObjectiveProblem,
    MultiObjectiveProblem,
    MultiPhenotype,
    evalAllObjectives,
    takeObjectiveValues,
  )
where

import Data.List (transpose)
import Moo.GeneticAlgorithm.Types

type SingleObjectiveProblem fn = (ProblemType, fn)

type MultiObjectiveProblem fn = [SingleObjectiveProblem fn]

-- | An individual with all objective functions evaluated.
type MultiPhenotype a = (Genome a, [Objective])

instance a1 ~ a2 => GenomeState (MultiPhenotype a1) a2 where
  takeGenome = fst

takeObjectiveValues :: MultiPhenotype a -> [Objective]
takeObjectiveValues = snd

-- | Calculate multiple objective per every genome in the population.
evalAllObjectives ::
  forall fn gt a.
  (ObjectiveFunction fn a, GenomeState gt a) =>
  -- | a list of @problems@
  MultiObjectiveProblem fn ->
  -- | a population of @genomes@
  [gt] ->
  [MultiPhenotype a]
evalAllObjectives problems genomes =
  let rawgenomes = map takeGenome genomes
      pops_per_objective = map (\(_, f) -> evalObjective f rawgenomes) problems
      ovs_per_objective = map (map takeObjectiveValue) pops_per_objective
      ovs_per_genome = transpose ovs_per_objective
   in zip rawgenomes ovs_per_genome
