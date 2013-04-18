module Tests.Internals.TestMultiobjective where


import Test.HUnit
import Control.Monad (forM_)
import Data.Function (on)
import Data.List (sortBy)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Multiobjective.Types
import Moo.GeneticAlgorithm.Multiobjective.NSGA2


import System.Random.Mersenne.Pure64 (pureMT)


dummyGenome :: [Objective] -> MultiPhenotype ()
dummyGenome ovs = ([], ovs)


testMultiobjective =
    TestList
    [ "domination predicate" ~: do
        let problems = [Minimizing, Maximizing, Minimizing]
        let worst = dummyGenome [100, 0, 100]
        let good1 = dummyGenome [0, 50, 50]
        let good23 = dummyGenome [50, 100, 0]
        let best = dummyGenome [0, 100, 0]
        assertEqual "good dominates worst"
                    True (domination problems good1 worst)
        assertEqual "good23 doesn't dominate good1"
                    False (domination problems good23 good1)
        assertEqual "good1 doesn't dominate good23"
                    False (domination problems good1 good23)
        assertEqual "best dominates good23"
                    True (domination problems best good23)
        assertEqual "worst doesn't dominate best"
                    False (domination problems worst best)
    , "non-dominated sort" ~: do
        let genomes = [ ([1], [2, 2]), ([2], [3, 2]), ([2,2], [2,3])
                      , ([3], [1,1.5]), ([3,3], [1.5, 0.5]), ([4], [0,0::Double])]
        assertEqual "non-dominated fronts"
                    [[[4]],[[3],[3,3]],[[1]],[[2],[2,2]]]
                    (map (map fst) $ nondominatedSort [Minimizing,Minimizing] genomes)
    , "calculate crowding distance" ~: do
        let inf = 1.0/0.0 :: Double
        assertEqual "two points" [inf, inf] $ crowdingDistances [[1],[2]]
        assertEqual "4 points" [inf, 2.5, inf, 2.0] $ crowdingDistances [[1.0], [2.0], [4.0], [3.5]]
        assertEqual "4 points 2D" [inf, 2.0, inf, 0.75, 2.0] $
                    crowdingDistances [[3,1], [1.75,1.75], [1,3], [2,2], [2.125,2.125]]
    , "rank with crowding" ~: do
        let gs = map (\x -> ([], x)) [[2,1],[1,2],[3,1],[1.9,1.9],[1,3]]
        let rs = concat $ rankAllSolutions [Minimizing,Minimizing] gs
        let inf = 1.0/0.0 :: Double
        assertEqual "non-dom ranks" [1,1,1,2,2]
                    (map rs'nondominationRank rs)
        assertEqual "in-front crowding distance" [inf, inf, 2.0, inf, inf]
                    (map rs'localCrowdingDistnace rs)
    , "calculate all objectives for all genomes" ~: do
        let genomes = [[8, 2], [2.0, 1.0], [1.0, 2.0], [4,4]]
        let objectives = [(Minimizing, sum), (Maximizing, product)]
                       :: [(ProblemType, [Double] -> Double)]
        let correct = [([8.0,2.0],[10.0,16.0]),([2.0,1.0],[3.0,2.0])
                      ,([1.0,2.0],[3.0,2.0]),([4.0,4.0],[8.0,16.0])]
        assertEqual "two objective functions" correct $
                    evalAllObjectives objectives genomes
    , "NSGA-II ranking with crowding" ~: do
        let mp = [ (Minimizing, (!!0))
                 , (Minimizing, (!!1))
                 ] :: [(ProblemType, [Double] -> Double)]
        let gs = [ [5,1], [1,5], [2,4], [3,3]  -- first front
                 , [6,6]                       -- third front
                 , [6,2], [5,3], [4,4], [2,6]  -- second front
                 ] :: [[Double]]
        let expected7 = [(([5.0,1.0],[5.0,1.0]),1.0)
                        ,(([1.0,5.0],[1.0,5.0]),1.0) -- order is preserved in the first front:
                        ,(([2.0,4.0],[2.0,4.0]),1.0) -- [2,4] is more crowded than [3,3]
                        ,(([3.0,3.0],[3.0,3.0]),1.0) -- but it doesn't matter for full fronts
                        ,(([6.0,2.0],[6.0,2.0]),2.0)
                        ,(([2.0,6.0],[2.0,6.0]),2.0) -- is front boundary point, and goes before [4,4]
                        ,(([4.0,4.0],[4.0,4.0]),2.0) -- is less crowded than [5,3]
                        -- [5,3] is more crowded and is truncated
                        -- [6,6] is in the third front and is truncated
                        ]
        let result7 = nsga2Ranking mp 7 gs
        assertEqual "7 solutions" expected7 result7
    , "NSGA-II ranking (output length)" ~: do
        let mp = [ (Minimizing, (!!0))
                 , (Minimizing, (!!1))
                 ] :: [(ProblemType, [Double] -> Double)]
        let gs = [ [5,1], [1,5], [2,4], [3,3]  -- first front
                 , [6,6]                       -- third front
                 , [6,2], [5,3], [4,4], [2,6]  -- second front
                 ] :: [[Double]]
        forM_ [0..(length gs)] $ \n -> do
          assertEqual (show n ++ " solutions") n $ length (nsga2Ranking mp n gs)
        assertEqual "max # of solutions" (length gs) $ length (nsga2Ranking mp maxBound gs)
    , "two NSGA-II steps" ~: do
        let mp = [ (Minimizing, (!!0))
                 , (Minimizing, (!!1))
                 ] :: [(ProblemType, [Double] -> Double)]
        let gs = [ [5,1], [1,5], [2,4], [3,3]  -- first front
                 , [6,6]                       -- third front
                 , [6,2], [5,3], [4,4], [2,6]  -- second front
                 ] :: [[Double]]
        let expected = [([1.0,5.0],1.0),([5.0,1.0],1.0),([1.0,5.0],1.0)
                       ,([5.0,1.0],1.0),([3.0,3.0],1.0),([3.0,3.0],1.0)
                       ,([2.0,4.0],1.0),([2.0,4.0],1.0),([1.0,5.0],1.0)]
        let result = flip evalRandom (pureMT 1) $
                     loop (Generations 1)
                     (stepNSGA2default mp noCrossover noMutation) gs
        assertEqual "solutions and ranking" expected result
    ]