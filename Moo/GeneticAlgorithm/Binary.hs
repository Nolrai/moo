-- |
--
-- Binary genetic algorithms. Candidates solutions are represented as bit-strings.
--
-- Choose Gray code if sudden changes to the variable value after a point
-- mutation are undesirable, choose binary code otherwise.  In Gray code
-- two successive variable values differ in only one bit, it may help to
-- prevent premature convergence.
--
-- To apply binary genetic algorithms to real-valued problems, the real
-- variable may be discretized ('encodeGrayReal' and
-- 'decodeGrayReal'). Another approach is to use continuous genetic
-- algorithms, see "Moo.GeneticAlgorithm.Continuous".
--
-- To encode more than one variable, just concatenate their codes.
module Moo.GeneticAlgorithm.Binary
  ( -- * Types
    module Moo.GeneticAlgorithm.Types,

    -- * Encoding
    encodeGray,
    decodeGray,
    encodeBinary,
    decodeBinary,
    encodeGrayReal,
    decodeGrayReal,
    bitsNeeded,
    splitEvery,

    -- * Initialization
    getRandomBinaryGenomes,

    -- * Selection
    rouletteSelect,
    stochasticUniversalSampling,
    tournamentSelect,

    -- ** Scaling and niching
    withPopulationTransform,
    withScale,
    rankScale,
    withFitnessSharing,
    hammingDistance,

    -- ** Sorting
    bestFirst,

    -- * Crossover
    module Moo.GeneticAlgorithm.Crossover,

    -- * Mutation
    pointMutate,
    asymmetricMutate,
    constFrequencyMutate,

    -- * Control
    module Moo.GeneticAlgorithm.Random,
    module Moo.GeneticAlgorithm.Run,
  )
where

import Codec.Binary.Gray.List
import Data.Bits
import Data.List (genericLength)
import Moo.GeneticAlgorithm.Crossover
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Selection
import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Utilities (getRandomGenomes)

-- | How many bits are needed to represent a range of integer numbers
-- @(from, to)@ (inclusive).
bitsNeeded :: (Integral a, Integral b) => (a, a) -> b
bitsNeeded (from, to) =
  let from' = min from to
      to' = max from to
   in ceiling . logBase (2 :: Double) . fromIntegral $ (to' - from' + 1)

-- | Encode an integer number in the range @(from, to)@ (inclusive) as
-- binary sequence of minimal length. Use of Gray code means that a
-- single point mutation leads to incremental change of the encoded
-- value.
encodeGray :: (FiniteBits b, Bits b, Integral b) => (b, b) -> b -> [Bool]
encodeGray = encodeWithCode gray

-- | Decode a binary sequence using Gray code to an integer in the
-- range @(from, to)@ (inclusive). This is an inverse of 'encodeGray'.
-- Actual value returned may be greater than @to@.
decodeGray :: (FiniteBits b, Bits b, Integral b) => (b, b) -> [Bool] -> b
decodeGray = decodeWithCode binary

-- | Encode an integer number in the range @(from, to)@ (inclusive)
-- as a binary sequence of minimal length. Use of binary encoding
-- means that a single point mutation may lead to sudden big change
-- of the encoded value.
encodeBinary :: (FiniteBits b, Bits b, Integral b) => (b, b) -> b -> [Bool]
encodeBinary = encodeWithCode id

-- | Decode a binary sequence to an integer in the range @(from, to)@
-- (inclusive). This is an inverse of 'encodeBinary'.  Actual value
-- returned may be greater than @to@.
decodeBinary :: (FiniteBits b, Bits b, Integral b) => (b, b) -> [Bool] -> b
decodeBinary = decodeWithCode id

-- | Encode a real number in the range @(from, to)@ (inclusive)
-- with @n@ equally spaced discrete values in binary Gray code.
encodeGrayReal :: (RealFrac a) => (a, a) -> Int -> a -> [Bool]
encodeGrayReal range n = encodeGray (0, n -1) . toDiscreteR range n

-- | Decode a binary sequence using Gray code to a real value in the
-- range @(from, to)@, assuming it was discretized with @n@ equally
-- spaced values (see 'encodeGrayReal').
decodeGrayReal :: (RealFrac a) => (a, a) -> Int -> [Bool] -> a
decodeGrayReal range n = fromDiscreteR range n . decodeGray (0, n -1)

-- | Represent a range @(from, to)@ of real numbers with @n@ equally
-- spaced values.  Use it to discretize a real number @val@.
toDiscreteR ::
  (RealFrac a) =>
  -- | @(from, to)@, the range to be encoded
  (a, a) ->
  -- | @n@, how many discrete numbers from the range to consider
  Int ->
  -- | a real number in the range @(from, to)@  to discretize
  a ->
  -- | a discrete value (normally in the range @(0, n-1)@)
  Int
toDiscreteR range n val =
  let from = uncurry min range
      to = uncurry max range
      dx = (to - from) / (fromIntegral (n - 1))
   in round $ (val - from) / dx

-- | Take a range @(from, to)@ of real numbers with @n@ equally spaced values.
-- Convert @i@-th value to a real number. This is an inverse of 'toDiscreteR'.
fromDiscreteR ::
  (RealFrac a) =>
  -- | @(from, to)@, the encoded range
  (a, a) ->
  -- | @n@, how many discrete numbers from the range to consider
  Int ->
  -- | a discrete value in the range @(0, n-1)@
  Int ->
  -- | a real number from the range
  a
fromDiscreteR range n i =
  let from = uncurry min range
      to = uncurry max range
      dx = (to - from) / (fromIntegral (n - 1))
   in from + (fromIntegral i) * dx

-- | Split a list into pieces of size @n@. This may be useful to split
-- the genome into distinct equally sized “genes” which encode
-- distinct properties of the solution.
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = let (nxs, rest) = splitAt n xs in nxs : splitEvery n rest

encodeWithCode :: (FiniteBits b, Bits b, Integral b) => ([Bool] -> [Bool]) -> (b, b) -> b -> [Bool]
encodeWithCode code (from, to) n =
  let from' = min from to
      to' = max from to
      nbits = bitsNeeded (from', to')
   in code . take nbits $ toList (n - from') ++ (repeat False)

decodeWithCode :: (FiniteBits b, Bits b, Integral b) => ([Bool] -> [Bool]) -> (b, b) -> [Bool] -> b
decodeWithCode decode (from, to) bits =
  let from' = min from to
   in (from' +) . fromList . decode $ bits

-- | Generate @n@ random binary genomes of length @len@.
-- Return a list of genomes.
getRandomBinaryGenomes ::
  -- | how many genomes to generate
  Int ->
  -- | genome length
  Int ->
  Rand ([Genome Bool])
getRandomBinaryGenomes n len = getRandomGenomes n (replicate len (False, True))

-- | Flips a random bit along the length of the genome with probability @p@.
--  With probability @(1 - p)@ the genome remains unaffected.
pointMutate :: Double -> MutationOp Bool
pointMutate p = withProbability p $ \bits -> do
  r <- getRandomR (0, length bits - 1)
  let (before, (bit : after)) = splitAt r bits
  return (before ++ (not bit : after))

-- | Flip @1@s and @0@s with different probabilities. This may help to control
--  the relative frequencies of @1@s and @0@s in the genome.
asymmetricMutate ::
  -- | probability of a @False@ bit to become @True@
  Double ->
  -- | probability of a @True@ bit to become @False@
  Double ->
  MutationOp Bool
asymmetricMutate prob0to1 prob1to0 = mapM flipbit
  where
    flipbit False = withProbability prob0to1 (return . not) False
    flipbit True = withProbability prob1to0 (return . not) True

-- Preserving the relative frequencies of ones and zeros:
--
-- ones' = p0*(n-ones) + (1-p1)*ones
-- ones + p0*ones + (p1 - 1)*ones = p0*n
-- p0 + p1 = p0 * n / ones
--
-- zeros' = (1-p0)*zeros + p1*(n-zeros)
-- zeros + (p0 - 1)*zeros + p1*zeros = n*p1
-- p0 + p1 = p1 * n / zeros
--
-- => p0 * zeros = p1 * ones
--
-- Average number of changed bits:
--
-- m = p0*zeros + p1*ones
--
-- => p0 = m / (2*zeros)
--    p1 = m / (2*ones)
--
-- Probability of changing a bit:
--
-- p = m / n
--

-- | Flip @m@ bits on average, keeping the relative frequency of @0@s
--  and @1@s in the genome constant.
constFrequencyMutate ::
  Real a =>
  -- | average number of bits to change
  a ->
  MutationOp Bool
constFrequencyMutate m bits =
  let (ones, zeros) = foldr (\b (o, z) -> if b then (o + 1, z) else (o, z + 1)) (0, 0) bits
      p0to1 = fromRational $ 0.5 * (toRational m) / zeros
      p1to0 = fromRational $ 0.5 * (toRational m) / ones
   in asymmetricMutate p0to1 p1to0 bits

-- | Hamming distance between @x@ and @y@ is the number of coordinates
-- for which @x_i@ and @y_i@ are different.
--
-- Reference: Hamming, Richard W. (1950), “Error detecting and error
-- correcting codes”, Bell System Technical Journal 29 (2): 147–160,
-- MR 0035935.
hammingDistance :: (Eq a, Num i) => [a] -> [a] -> i
hammingDistance xs ys = genericLength . filter id $ zipWith (/=) xs ys
