import Data.List

data Group = Group {
    size            :: Integer
} deriving (Show, Eq)

instance Ord Group where
    (Group s1) `compare` (Group s2) = s1 `compare` s2

addMember :: Group -> Group
addMember (Group s) = Group (s + 1)

emptyGroups :: Integral a => a -> [Group]
emptyGroups n = take (fromIntegral n) (map Group [0,0..])

data Params = Params {
    memberContrib       :: Double,
    memberDetr          :: Double,
    memberSelflessness  :: Double,
    maxGroups           :: Integer,
    numJoiners          :: Integer
}

-- best fitness simulation without selflessness
bestFitNoSelflessSim :: Params -> (Params -> Group -> Double) -> [Group]
bestFitNoSelflessSim p f = bfnssHelper p f (emptyGroups (maxGroups p)) (numJoiners p)

bfnssHelper :: Params -> (Params -> Group -> Double) -> [Group] -> Integer -> [Group]
bfnssHelper _ _ acc 0 = acc
bfnssHelper p f acc n = bfnssHelper p f (step p f acc) (n - 1)

-- finds the group that maximizes the fitness function,
-- if there is a tie, randomly pick one
step :: Params -> (Params -> Group  -> Double) -> [Group] -> [Group]
step p fitnessFunc state
    = case length maxes of
          0     -> error "How did you do this"
          1     -> addMember best : tail sorted
          _     -> addMember best : tail sorted -- TODO explore other options
    where
        sorted = sortBy fitnessCmp state
        best = head sorted
        maxes = filter (\ g1 -> fitnessFunc p g1 == fitnessFunc p best) sorted
        fitnessCmp g1 g2 = fitnessFunc p g2 `compare` fitnessFunc p g1

-- user defined functions

-- The simple fitness function derived from dF/dn = a - b*n
-- F(n) = a*n + (b/2)*n^2
fitness ::  Params -> Group -> Double
fitness (Params a b _ _ _) (Group gSize)
    = (a * fromIntegral gSize) - (b/2) * (fromIntegral gSize ^^ (2 :: Integer))
