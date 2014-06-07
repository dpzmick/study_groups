import Data.List

data Group = Group {
    size            :: Integer
} deriving (Show, Eq)

instance Ord Group where
    (Group s1) `compare` (Group s2) = s1 `compare` s2

addMember :: Group -> Group
addMember (Group s) = Group (s + 1)

data Params = Params {
    memberContrib       :: Double,
    memberDetr          :: Double,
    memberSelflessness  :: Double,
    maxGroups           :: Integer,
    numJoiners          :: Integer
}

-- best fitness simulation without selflessness
-- bestFitNoSelflessSim :: Params -> (Group -> Params -> Double) -> [Group]
-- bestFitNoSelflessSim p fitnessFunc
--    = step p fitnessFunc (take (fromIntegral (maxGroups p)) (map Group [0,0..]))

-- finds the group that maximizes the fitness function,
-- if there is a tie, randomly pick one
step :: Params -> (Params -> Group  -> Double) -> [Group] -> [Group]
step p fitnessFunc state
    = addMember (head sorted) : tail sorted
    where
        sorted = sortBy fitnessCmp state
        fitnessCmp g1 g2 = fitnessFunc p g1 `compare` fitnessFunc p g2


-- user defined functions

-- The simple fitness function derived from dF/dn = a - b*n
-- F(n) = a*n + (b/2)*n^2
fitness ::  Params -> Group -> Double
fitness (Params a b _ _ _) (Group gSize)
    = (a * fromIntegral gSize) - (b/2) * (fromIntegral gSize ^^ (2 :: Integer))
