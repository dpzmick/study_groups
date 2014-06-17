import Data.List (sortBy, maximumBy)
import Data.Function (on)
import System.Random.Shuffle (shuffle')
import System.Random (getStdGen, RandomGen)

data Params = Params {
    memberContrib   :: Double,
    memberDetriment :: Double,
    numJoiners      :: Integer,
    numGroups       :: Integer
}

-- group stuff

type Group = Integer

-- allow a group to become more complex if needed. Is this bad style?
addMember :: Group -> Group
addMember g = g + 1

size :: Group -> Integer
size g = g

emptyGroups :: [Group]
emptyGroups = [0,0..] :: [Group]

fitness :: Params -> Group -> Double
fitness (Params a b _ _) g =
        a * fromIntegral (size g) - (b / 2) * ( fromIntegral (size g) ^ (2 :: Integer) )


-- sim stuff
sim :: RandomGen g => Params -> g -> [Group]
sim ps@(Params _ _ joiners groups) g =
        iterate (oneJoin ps g) startingGroups !! fromIntegral joiners
    where
        startingGroups = take (fromIntegral groups) emptyGroups

-- find all groups with best fitness, randomly pick one and join it
oneJoin :: RandomGen g => Params -> g -> [Group] -> [Group]
oneJoin ps gen gs =
        addMember (head bestsShuffled) : tail bestsShuffled ++ rests
    where
        bestFitness = maximum (map (fitness ps) gs)
        bests = filter (\ g -> (fitness ps g) == bestFitness) gs
        rests = filter (\ g -> (fitness ps g) /= bestFitness) gs
        bestsShuffled = shuffle' bests (length bests) gen


main :: IO ()
main = do
        g <- getStdGen
        print (sim (Params 1 0.5 5 10) g)
