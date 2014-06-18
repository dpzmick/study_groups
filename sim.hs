import System.Random.Shuffle (shuffle')
import System.Random (getStdGen, RandomGen, randomRs)

data Params = Params {
    memberContrib   :: Double,
    memberDetriment :: Double,
    selflessness    :: Double,
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
fitness (Params a b _ _ _) g =
        a * fromIntegral (size g) - (b / 2) * ( fromIntegral (size g) ^ (2 :: Integer) )

-- sim stuff
sim :: RandomGen g => Params -> g -> [Double] -> [Group]
sim ps gen chances =
        simIterate ps gen chances startingGroups (fromIntegral (numJoiners ps))
    where
        startingGroups = take (fromIntegral (numGroups ps)) emptyGroups

simIterate :: RandomGen g => Params -> g -> [Double] -> [Group] -> Integer -> [Group]
simIterate ps gen chances gs remain
    | remain /= 1 = simIterate ps gen (tail chances) (oneJoin ps gen chances gs) (remain - 1)
    | remain == 1 = oneJoin ps gen chances gs
    | otherwise = error "remain should never drop below 1"

-- find all groups with best fitness, randomly pick one and join it
oneJoin :: RandomGen g => Params -> g -> [Double] -> [Group] -> [Group]
oneJoin ps@(Params _ _ s _ _) gen (chance:_) gs =
        if chance <= s
            then selflessJoin ps gen gs
            else normalJoin ps gen gs
oneJoin _ _ [] _ = error "this list should never be empty"

normalJoin :: RandomGen g => Params -> g -> [Group] -> [Group]
normalJoin ps gen gs =
        addMember (head bestsShuffled) : tail bestsShuffled ++ rests
    where
        bestFitness = maximum (map (fitness ps) gs)
        bests = filter (\ g -> fitness ps g == bestFitness) gs
        rests = filter (\ g -> fitness ps g /= bestFitness) gs
        bestsShuffled = shuffle' bests (length bests) gen

selflessJoin :: RandomGen g => Params -> g -> [Group] -> [Group]
selflessJoin ps gen gs =
        case undamaged of
            [ ] -> normalJoin ps gen gs -- if there are none, do a normal join
            _   -> addMember (head undamaged) : tail undamaged ++ damaged
        where
            undamagedOrdered = filter (\ g -> fitness ps (addMember g) >= fitness ps g) gs
            damaged          = filter (\ g -> fitness ps (addMember g) <  fitness ps g) gs
            undamaged = shuffle' undamagedOrdered (length undamagedOrdered) gen

main :: IO ()
main = do
        shuffleGen <- getStdGen
        let chances = randomRs (0.0,1.0) shuffleGen :: [Double]
        print (sim (Params 1 0.5 0.5 5 10) shuffleGen chances)
