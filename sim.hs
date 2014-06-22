import Data.List (sort)
import System.Random.Shuffle (shuffle')
import System.Random (getStdGen, RandomGen, randomRs)

data Params = Params {
    memberContrib   :: Double,
    memberDetriment :: Double,
    selflessness    :: Double,
    splitChance     :: Double,
    numJoiners      :: Integer,
    numGroups       :: Integer
}

optimalSize :: Params -> Integer
optimalSize (Params a b _ _ _ _) = floor (a / b)

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
fitness (Params a b _ _ _ _) g =
        a * fromIntegral (size g) - (b / 2) * ( fromIntegral (size g) ^ (2 :: Integer) )

-- find all groups with best fitness, randomly pick one and join it
-- sometimes, we feel a bit selfless, so we don't join a group when we will
-- decrease the fitness of the group
oneJoin :: RandomGen g => Params -> g -> [Double] -> [Group] -> [Group]
oneJoin ps@(Params _ _ s _ _ _) gen (chance:_) gs =
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

-- some monadic stuff, make's my headache go away a bit
splitGroups :: RandomGen g => Params -> g -> [Double] -> [Group] -> Int -> [Group]
splitGroups ps gen chances gs i =
        if (head cs) <= (splitChance ps) && size g > optimalSize ps
            then
                if i == 0
                    then modified
                    else splitGroups ps gen cs modified (i-1)
            else
                if i == 0
                    then gs
                    else splitGroups ps gen cs gs (i-1)
        where
            cs = tail chances
            g = gs !! i
            keepers = div g 2
            leavers = g - keepers
            almostModified = take (i) gs ++ drop (i+1) gs
            modified = keepers : simLoop ps gen cs almostModified leavers

simLoop :: RandomGen g => Params -> g -> [Double] -> [Group] -> Integer -> [Group]
simLoop ps gen givenChances gs remainingJoiners =
        if remainingJoiners == 1
            then res
            else simLoop ps gen chances res (remainingJoiners - 1)
        where
            chances = tail givenChances
            res = splitGroups ps gen (tail chances) (oneJoin ps gen chances gs) (length gs - 1)

trial ps gen chances = simLoop ps gen chances gs (numJoiners ps)
    where gs = take (fromIntegral (numGroups ps)) emptyGroups

main :: IO ()
main = do
        let ps = Params {
            memberContrib   = 1.0,
            memberDetriment = 0.5,
            selflessness    = 0.5,
            splitChance     = 0.5,
            numJoiners      = 7,
            numGroups       = 10
        }

        shuffleGen <- getStdGen
        let chances = randomRs (0.0,1.0) shuffleGen :: [Double]

        let result = trial ps shuffleGen chances
        print (sort result)
        print (length result)
