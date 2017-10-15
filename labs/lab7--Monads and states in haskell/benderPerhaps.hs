import Data.Map (Map)
import qualified Data.Map as Map

-- In this code, we model bender moving around, but
-- if he finds beer, he will stop responding to commands.
type Pos = (Integer, Integer)

x -: f = f x

start = (0,0)

badPos = Map.empty
  -: Map.insert (0,2) True
  -: Map.insert (-1,3) True
  -: Map.insert (-3,-8) True


moveTo :: Pos -> Maybe Pos
moveTo p =
  if Map.member p badPos
    then Nothing
    else Just p

up (x, y)    = moveTo (x, y+1)
down (x, y)  = moveTo (x, y-1)
left (x, y)  = moveTo (x-1, y)
right (x, y) = moveTo (x+1, y)

-- Our directions now result in Maybe Pos values, so we can't chain them with "-:" anymore.
test1 = return start >>= up >>= right
test2 = return start >>= up >>= left >>= left >>= right >>= down
test3 = return start >>= left >>= left >>= up >>= up >>= right >>= up >>= right >>= right >>= down


