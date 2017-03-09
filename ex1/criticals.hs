import Data.Graph
import Data.Tree
import Data.Map.Strict (empty, (!), insert, insertWith)
import Data.List ((\\))
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

--assignNum :: Tree a -> Tree (a, Int)
assignNum = fst . aux 1
  where aux :: Int -> Tree a -> (Tree (a, Int), Int)
        aux id (Node x ts) = (Node (x, id) ts', id')
          where (ts', id') = auxs (id + 1) ts
        auxs :: Int -> [Tree a] -> ([Tree (a, Int)], Int)
        auxs id [] = ([], id)
        auxs id (t : ts) = (t' : ts', id'')
          where (t', id') = aux id t
                (ts', id'') = auxs id' ts

--getGraph :: [(Int,Int)] -> Map Int [Int]
getGraph [] = empty
getGraph ((x,y):edges) = insertWith (++) x [y] (getGraph edges)


criticals (Node (_, _, _) []) = 0

criticals (Node (1, num, low) ws) = foldr ((+) . criticals) init ws
	where init = if length ws > 1 then 1 else 0

criticals (Node (v, num, low) ws) = foldr ((+) . criticals) init ws
	where init = if any ((>= num) . getLow) ws then 1 else 0


--assignLow :: Graph -> Map Int Int -> Int -> Tree (a, Int) -> Tree (a, Int, Int)
assignLow graph nums vp (Node (v, num) []) = let backwards = getBackEdges graph v vp []
                                                 backward = minimum (num : map (nums!) backwards)
                                                 low = min num backward
                                             in Node (v, num, low) []

assignLow graph nums vp (Node (v, num) ws) = let ws' = map (assignLow graph nums v) ws
                                                 forward = minimum (map getLow ws')
                                                 backwards = getBackEdges graph v vp ws
                                                 backward = minimum (num : map (nums!) backwards)
                                                 low = min num (min forward backward)
                                             in Node (v, num, low) ws'


getLow (Node (_, _, low) _) = low
getV (Node (v, _) _) = v

getBackEdges graph v vp ws = (graph!v) \\ (vp : map getV ws)

makeNums [] = empty
makeNums ((v, num):list) = insert v num (makeNums list)

--input
readInt s = L.readInt (L.dropWhile isSpace s)

readMany s = case readInt s of
	Just (x1, r) -> let Just (x2, r') = readInt r; (xs, t) = readMany r' in ((x1, x2) : (x2, x1) : xs, t)
  	Nothing -> ([], s)

main = do
	all <- L.getContents
	let Just (n, r1) = readInt all
	let Just (k, r2) = readInt r1
	let (edges, _)  = readMany r2
	let graph = getGraph edges
	let tree = (assignNum . head . dff) (buildG (1,n) edges)
	let nums = (makeNums . flatten) tree
	let treeFinal = assignLow graph nums 0 tree
	print $ criticals treeFinal
