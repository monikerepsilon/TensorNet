module TensorNet
( TN(..),
  tn,
  merge,
  stochasticity,
  ptn,
  update,
  updateAll,
  stochList,
  closeEdges,
  lstoch,
  stochToLStoch
)

where

import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Control.Exception (assert)
import Debug.Trace

data TN = TN Int [Int] [[Int]] (Set.Set (Int, Int, Int, Int)) deriving Show

tn :: TN -> Bool
tn (TN 0 [] [] e) = Set.null e
tn (TN n a s e) | not valid = False -- domain of inputs
                | tn (TN (n-1) (tail a) (tail s) e) = True -- removing a node
                | (Set.size e) >= 2 && let (v1,w1,v2,w2) = Set.elemAt 0 e -- removing an edge
                    in any (== v1) [1..n] && any (== v2) [1..n] && any (== w1) [1..a!!(v1-1)] && any (== w2) [1..a!!(v2-1)] && v1 /= v2
                    && Set.member (v2,w2,v1,w1) e
                    && (length [(v1',w1',v2',w2')|(v1',w1',v2',w2') <- (Set.elems e), (v1' == v1 && w1' == w1) || (v2' == v2 && w2' == w2)]) == 1
                    && s!!(v1-1)!!(w1-1) == s!!(v2-1)!!(w2-1)
                    && tn (TN n a s (Set.delete (v1,w1,v2,w2) (Set.delete (v2,w2,v1,w1) e) ) ) = True
                | otherwise = False
                where valid = n > 0 && length s == n && length a == n && map length s == a && all (all (>0)) s
                
update :: Int -> Set.Set Int -> Int -> Int
update w s offset = w - Set.size s' + offset
    where (s', _) = Set.split w s

updateAll :: Set.Set Int -> Set.Set Int -> Int -> Set.Set Int
updateAll ways s offset = Set.map (\w -> update w s offset) ways
    
merge :: TN -> Int -> Int -> TN
merge (TN n a s e) v1 v2 = assert valid $
            let newArity1 = a!!(v1 - 1) - Set.size (Set.filter (\(v1',_,v2',_) -> (v1'==v1 && v2'==v2) ) $ e)
                newArity2 = a!!(v2 - 1) - Set.size (Set.filter (\(v1',_,v2',_) -> (v1'==v2 && v2'==v1) ) $ e)
                a' = (take (v1-1) a) ++ [newArity1 + newArity2] ++ (take (v2 - v1 - 1) (drop v1 a)) ++ (drop v2 a)
                ways1 = Set.map (\(_,w,_,_) -> w) (Set.filter (\(v1',_,v2',_) -> v1'==v1 && v2'==v2) $ e)
                ways2 = Set.map (\(_,w,_,_) -> w) (Set.filter (\(v1',_,v2',_) -> v1'==v2 && v2'==v1) $ e)
                newSize = [sz | (sz,i) <- (zip (s!!(v1-1)) [1..]), Set.notMember i ways1] ++ [sz | (sz,i) <- (zip (s!!(v2-1)) [1..]), Set.notMember i ways2]
                s' = (take (v1-1) s) ++ [newSize] ++ (take (v2 - v1 - 1) (drop v1 s)) ++ (drop v2 s)
                eUnaffected = Set.filter (\(v1',_,v2',_) -> v1' /= v1 && v1' /= v2 && v2' /= v1 && v2' /= v2) $ e
                eUpdated1 = Set.union
                    (Set.map (\(_,w1,v2',w2) -> (v1, update w1 ways1 0, v2', w2) ) (Set.filter (\(v1',w1,v2',w2) -> v1' == v1 && v2' /= v2) $ e)) -- v1 in first position
                    (Set.map (\(v1',w1,_,w2) -> (v1', w1, v1, update w2 ways1 0) ) (Set.filter (\(v1',w1,v2',w2) -> v1' /= v2 && v2' == v1) $ e)) -- v1 in second position
                eUpdated2 = Set.union
                    (Set.map (\(_,w1,v2',w2) -> (v2, update w1 ways2 newArity1, v2', w2) ) (Set.filter (\(v1',w1,v2',w2) -> v1' == v2 && v2' /= v1) $ e)) -- v2 in first position
                    (Set.map (\(v1',w1,_,w2) -> (v1', w1, v2, update w2 ways2 newArity1) ) (Set.filter (\(v1',w1,v2',w2) -> v1' /= v1 && v2' == v2) $ e)) -- v2 in second position
                prunedEdges = Set.union eUnaffected (Set.union eUpdated1 eUpdated2)
                updateV2 = (Set.map (\(v1',w1,v2',w2) -> if v1' == v2 then (v1,w1,v2',w2) else if v2'==v2 then (v1',w1,v1,w2) else (v1',w1,v2',w2)) ) $ prunedEdges -- update v2's edges to v1's id
                e' = (Set.map (\(v1', w1, v2', w2) -> (if v1' > v2 then v1' - 1 else v1', w1, if v2' > v2 then v2' - 1 else v2', w2)) ) $ updateV2 -- update vertex id's for vertices coming after v2
                out = TN (n-1) a' s' e'
            in out --assert tn out $ out
    where valid = tn (TN n a s e) && any (== v1) [1..n] && any (== v2) [1..n] && v1 < v2
    
stochasticity :: TN -> [Set.Set (Set.Set Int)] -> Set.Set (Set.Set Int)
stochasticity (TN 1 arity sz e) [stoch] = assert (tn (TN 1 arity sz e)) $ stoch
stochasticity (TN n arity sz e) stoch = let t = Set.map (\(_,w,_,_) -> w) (Set.filter (\(v1,_,v2,_) -> v1==1 && v2==2) $ e) -- contraction ways for vertex 1
                                            v = Set.map (\(_,w,_,_) -> w) (Set.filter (\(v1,_,v2,_) -> v1==2 && v2==1) $ e) -- contraction ways for vertex 2
                                            aIdxSet = Set.fromList [1..arity!!0]
                                            bIdxSet = Set.fromList [1..arity!!1]
                                            cIdxSet = Set.fromList [1..(arity!!0 + arity!!1 - 2 * Set.size t)]
                                            sPairs = [(Set.difference aIdxSet sa, Set.difference bIdxSet sb) | sa <- Set.toList (stoch!!0), sb <- Set.toList (stoch!!1),
                                                                (Set.null (Set.intersection sa t) || Set.null (Set.intersection sb v))]
                                                                                       
                                            mergedStoch = Set.fromList [Set.difference cIdxSet (Set.union (updateAll sa t 0)
                                                                                  (updateAll sb v ((arity!!0) - Set.size t)))
                                                                                  | (sa,sb) <- sPairs]
                                            restStoch = (drop 2 stoch)
                                        in  assert valid $ (stochasticity (merge struct 1 2) (mergedStoch:restStoch))
                                        where struct = (TN n arity sz e)
                                              valid = n >= 2 && tn struct && (all (\(a, s) -> Set.fold (\s1 s2 -> (Set.fold (\s3 s4 -> ((s3 <= a && s3 > 0) && s4)) True s1) && s2 ) True s) (zip arity stoch) ) -- valid TN, stoch constraints respect arity at each node

applyToSet :: (Int -> Int) -> ((Set.Set Int) -> (Set.Set Int))
applyToSet f = Set.map f

contractionList :: [Int] -> [Int] -> [Int] -> Int -> [Int]
contractionList [] next t offset = []
contractionList (r:range) next t offset =   if (elem r t)
                                            then (((fromJust $ elemIndex r t) + offset):(contractionList range next t offset))
                                            else ((head next):(contractionList range (tail next) t offset))
 
contractionMap :: [Int] -> [Int] -> Int -> ((Set.Set Int) -> (Set.Set Int))
contractionMap range t offset = applyToSet ((!!) (contractionList range range t offset) . (+ (-1)))
 
lstoch :: TN -> [Set.Set (Set.Set Int, Set.Set Int)] -> Set.Set(Set.Set Int, Set.Set Int)
lstoch (TN 1 arity sz e) [ls] = assert (tn (TN 1 arity sz e)) $ ls
lstoch (TN nv arity sz e) ls = let cidx = Set.toList (Set.map (\(_,w1,_,w2) -> (w1,w2)) (Set.filter (\(v1,_,v2,_) -> v1==1 && v2==2) $ e)) -- contraction ways
                                   t = [w | (w,_) <- cidx]
                                   v = [w | (_,w) <- cidx]
                                   m = arity!!0
                                   n = arity!!1
                                   l = length t
                                   tSet = Set.fromList t
                                   vSet = Set.fromList v
                                   alpha = contractionMap [1..m] t (1 + m + n - 2 * l)
                                   beta = contractionMap [(m - l + 1)..(m - l + 1 + n)] [x + m - l | x <- v] (1 + m + n - 2 * l)
                                   sPairs = [((sa,raBar), (sb,rbBar)) | (sa,ra) <- Set.toList (ls!!0), (sb,rb) <- Set.toList (ls!!1),
                                              let raBar = (Set.difference (Set.fromList [1..(m - Set.size sa)]) ra),
                                              let rbBar = (Set.difference (Set.fromList [1..(n - Set.size sb)]) rb),
                                              (((Set.null (Set.intersection sa (tSet))) && (Set.null (Set.intersection (beta sb) (alpha raBar)) ) )
                                                || ((Set.null (Set.intersection sb (vSet))) && (Set.null (Set.intersection (alpha sa) (beta rbBar)) ) ))
                                                && (Set.null (Set.intersection (alpha raBar) (beta rbBar)))]
                                   merged = Set.fromList [(sc, rc ) | ((sa,raBar), (sb,rbBar)) <- sPairs,
                                                                       let sc' = Set.union (alpha (Set.difference sa tSet)) (beta (Set.difference sb vSet)),
                                                                       let rcBar = (Set.union (alpha (Set.difference raBar tSet)) (beta (Set.difference rbBar vSet)) ),
                                                                       let sc = if rcBar == Set.fromList [1..m+n-2*l-(Set.size sc')]
                                                                                then Set.empty
                                                                                else sc',
                                                                       let rc = if Set.null sc
                                                                                then Set.empty
                                                                                else (Set.difference (Set.fromList [1..(m+n-2*l-(Set.size sc))]) rcBar)]
                                   rest = (drop 2 ls)
                                   in assert valid $ (lstoch (merge struct 1 2) (merged:rest))
                                   where struct = (TN nv arity sz e)
                                         valid = nv >= 2 && tn struct
                                              
ptn :: TN -> [Set.Set (Set.Set Int)] -> Bool
ptn struct stoch = (stochasticity struct stoch) == Set.fromList [Set.fromList []]

closeEdges :: [(Int, Int, Int, Int)] -> Set.Set (Int, Int, Int, Int)
closeEdges e = Set.union (Set.fromList [(v2,w2,v1,w1) | (v1,w1,v2,w2) <- e]) (Set.fromList e)

stochToLStoch :: TN -> [Set.Set (Set.Set Int)] -> [Set.Set (Set.Set Int, Set.Set Int)]
stochToLStoch (TN n a s e) stoch = [Set.map (\x -> (if Set.null x then Set.empty else Set.difference (Set.fromList [1..(a!!i)]) x, Set.fromList [1..(Set.size x)])) st | (st,i) <- zip stoch [0..]]

stochList :: [[[Int]]] -> [Set.Set (Set.Set Int)]
stochList s = [Set.fromList [Set.fromList stoch | stoch <- ss] | ss <- s]