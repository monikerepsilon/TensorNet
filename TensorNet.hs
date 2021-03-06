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
  lstoch2,
  stochToLStoch,
  permuteTN
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

permuteTN :: TN -> [TN]
permuteTN (TN n a s e) = map (\(n',a',s',e') -> TN n' a' s' e') [(n, [a!!p' | p' <- p], [s!!p' | p' <- p], e') | p <- permutations [0..(n-1)],
                                                                 let e' = Set.map (\(v1,w1,v2,w2) -> ((fromJust $ elemIndex (v1-1) p) + 1,w1,(fromJust $ elemIndex (v2-1) p) + 1,w2)) e]
                
update :: Int -> Set.Set Int -> Int -> Int
update w s offset = w - Set.size s' + offset
    where (s', _) = Set.split w s

updateInv :: Int -> Set.Set Int -> Int -> Int
updateInv w s offset = head [w' | w' <- [(w-offset)..(w-offset+(Set.size s))], Set.notMember w' s, (update w' s offset) == w]

applyToSet :: (Int -> Int) -> ((Set.Set Int) -> (Set.Set Int))
applyToSet f = Set.map f
    
updateAll :: Set.Set Int -> Set.Set Int -> Int -> Set.Set Int
updateAll ways s offset = Set.map (\w -> update w s offset) (Set.difference ways s)

updateAllInv :: Set.Set Int -> Set.Set Int -> Int -> Set.Set Int
updateAllInv ways s offset = Set.map (\w -> updateInv w s offset) ways

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
                                   sPairs = [((sa,ra), (sb,rb)) | (sa,ra) <- Set.toList (ls!!0), (sb,rb) <- Set.toList (ls!!1),
                                              let raBar = alpha (Set.difference (Set.difference (Set.fromList [1..m]) (updateAllInv ra sa 0)) sa),
                                              let rbBar = beta (Set.difference (Set.difference (Set.fromList [1..n]) (updateAllInv rb sb 0)) sb),
                                              trace ("pairs = " ++ show ((sa,ra),(sb,rb))) True,
                                              trace ("rbbar = " ++ show rbBar) True,
                                              traceShowId (((Set.null (Set.intersection sa (tSet))) && (Set.null (Set.intersection (beta sb) raBar) ) )
                                                || ((Set.null (Set.intersection sb (vSet))) && (Set.null (Set.intersection (alpha sa) rbBar) ) ))
                                                && traceShowId (Set.null (Set.intersection raBar rbBar)) -- A and B must be independent of each others' remaining contraction indices
                                                 && traceShowId (Set.isSubsetOf (Set.difference (alpha tSet) (Set.union (alpha sa) (beta sb))) (Set.union raBar rbBar)) -- remaining contraction indices must only be over non-redundant blocks (else our sum is >1)
                                                ]
                                   merged = Set.fromList [(sc, rc ) | ((sa,ra), (sb,rb)) <- sPairs,
                                                                       let sc' = Set.union (alpha (Set.difference sa tSet)) (beta (Set.difference sb vSet)),
                                                                       let rc  = (Set.union (updateAll (alpha (Set.difference (updateAllInv ra sa 0) tSet)) sc' 0) (updateAll (beta (Set.difference (updateAllInv rb sb 0) vSet)) sc' 0) ),
                                                                       let sc = if Set.null rc
                                                                                then Set.empty
                                                                                else sc']
                                                                       --let rc = if Set.null sc
                                                                       --         then Set.empty
                                                                       --         else (Set.difference (Set.fromList [1..(m+n-2*l-(Set.size sc))]) rcBar)]
                                   rest = (drop 2 ls)
                                   in assert valid $ (lstoch (merge struct 1 2) (merged:rest))
                                   where struct = (TN nv arity sz e)
                                         valid = nv >= 2 && tn struct

findPath :: Set.Set (Set.Set Int, Set.Set Int, Set.Set Int) -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int) -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int) -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int) -> Set.Set Int -> Set.Set Int -> Set.Set Int-> Set.Set Int -> Set.Set Int -> Set.Set Int -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int)
findPath lsA lsRemA lsB lsRemB t sA rA sB rB curStoch
    | (Set.null lsA) && (Set.null lsB) = Set.empty
    | otherwise = 
        let mS            = Set.union (Set.union sA sB) s
            mR            = Set.union (Set.union rA rB) r
            mSt           = Set.union curStoch stoch
            curRule       = (Set.isSubsetOf t (Set.union mSt mS)) && (Set.isSubsetOf t mR)
            useA          = not (Set.null lsA)
            (s, r, stoch) = if useA
                            then Set.elemAt 0 lsA
                            else Set.elemAt 0 lsB
            lsA'          = if useA
                            then Set.deleteAt 0 lsA
                            else lsA
            lsB'          = if useA
                            then lsB
                            else Set.deleteAt 0 lsB
            sA'           = if useA
                            then (Set.union sA s)
                            else sA
            rA'           = if useA
                            then (Set.union rA r)
                            else rA
            sB'           = if useA
                            then sB
                            else (Set.union sB s)
            rB'           = if useA
                            then rB
                            else (Set.union rB r)
            nextRule      = if useA -- Can't use rule if it contains a contraction index that hasn't been removed from OTHER tensor
                            then ((Set.null (Set.intersection s (Set.difference t rB)))
                                && (Set.null (Set.intersection r sA))
                                && (Set.null (Set.intersection s rA))
                                && (Set.null (Set.intersection r rB)))
                            else ((Set.null (Set.intersection s (Set.difference t rA)))
                                && (Set.null (Set.intersection r sB))
                                && (Set.null (Set.intersection s rB))
                                && (Set.null (Set.intersection r rA)))
            children      = if nextRule -- can add next rule; add back removed rules since they might now be applicable
                                then findPath (Set.union lsA' lsRemA) Set.empty (Set.union lsB' lsRemB) Set.empty t sA' rA' sB' rB' mSt
                                else Set.empty
            rest          = if useA -- or, *not* using the rule
                                then findPath lsA' (Set.insert (s, r, stoch) lsRemA) lsB' lsRemB t sA rA sB rB curStoch
                                else findPath lsA' lsRemA lsB' (Set.insert (s, r, stoch) lsRemB) t sA rA sB rB curStoch
        in if   (nextRule && (curRule || (not (Set.null children)))) -- can add current rule?
            then Set.union rest
                 (if not (Set.null children)
                  then Set.union children
                       (Set.map (\(cs, cr, cst) -> (Set.difference mS t, Set.difference mR t, Set.difference (Set.difference (Set.union mSt (Set.union cs cst)) mS) t)) children)
                  else Set.singleton (Set.difference mS t, Set.difference mR t, (Set.difference (Set.difference mSt mS ) t)) )
                 
            else rest
                                                           
findLS :: Set.Set (Set.Set Int, Set.Set Int, Set.Set Int) -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int) -> Set.Set Int -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int)
findLS lsA lsB t =
    if (Set.null lsA) || (Set.null lsB)
    then Set.empty
    else findPath lsA Set.empty lsB Set.empty t Set.empty Set.empty Set.empty Set.empty Set.empty

normalizeLS :: Int -> Set.Set(Set.Set Int, Set.Set Int, Set.Set Int) -> Set.Set(Set.Set Int, Set.Set Int, Set.Set Int)
normalizeLS a ls = Set.map
    (\(s,r,st) ->   if Set.null r
                    then let s' = Set.union s st
                         in (s', Set.difference (Set.fromList [1..a]) s', Set.empty) -- empty redundancy is just stochasticity, so normalize
                    else (s,r,st)
    )
    ls
    --(Set.filter (\(s,r,st) -> Set.null (Set.intersection s r)) ls) -- remove any self-contradictory rules


    
lstoch2 :: TN -> [Set.Set (Set.Set Int, Set.Set Int, Set.Set Int)] -> Set.Set(Set.Set Int, Set.Set Int, Set.Set Int)
lstoch2 (TN 1 arity sz e) [ls] = assert (tn (TN 1 arity sz e)) $ ls
lstoch2 (TN nv arity sz e) ls = let cidx = Set.toList (Set.map (\(_,w1,_,w2) -> (w1,w2)) (Set.filter (\(v1,_,v2,_) -> v1==1 && v2==2) $ e)) -- contraction ways
                                    t = [w | (w,_) <- cidx]
                                    v = [w | (_,w) <- cidx]
                                    m = arity!!0
                                    n = arity!!1
                                    l = length t
                                    tSet = Set.fromList t
                                    vSet = Set.fromList v
                                    alpha = contractionMap [1..m] t (1 + m + n - 2 * l)
                                    beta = contractionMap [(m - l + 1)..(m - l + 1 + n)] [x + m - l | x <- v] (1 + m + n - 2 * l)
                                    cSet = alpha tSet
                                    merged = normalizeLS (m + n - 2 * l) $ findLS (Set.map (\(s,r,st) -> (alpha s, alpha r, alpha st)) (ls!!0)) (Set.map (\(s,r,st) -> (beta s, beta r, beta st)) (ls!!1)) cSet
                                    rest = (drop 2 ls)
                                    in assert valid $
                                       (lstoch2 (merge struct 1 2) (merged:rest))
                                    where struct = (TN nv arity sz e)
                                          valid = nv >= 2 && tn struct
                                              
ptn :: TN -> [Set.Set (Set.Set Int)] -> Bool
ptn struct stoch = (stochasticity struct stoch) == Set.fromList [Set.fromList []]

closeEdges :: [(Int, Int, Int, Int)] -> Set.Set (Int, Int, Int, Int)
closeEdges e = Set.union (Set.fromList [(v2,w2,v1,w1) | (v1,w1,v2,w2) <- e]) (Set.fromList e)

stochToLStoch :: TN -> [Set.Set (Set.Set Int)] -> [Set.Set (Set.Set Int, Set.Set Int, Set.Set Int)]
stochToLStoch (TN n a s e) stoch = [Set.map (\x -> (Set.difference (Set.fromList [1..(a!!i)]) x, x, Set.empty)) st | (st,i) <- zip stoch [0..]]

stochList :: [[[Int]]] -> [Set.Set (Set.Set Int)]
stochList s = [Set.fromList [Set.fromList stoch | stoch <- ss] | ss <- s]