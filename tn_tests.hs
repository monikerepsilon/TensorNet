import Control.Exception (assert)
import qualified Data.Set as Set
import TensorNet
import Data.List
import Debug.Trace

ptns = [(TN 2 [1,2] [[4], [4,3]] (closeEdges [])),
        (TN 3 [1,2,3] [[4], [4,3], [4,4,4]] (closeEdges [(1,1,3,1), (2,1,3,2)])),
        (TN 4 [1,2,3,2] [[4], [4,3], [4,4,4], [3,3]] (closeEdges [(1,1,3,1), (2,1,3,2), (2,2,4,1)])),
        (TN 4 [2,2,3,3] [[4,3], [4,3], [4,4,4], [3,3,3]] (closeEdges [(1,1,3,1), (1,2,4,2), (2,1,3,2), (2,2,4,1)])),
        (TN 4 [2,3,2,1] [[3,3],[4,4,4],[4,3],[4]] (closeEdges [(4,1,2,1), (3,1,2,2), (3,2,1,1)])),
        (TN 2 [3,3] [[4,4,3], [4,4,4]] (closeEdges [(1,1,2,1), (1,2,2,2)])),
        (TN 2 [2,2] [[4,4],[4,4]] (closeEdges [])),
        (TN 2 [2,3] [[4,3], [4,4,4]] (closeEdges [(1,1,2,2)])),
        (TN 2 [1,3] [[4], [4,4,4]] (closeEdges [(1,1,2,1)])),
        (TN 3 [1,2,3] [[4], [3,3], [3,3,5]] (closeEdges [])),
        (TN 11 [1, 2, 3, 3, 3, 2, 3, 2, 4, 2, 2] [[3], [3,4], [4, 2, 5], [4, 2, 3], [3, 6, 6], [2, 5], [5, 4, 3], [4, 2], [2, 3, 4, 5], [3, 2], [4, 3]]
               (closeEdges [(1,1,2,1), (3,1,4,1), (3,2,4,2), (4,3,5,1), (6,2,7,1), (7,2,8,1), (8,2,9,1), (9,2,10,1), (9,3,11,1)])),
        (TN 11 [1, 2, 3, 3, 3, 2, 3, 2, 4, 2, 2] [[3], [3,4], [4, 2, 5], [4, 2, 3], [3, 6, 6], [2, 5], [5, 4, 3], [4, 2], [2, 3, 4, 5], [3, 2], [4, 3]]
               (closeEdges [(1,1,2,1), (3,1,4,1), (3,2,4,2), (4,3,5,1), (6,2,7,1), (7,2,8,1), (8,2,9,1), (9,2,10,1), (9,3,11,1)])) ]
stochLists = [(stochList [ [[]], [[1]] ]),
              (stochList [ [[]], [[1]], [[1],[2],[3]] ]),
              (stochList [ [[]], [[1]], [[1]], [[1]] ]),
              (stochList [ [[2]], [[1]], [[1],[2],[3]], [[1],[2],[3]] ]),
              (stochList [ [[1],[2]], [[1],[2],[3]], [[1]], [[]] ]),
              (stochList [ [[2]], [[1],[2],[3]] ]),
              (stochList [ [[1],[2]], [[1],[2]]]),
              (stochList [ [[1]], [[1],[2],[3]] ]),
              (stochList [ [[]], [[1],[2],[3]] ]),
              (stochList [ [[]], [[1]], [[1,2]]]),
              (stochList [ [[]], [[1]], [[1,2]], [[3]], [[]], [[]], [[1,2]], [[]], [[1,2,3]], [[]], [[1]] ] ),
              (stochList [ [[]], [[1]], [[1,2]], [[3]], [[]], [[]], [[1]], [[1]], [[1]], [[1]], [[1]] ] )]
              
stochs = map (\(p, sl) -> stochToLStoch p sl) (zip ptns stochLists)
caseNum = 9
allPerms = (permutations [1..(getTNn (ptns!!caseNum) )])
testCases = (zip3 allPerms (permuteTN (ptns!!caseNum)) (permutations (stochs!!caseNum)))

getTNn (TN n a s e) = n

runTest :: ([Int], TN, [Set.Set (Set.Set Int, Set.Set Int, Set.Set Int)]) -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int)
runTest (perm, p, ls) = permuteLS perm (lstoch2 p ls)

permuteLS :: [Int] -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int) -> Set.Set (Set.Set Int, Set.Set Int, Set.Set Int)
permuteLS perm ls = Set.map (\(s,r,st) -> 
                      let s' = s
                          r' = r
                          st' = st
                      in (s',r',st'))
                    ls

main :: IO ()
main = do assert (tn (TN 1 [1] [[5]] (Set.fromList []))) $ return []
          --mapM (putStrLn . show . (\(p, ls) -> lstoch p ls)) (zip ptns stochs)
          let results = map (runTest) testCases
              expected = (lstoch2 (ptns!!caseNum) (stochs!!caseNum))
                in do putStrLn ("Expected: " ++ (show expected))
                      if all (== expected) results
                      then putStrLn "All tests succeeded"
                      else mapM_ (putStrLn) (["Something failed: "] ++ [(show (allPerms!!i)) ++ "\n" ++ (show (results!!i)) | i <- [0..((length allPerms)-1)], expected /= (results!!i)])