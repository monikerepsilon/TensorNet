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
        (TN 3 [1,2,3] [[4], [3,3], [3,3,5]] (closeEdges []))]
stochLists = [(stochList [ [[]], [[1]] ]),
              (stochList [ [[]], [[1]], [[1],[2],[3]] ]),
              (stochList [ [[]], [[1]], [[1],[2],[3]], [[1], [2]] ]),
              (stochList [ [[2]], [[1]], [[1],[2],[3]], [[1],[2],[3]] ]),
              (stochList [ [[1],[2]], [[1],[2],[3]], [[1]], [[]] ]),
              (stochList [ [[2]], [[1],[2],[3]] ]),
              (stochList [ [[1],[2]], [[1],[2]]]),
              (stochList [ [[1]], [[1],[2],[3]] ]),
              (stochList [ [[]], [[1],[2],[3]] ]),
              (stochList [ [[]], [[1]], [[1,2]]])]
              
stochs = map (\(p, sl) -> stochToLStoch p sl) (zip ptns stochLists)
caseNum = 9
testCases = (zip3 (permutations [1..(getTNn (ptns!!caseNum) )]) (permuteTN (ptns!!caseNum)) (permutations (stochs!!caseNum)))

getTNn (TN n a s e) = n

runTest (perm, p, ls) = traceShow perm (lstoch2 p ls)

main :: IO ()
main = do assert (tn (TN 1 [1] [[5]] (Set.fromList []))) $ return []
          --mapM (putStrLn . show . (\(p, ls) -> lstoch p ls)) (zip ptns stochs)
          mapM (putStrLn . show . runTest) testCases
          putStrLn "All tests succeeded"