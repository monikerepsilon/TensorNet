import Control.Exception (assert)
import qualified Data.Set as Set
import TensorNet

main :: IO ()
main = do assert (tn (TN 1 [1] [[5]] (Set.fromList []))) $ return []
          putStrLn $ show $ stochasticity ptn1 stoch1
          putStrLn $ show $ stochasticity ptn2 stoch2
          putStrLn $ show $ stochasticity ptn3 stoch3
          putStrLn "All tests succeeded"
       where
          ptn1 = (TN 4 [1,2,3,2] [[4], [4,3], [4,4,4], [3,3]] (closeEdges [(1,1,3,1), (2,1,3,2), (2,2,4,1)]))
          stoch1 = stochList [ [[]], [[1]], [[1],[2],[3]], [[1], [2]] ]
          ptn2 = (TN 2 [1,2] [[4], [4,3]] (closeEdges []))
          stoch2 = stochList [ [[]], [[1]] ]
          ptn3 = (TN 4 [2,2,3,3] [[4,3], [4,3], [4,4,4], [3,3,3]] (closeEdges [(1,1,3,1), (1,2,4,2), (2,1,3,2), (2,2,4,1)]))
          stoch3 = stochList [ [[2]], [[1]], [[1],[2],[3]], [[1],[2],[3]] ]