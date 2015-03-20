import Control.Exception (assert)
import qualified Data.Set as Set
import TensorNet

main :: IO ()
main = do assert (tn (TN 1 [1] [[5]] (Set.fromList []))) $ return []
          putStrLn $ show $ stochasticity ptn1 stoch1
          putStrLn $ show $ stochasticity ptn2 stoch2
          putStrLn $ show $ stochasticity ptn3 stoch3
          putStrLn $ show $ stochasticity ptn4 stoch4
          putStrLn $ show $ stochasticity ptn5 stoch5
          putStrLn "All tests succeeded"
       where
          ptn1 = (TN 4 [1,2,3,2] [[4], [4,3], [4,4,4], [3,3]] (closeEdges [(1,1,3,1), (2,1,3,2), (2,2,4,1)]))
          stoch1 = stochList [ [[]], [[1]], [[1],[2],[3]], [[1], [2]] ]
          ptn2 = (TN 2 [1,2] [[4], [4,3]] (closeEdges []))
          stoch2 = stochList [ [[]], [[1]] ]
          ptn3 = (TN 4 [2,2,3,3] [[4,3], [4,3], [4,4,4], [3,3,3]] (closeEdges [(1,1,3,1), (1,2,4,2), (2,1,3,2), (2,2,4,1)]))
          stoch3 = stochList [ [[2]], [[1]], [[1],[2],[3]], [[1],[2],[3]] ]
          ptn4 = (TN 4 [2,3,2,1] [[3,3],[4,4,4],[4,3],[4]] (closeEdges [(4,1,2,1), (3,1,2,2), (3,2,1,1)]))
          stoch4 = stochList [ [[1],[2]], [[1],[2],[3]], [[1]], [[]] ]
          ptn5 = (TN 2 [3,3] [[4,4,3], [4,4,4]] (closeEdges [(1,1,2,1), (1,2,2,2)]))
          stoch5 = stochList [ [[2]], [[1],[2],[3]] ]