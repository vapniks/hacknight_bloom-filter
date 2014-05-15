-- Implementation of a bloom filter in haskell
-- Usage: runhaskell bloom-filter.hs <filename> <index size> <number of hashes> [<query>..]

import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import System.Environment
import System.Exit
import System.IO
import Control.Applicative
import qualified Data.Hashable as DH

type BoolVec = V.Vector Bool
type HashList = [String -> Int -> Int]
data Stats = Stats { numFalsePos :: Int, numFalseNeg :: Int, numTruePos :: Int , numTrueNeg :: Int} deriving (Show)

main :: IO ()
main = do 
        args <- getArgs
        case args of
           filename:(indexsize:(numhashes:queries)) -> do
                                file <- readFile filename
                                let salts = [1..(read numhashes::Int)]
                                    hashes = fmap (\x -> (\word size -> mod (DH.hashWithSalt x word) size)) salts
                                    allwords = S.fromList (words file)
                                    blankindex = (V.replicate (read indexsize::Int) False)
                                    index = addAllToIndex hashes blankindex allwords
                                    inhashes = fmap (queryIndex hashes index) queries
                                    insets = fmap (flip S.member allwords) queries
                                sequence $ zipWith3 printQueryResult insets inhashes queries
                                putStrLn ""
                                printStats $ foldl (\acc f -> f acc) (Stats 0 0 0 0)
                                                                     (zipWith3 updateStats insets inhashes queries)
           otherwise -> do hPutStrLn stderr "Usage: bloom-filter <filename> <index size> <number of hashes> [<query>..]"
                           exitWith $ ExitFailure 1

-- This is the bottleneck (more specifically the V.// operation)
addToIndex :: HashList -> String -> BoolVec -> BoolVec
addToIndex hashes word index = let hashvals = (hashes <*> [word] <*> [(V.length index)])
                                   in index V.// zip hashvals (repeat True)

addAllToIndex :: HashList -> BoolVec -> S.Set String -> BoolVec
addAllToIndex hashes index words = S.fold (addToIndex hashes) index words

queryIndex :: HashList -> BoolVec -> String -> Bool
queryIndex hashes index query = let hashvals = (hashes <*> [query] <*> [(V.length index)])
                                in and $ fmap (\x -> index V.! x) hashvals

printQueryResult :: Bool -> Bool -> String -> IO ()
printQueryResult inset inhash query = do case (inset,inhash) of
                                           (True,True) -> putStrLn $ "True positive: " ++ query
                                           (True,False) -> putStrLn $ "False negative: " ++ query
                                           (False,True) -> putStrLn $ "False positive: " ++ query
                                           (False,False) -> putStrLn $ "True negative: " ++ query

updateStats :: Bool -> Bool -> String -> Stats -> Stats
updateStats inset inhash query stats = case (inset,inhash) of
                                         (True,True) -> stats {numTruePos = 1 + numTruePos stats}
                                         (True,False) -> stats {numFalseNeg = 1 + numFalsePos stats}
                                         (False,True) -> stats {numFalsePos = 1 + numFalseNeg stats}
                                         (False,False) -> stats {numTrueNeg = 1 + numTrueNeg stats}

printStats :: Stats -> IO ()
printStats (Stats fp fn tp tn) = do let t = fromIntegral (tp + tn)
                                    putStrLn $ "Total number of queries = " ++ show t ++ " (" 
                                                ++ show tp ++ " hits, and " ++ show tn ++ " misses)"
                                    putStrLn $ "Error rate = " 
                                               ++ show (fromIntegral (fp+fn) / t) ++ " ("
                                               ++ show (fromIntegral (100 * fp) / t) ++ "% false positives, and "
                                               ++ show (fromIntegral (100 * fn) / t) ++ "% false negatives)"
