import qualified Data.Set as S
import qualified Data.Vector as V
import System.Environment
import System.Exit
import System.IO
import Control.Applicative
import qualified Data.Hashable as DH

type BoolVec = V.Vector Bool
type HashList = [String -> Int -> Int]
data Stats = Stats { numTotal :: Int
                   , numFalsePos :: Int
                   , numFalseNeg :: Int
                   , numTruePos :: Int 
                   , numTrueNeg :: Int
                   } deriving (Show)


main :: IO ()
main = do 
        args <- getArgs
        case args of
           filename:(indexsize:(numhashes:queries)) -> do
                                file <- readFile filename
                                let salts = [1..(read numhashes::Int)]
                                    hashes = fmap (\x -> (\word size -> mod (DH.hashWithSalt x word) size)) salts
                                    allwords = (words file)
                                    blankindex = (V.replicate (read indexsize::Int) False)
                                    index = addAllToIndex hashes blankindex allwords
                                    inhashes = fmap (queryIndex hashes index) queries
                                    insets = fmap (flip S.member $ S.fromList allwords) queries
                                sequence $ zipWith3 printQueryResult insets inhashes queries
                                putStrLn ""
                                printStats $ foldl (\acc f -> f acc) (Stats 0 0 0 0 0) (zipWith3 updateStats insets inhashes queries)
           otherwise -> do
                hPutStrLn stderr "Usage: bloom-filter <filename> <index size> <number of hashes> [<query>..]"
                exitWith $ ExitFailure 1

addToIndex :: HashList -> BoolVec -> String -> BoolVec
addToIndex hashes index word = let addhash = (\ind hash -> ind V.// [(hash,True)])
                                   hashvals = (hashes <*> [word] <*> [(V.length index)])
                               in foldl addhash index hashvals

addAllToIndex :: HashList -> BoolVec -> [String] -> BoolVec
addAllToIndex hashes index words = foldl (addToIndex hashes) index words

queryIndex :: HashList -> BoolVec -> String -> Bool
queryIndex hashes index query = let hashvals = (hashes <*> [query] <*> [(V.length index)])
                                in and $ fmap (\x -> index V.! x) hashvals

printQueryResult :: Bool -> Bool -> String -> IO ()
printQueryResult inset inhash query = do case (inset,inhash) of
                                           (True,True) -> putStrLn $ "True positive: \"" ++ query ++ "\" is there."
                                           (True,False) -> putStrLn $ "False negative: \"" ++ query ++ "\" is there."
                                           (False,True) -> putStrLn $ "False positive: \"" ++ query ++ "\" is not there."
                                           (False,False) -> putStrLn $ "True negative: \"" ++ query ++ "\" is not there."

updateStats :: Bool -> Bool -> String -> Stats -> Stats
updateStats inset inhash query stats = let stats' = stats {numTotal = 1 + numTotal stats}
                                       in case (inset,inhash) of
                                           (True,True) -> stats' {numTruePos = 1 + numTruePos stats}
                                           (True,False) -> stats' {numFalseNeg = 1 + numFalsePos stats}
                                           (False,True) -> stats' {numFalsePos = 1 + numFalseNeg stats}
                                           (False,False) -> stats' {numTrueNeg = 1 + numTrueNeg stats}

printStats :: Stats -> IO ()
printStats (Stats t fp fn tp tn) = do putStrLn $ "Total number of queries = " ++ show t ++ " ("
                                                                              ++ show tp ++ " hits, and "
                                                                              ++ show tn ++ " misses)"
                                      putStrLn $ "Error rate = " 
                                                 ++ show (fromIntegral (fp+fn) / fromIntegral t) ++ " ("
                                                 ++ show (fromIntegral (100 * fp) / fromIntegral t) ++ "% false positives, and "
                                                 ++ show (fromIntegral (100 * fn) / fromIntegral t) ++ "% false negatives)"
