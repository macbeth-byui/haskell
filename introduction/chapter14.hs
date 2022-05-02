import Text.Read (readMaybe)
import System.IO

printList :: Show a => [a] -> IO ()
printList l = do
    putStr "["
    printSequence l
    putStr "]\n"

printSequence :: Show a => [a] -> IO ()
printSequence [] = return ()
printSequence [x] = do
    putStr (show x)
printSequence (x:xs) = do
    putStr (show x)
    putStr ","
    printSequence xs

readInt :: IO (Maybe Integer)
readInt = 
    do
        value <- getLine 
        return (readMaybe value :: Maybe Integer)

type Tuple3 = (Integer, Integer, Integer)
readTuple :: IO (Maybe Tuple3)
readTuple = do
    v1 <- readInt 
    v2 <- readInt
    v3 <- readInt
    case (v1,v2,v3) of
        (Just r1, Just r2, Just r3) -> return (Just (r1, r2, r3))
        _                           -> return Nothing

multTableFile :: String -> Integer -> IO ()
multTableFile filename n = do
    fh <- openFile filename WriteMode
    writeMultiTableFile fh n [1..n] -- [1..n] represents multipliers
    hClose fh

writeMultiTableFile :: Handle -> Integer -> [Integer] -> IO ()
writeMultiTableFile _ _ [] = return ()
writeMultiTableFile fh n (x:xs) = do
    hPutStrLn fh (getRowString (map (*x) [1..n])) -- [x*1, x*2, ... x*n]
    writeMultiTableFile fh n xs

getRowString :: [Integer] -> String
getRowString [] = ""
getRowString (x:xs) = show x ++ "\t" ++ getRowString xs

countFile :: FilePath -> IO Integer
countFile filename = do
    fh <- openFile filename ReadMode
    count <- countFileLine fh
    hClose fh
    return count

countFileLine :: Handle -> IO Integer
countFileLine fh = do
    eof <- hIsEOF fh
    if eof 
        then return 0
        else do
            hGetLine fh
            count <- countFileLine fh
            return (count + 1)
            
copyFile :: FilePath -> FilePath -> IO ()
copyFile source target = do
    content <- readFile source
    writeFile target content

