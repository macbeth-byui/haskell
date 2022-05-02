module FileUtility where

import System.IO

countFile :: FilePath -> IO Integer
countFile source = do
    fh <- openFile source ReadMode
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