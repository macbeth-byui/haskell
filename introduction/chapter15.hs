import System.Environment
import FileUtility

main :: IO()
main = do
    args <- getArgs
    case args of
        [file] -> do
            count <- FileUtility.countFile file
            putStrLn ("Lines = " ++ show count)
        [file1, file2] -> do
            FileUtility.copyFile file1 file2
            putStrLn "Done"
        _ -> do
            putStrLn "Invalid option."
