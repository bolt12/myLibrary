import Text.Regex.PCRE
import Text.Printf (printf)

getSignatures :: [String] -> IO [String]
getSignatures = return . filter (\x -> x =~ "::")

writeHeader :: String -> IO ()
writeHeader = writeFile "README.md" . printf "# %s\n"

appendHeader :: String -> IO ()
appendHeader = appendFile "README.md" . printf "# %s\n"

appendDescription :: IO ()
appendDescription = appendFile "README.md" "A not so naive implementation attempt of some of the pre defined functions in Haskell's Prelude and more.\n"

main :: IO ()
main = do
        codeFile <- readFile "MyPrelude.hs"
        signatures <- getSignatures $ lines codeFile
        writeHeader "myPrelude"
        appendDescription
        appendHeader "List of functions implemented"
        mapM_ (appendFile "README.md" . printf "- `%s`\n") signatures

