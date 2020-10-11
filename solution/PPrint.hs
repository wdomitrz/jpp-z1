module PPrint where
import           Data.List                      ( intersperse )
writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k, v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS $ showString "\n"
pprH = intercalateS $ showString " "

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep = flip (foldr ($)) . intersperse sep

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith = (pprV .) . map

runShows :: ShowS -> IO ()
runShows = putStrLn . ($ "")
