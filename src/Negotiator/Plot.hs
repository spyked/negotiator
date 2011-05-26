module Negotiator.Plot where

cleanFile :: FilePath -> IO ()
cleanFile path = writeFile path ""

writeOptions :: FilePath -> IO ()
writeOptions path = appendFile path "set key off\nset grid\nset ticslevel 0\n"

writePoints :: (Show a, Show b) => FilePath -> [(a,b)] -> IO ()
writePoints path ps = mapM_ writePoint ps
    where
    writePoint (x,y) = appendFile path $ show x ++ " " ++ show y ++ "\n"

writeFunc :: (Show a, Show b) => FilePath -> (a -> b) -> [a] -> IO ()
writeFunc path f inps = writePoints path ps >> appendFile path "end\n\n"
    where
    ps = zip [1 .. length inps] $ map f inps

