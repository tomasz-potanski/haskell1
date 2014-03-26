import System.Environment  
import System.IO  
import System.Directory  
import Prelude
import TreeDict
import Data.List
  
main = do 
			argz <- getArgs
			if ((length argz) >= 1) then do
				(fileName:_) <- getArgs
				fileExists <- doesFileExist fileName  
				if fileExists  
					then do contents <- readFile fileName  

						let dodruku = (take 10 (sort (toListSwap (fromList2 (filter odsiewacz (foldl1 (++) (map (words) (lines contents))))))))
						let max_dl = (foldl (\acc (a,b) -> max acc (length b)) 0 dodruku)+6
						let dodruku2 = map (\(a,b) -> (-a, b ++ dajXspacji(max_dl - (length b)))) dodruku
						mapM_ (\x -> (putStrLn  ((snd x) ++ "\t" ++ (show (fst x))))) dodruku2

						return ()
					else do putStrLn "The file doesn't exist!"
			else do putStrLn "You didn't typed file name! ;)"
			

odsiewacz = (\x -> (length x > 2) && (isInteger x == False))
		
isInteger s = case reads s :: [(Integer, String)] of
	[(_, "")] -> True
	_		  -> False
	
dajXspacji 0 = ""
dajXspacji 1 = " "
dajXspacji n = " " ++ (dajXspacji (n-1))