import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import BinaryTreeFunctions (mainGameLoop, buildBalancedTree)
import System.Process

clear = system "cls"

play = do
	clear
	story <- fmap Text.lines (Text.readFile "gameFile.txt")    
	let gameBinTree = buildBalancedTree story								
	mainGameLoop gameBinTree
	putStrLn "Quer jogar de novo? (1 para sim, 2 para nao)"
	playAgain <- readLn
	if (playAgain == 1) then (play) else  print "Obrigado por jogar!"
