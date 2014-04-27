import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import BinaryTreeFunctions (runTree, buildBalanced, prettyPrint)

play = do
	list <- fmap Text.lines (Text.readFile "gameFile.txt")    
	let tree = buildBalanced list								
	runTree tree
	putStrLn "Quer jogar de novo? (1 para sim, 2 para nao)"
	playAgain <- readLn
	if (playAgain == 1) then (play) else  print "Obrigado por jogar!"
