import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import System.Process
import BinaryTreeFunctions (buildBalancedTree, printTree, preOrder)

clear = system "cls"
	
showGameMap = do
	clear
	putStrLn "Os numeros indicam a linha do arquivo e essa funcao mostra em quais nodes essas linhas se encontram"
	story <- fmap Text.lines (Text.readFile "gameFile.txt")
	let storyLength = length story
	let newList = [1..storyLength]
	let gameTree = buildBalancedTree newList
	putStr (printTree gameTree)

help = do
	clear
	story <- fmap Text.lines (Text.readFile "gameFile.txt")
	let storyLength = length story
	let newList = [1..storyLength]
	putStrLn "Sua historia normalmente eh organizada dessa forma: "
	print newList
	let gameTree = buildBalancedTree newList
	putStrLn "A lista a seguir mostra em quais linhas devem ficar cada uma das linhas da sua historia: "
	print (preOrder gameTree)