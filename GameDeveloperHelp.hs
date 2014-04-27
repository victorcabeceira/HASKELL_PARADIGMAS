import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import BinaryTreeFunctions (buildBalanced, prettyPrint)
	
showGameMap = do
	putStrLn "Os numeros indicam a linha do arquivo e essa funcao mostra em quais nodes essas linhas se encontram"
	list <- fmap Text.lines (Text.readFile "gameFile.txt")
	let tamanho = length list
	let newList = [1..tamanho]
	let gameTree = buildBalanced newList
	putStr (prettyPrint gameTree)
	