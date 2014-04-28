module BinaryTreeFunctions (mainGameLoop, buildBalancedTree, printTree, preOrder) where

import System.Process

data BinTree a = Null
	|Node a (BinTree a)(BinTree a)
	deriving (Eq,Show)

clear = system "cls"	

mainGameLoop :: (Eq a, Show a) => BinaryTreeFunctions.BinTree a -> IO()
mainGameLoop (Node a Null Null) = do
	clear
	print a
	putStrLn "Fim de jogo"
mainGameLoop (Node a left right) = do
	clear
	print a 
	putStrLn "Faca sua escolha: "
	choice <- readLn
	if ((choice == 1) && (left /= Null)) then (mainGameLoop (left)) else (if ((choice == 2) && (right /= Null)) then (mainGameLoop (right)) else mainGameLoop(Node a left right))

buildBalancedTree :: [a] -> BinaryTreeFunctions.BinTree a
buildBalancedTree [] = Null
buildBalancedTree list = (Node (list !! half)(buildBalancedTree $ take half list) (buildBalancedTree $ drop (half+1) list))
    where half = length list `quot` 2

printTree :: Show a => BinaryTreeFunctions.BinTree a -> [Char]
printTree (Null) = "Empty root."
printTree (Node a left right) = unlines (printTreeHelper (Node a left right))

printTreeHelper :: Show a => BinaryTreeFunctions.BinTree a -> [[Char]]
printTreeHelper (Node a left right)
    = (show a) : (printTreeSubtree left right)
        where
            printTreeSubtree left right =
                ((pad "+- " "|  ") (printTreeHelper right))
                    ++ ((pad "`- " "   ") (printTreeHelper left))
            pad first rest = zipWith (++) (first : repeat rest)
printTreeHelper (Null) = []	

preOrder :: (Ord a) => BinaryTreeFunctions.BinTree a -> [a]
preOrder Null = []
preOrder (Node a left right) = [a] ++ preOrder left ++ preOrder right