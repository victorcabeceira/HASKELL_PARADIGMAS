module BinaryTreeFunctions (runTree, buildBalanced, prettyPrint) where

data BinTree a = Null
	|Node a (BinTree a)(BinTree a)
	deriving (Eq,Show)

runTree :: (Eq a, Show a) => BinaryTreeFunctions.BinTree a -> IO()
runTree (Node a left right) = do
	print a 
	putStrLn "Faca sua escolha: "
	choice <- readLn
	if ((choice == 1) && (left /= Null)) then (runTree (left)) else (if ((choice == 2) && (right /= Null)) then (runTree (right)) else print "Game over")

buildBalanced :: [a] -> BinaryTreeFunctions.BinTree a
buildBalanced []   = Null
buildBalanced list = (Node (list !! half)(buildBalanced $ take half list) (buildBalanced $ drop (half+1) list))
    where half = length list `quot` 2

prettyPrint :: Show a => BinaryTreeFunctions.BinTree a -> [Char]
prettyPrint (Null) = "Empty root."
prettyPrint (Node a left right) = unlines (prettyPrintHelper (Node a left right))

prettyPrintHelper (Node a left right)
    = (show a) : (prettyPrintSubtree left right)
        where
            prettyPrintSubtree left right =
                ((pad "+- " "|  ") (prettyPrintHelper right))
                    ++ ((pad "`- " "   ") (prettyPrintHelper left))
            pad first rest = zipWith (++) (first : repeat rest)
prettyPrintHelper (Null) = []	