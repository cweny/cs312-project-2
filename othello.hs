import Data.Map as Map
import Data.List as List
import Control.Monad

data Color = White | Black | Empty deriving (Eq, Show)
type Move = (Int, Int)
type Board = Map.Map Move Color

-- 
-- SETUP
-- 

-- A list of all possible coordinates for moves on the board.
all_moves :: [Move]
all_moves = [(x,y) | x <- [0..7], y <- [0..7]]


-- Initial list of all possible moves.
start_boarder :: [Move]
start_boarder = [(2,5),(3,5),(4,5),(5,5),(2,4),(5,4),(2,3),(5,3),(2,2),(3,2),(4,2),(5,2)]

-- Empty board, no pieces.
empty_board :: Board
empty_board = Map.fromList (List.zip all_moves (repeat Empty))

-- Initial boad with 2 black pieces and 2 white pieces.
init_board :: Board
init_board = Map.union (fromList (List.zip [(3,3), (3,4), (4,4), (4,3)] 
	[White, Black, White, Black])) empty_board

--  Returns the opposing color of player given.
other_color :: Color -> Color
other_color Black = White
other_color White = Black
other_color Empty = Empty

-- 
-- GAMEPLAY/RULES
-- 

--  Checks all directions and returns list of moves in given direction.
check_alldirections :: Move -> String -> [Move]
check_alldirections (x,y) direction
  | direction == "U" =  [(x, y+h)   | h <- [1..7], y+h <=7]
  | direction == "R" =  [(x+h, y)   | h <- [1..7], x+h <=7]
  | direction == "B" =  [(x, y-h)   | h <- [1..7], y-h >=0]
  | direction == "L" =  [(x-h, y)   | h <- [1..7], x-h >=0]
  | direction == "UR" = [(x+h, y+h) | h <- [1..7], x+h <=7, y+h <=7]
  | direction == "BR" = [(x+h, y-h) | h <- [1..7], x+h <=7, y-h >=0]
  | direction == "BL" = [(x-h, y-h) | h <- [1..7], x-h >=0, y-h >=0]
  | otherwise = [(x-h, y+h) | h <- [1..7], x-h >=0, y+h <=7]
 
-- Returns true if move is allowed.
is_allowedmove :: Board -> Color -> Move -> Bool
is_allowedmove b c p = (Map.!) b p == Empty && List.length (toflip b p c) > 0

-- Returns a list of allowed moves for the player.
get_allowedmove :: Board -> Color -> [Move] -> [Move]
get_allowedmove b c ml = List.filter (\p -> is_allowedmove b c p) ml

-- Returns the list of pieces that need to be flipped.
toflip :: Board -> Move -> Color -> [Move]
toflip b p c = List.concat 
	[toflip_indirection b c l | l <- List.map (check_alldirections p) ["U", "R", "B", "L", "UR", "BR", "BL"]]

-- Returns a list of colors at position given.
get_colors :: Board -> [Move] -> [Color]
get_colors b = List.map (b Map.!)

-- Finds the list of pieces that need to be flipped.
toflip_indirection :: Board -> Color -> [Move] -> [Move]
toflip_indirection _ _ [] = []
toflip_indirection _ _ (_:[]) = []
toflip_indirection b p pl 
	| top /= [] && fst (head top) == p = List.map snd (List.takeWhile (\x -> fst x == other_color p) (zip (get_colors b pl) pl))
	| otherwise = []
	where 
		top = List.dropWhile (\x -> fst x == other_color p) (zip (get_colors b pl) pl)

-- Counts number of pieces for player with color on the board.
count_colors :: Board -> Color -> Int
count_colors b col = Map.fold 
		(\value acc -> if value == col then 1 + acc else acc) 0 b		

--  Returns the player with most pieces on the board
winner :: Board -> Color
winner b =  if numB > numW then Black 
				else if numB == numW then Empty
					else White
		where 
			numB = count_colors b Black
			numW = count_colors b White

-- Makes a move for player on the baord.
make_move :: Move -> Board -> Color -> Board
make_move p b c = 
	let pl = toflip b p c in
	Map.union (fromList [(p, c)]) 
		(Map.union (fromList $ List.zip pl (repeat $ c)) b) 

-- Returns user move given read input.
get_usermove :: Board -> Color -> IO Move
get_usermove board color = do
	l <- getLine
	return $ read l  

--
-- AI
--

-- Initializes AI strength, read input.
init_AI :: IO Int
init_AI = do
	putStrLn "How strong should the AI be? [1,2,3...]"
	putStrLn "Note that errors occur if this number is too high"
	inDepth <- getLine
	return $ read inDepth

-- Returns best move with board value for AI.
get_aimove :: Board -> Color -> Int -> [Move] -> Move
get_aimove board color depth movelist =
	(\(position, _) -> position) (List.maximumBy
			(\(_,boardValue1) (_,boardValue2) -> compare boardValue1 boardValue2)
			(List.map
				(\pos -> (pos, get_boardvalue (make_move pos board color) color color depth movelist))
				(get_allowedmove board color movelist)))

-- Returns a board value given the board, current color, movelist, and depth.
get_boardvalue :: Board -> Color -> Color -> Int -> [Move] -> Int
get_boardvalue board origColor color depth movelist =
	let
		nextColor = other_color color
		nextColorLegalMoves = get_allowedmove board nextColor movelist
		nextColorBoardValue = maximum (
			List.map 
				(\position -> get_boardvalue (make_move position board color) origColor nextColor (depth - 1) (update_boarder board position movelist))
				nextColorLegalMoves)
		thisColorBoardValue = length $ get_allowedmove board color movelist
	in if (List.null nextColorLegalMoves)
		then if (winner board) == origColor then 10000 else -10000
		else
			if depth > 0
				then if nextColor == origColor then nextColorBoardValue else - nextColorBoardValue
				else if nextColor == origColor then thisColorBoardValue else - thisColorBoardValue

-- Returns raw board value.
get_raw_boardvalue :: Board -> Color -> Int
get_raw_boardvalue board color = 
	if color == Black
	then  bScore - wScore
	else wScore - bScore
	where 
		bScore = (count_colors board Black)
		wScore = (count_colors board White)

-- Updates boarder by removing the move placed and adding, if on an empty board space, all adjacent board spaces.
update_boarder :: Board -> Move -> [Move] -> [Move]
update_boarder _ _ [] = []
update_boarder b p ml = do
	remove_elem (x,y) ml3
	where
		x = fst p
		y = snd p
		up = if (y+1 < 8) then 
					if (head (get_colors b [(x,y+1)])) == Empty then (x,y+1) else (-1,-1)
				else (-1,-1)
		down = if (y-1 >= 0) then 
					if (head (get_colors b [(x,y-1)])) == Empty then (x,y-1) else (-1,-1)
				else (-1,-1)
		left = if (x-1 >= 0) then
 					if (head (get_colors b [(x-1,y)])) == Empty then (x-1,y) else (-1,-1)
				else (-1,-1)
		right = if (x+1 < 8) then 
					if (head (get_colors b [(x+1,y)])) == Empty then (x+1,y) else (-1,-1)
				else (-1,-1)
		ul = if (y+1 < 8 && x-1 >= 0) then 
					if (head (get_colors b [(x-1,y+1)])) == Empty then (x,y+1) else (-1,-1)
				else (-1,-1)
		ur = if (y+1 < 8 && x+1 < 8) then 
					if (head (get_colors b [(x+1,y+1)])) == Empty then (x,y+1) else (-1,-1)
				else (-1,-1)
		dl = if (y-1 >= 0 && x-1 >= 0) then
 					if (head (get_colors b [(x-1,y-1)])) == Empty then (x-1,y-1) else (-1,-1)
				else (-1,-1)
		dr = if (y-1 >= 0 && x+1 < 8) then 
					if (head (get_colors b [(x+1,y-1)])) == Empty then (x+1,y-1) else (-1,-1)
				else (-1,-1)
		ml1 = ml ++ [up,down,left,right,ul,ur,dl,dr]
		ml2 = no_duplicates ml1
		ml3 = clean_list (-1,-1) ml2 

	
-- Remove all instances of the given element from the given list
clean_list :: Eq t => t -> [t] -> [t]
clean_list _ [] = []
clean_list x (y:ys) 
	| x == y = clean_list x ys
		| otherwise = y : clean_list x ys

-- no_duplicates lst removes duplicates from a list; returns a list with
-- the same elements as lst, but with only one instance of each element, thanks David Poole
no_duplicates :: Eq t => [t] -> [t]
no_duplicates [] = []
no_duplicates (e1:r)
   | elem e1 r = no_duplicates r
   | otherwise = e1 : no_duplicates r

-- Remove the first instance of the given element from the given list.
remove_elem :: Eq t => t -> [t] -> [t]
remove_elem _ [] = []
remove_elem x (y:ys) | x == y = ys
                    | otherwise = y : remove_elem x ys

-- 
-- UI
--

-- Prints symbol for each player with color.
print_symbol :: Color -> String
print_symbol color 
	| color == White = "O"
	| color == Black = "X"
	| otherwise = " "

-- Print row.
printRow :: Board -> Int -> String
printRow board row = show row ++ " | " ++
	(intercalate " | " (List.map
		(\position -> print_symbol (board ! position))
		([(x, row) | x <- [0..7]]))) ++" | " ++ show row

-- Print board.
printBoard :: Board -> String
printBoard board =
	"\n    0   1   2   3   4   5   6   7 \n -----------------------------------\n" ++
	(intercalate
		"\n -----------------------------------\n"
		(List.map (printRow board) $ [0..7])) ++ "\n -----------------------------------\n    0   1   2   3   4   5   6   7 "

-- Sets how frequently moves print.
set_printfreq :: IO Int
set_printfreq = do
	putStrLn "How frequently should moves be printed? [1,2,3...]"
	freq <- getLine
	return $ read freq

-- 
-- GAME MODES
-- 

-- Starts the game.
main :: IO ()
main = do
	putStrLn "Welcome to Othello!"
	putStrLn "Play with a friend: (2)"
	putStrLn "Play with the AI: (1)"
	putStrLn "Watch AI vs. AI death-match: (0)"
	putStrLn "Pick a mode: "
	playerNumber <- getLine
	let pn = read playerNumber
	if (pn /= 0 && pn /= 1 && pn /= 2) 
		then error "Invalid Mode!"
		else if pn == 0 then 
			do
			putStrLn "Diffuculty for White AI:"
			wDepth <- init_AI
			putStrLn "Diffuculty for Black AI:"
			bDepth <- init_AI
			freq <- set_printfreq
			ai_vs_ai init_board White start_boarder wDepth bDepth freq 0
		else if pn == 1 then
			do
			depth <- init_AI
			player_vs_ai depth init_board White start_boarder
		else player_vs_player init_board White start_boarder

-- AI vs. AI.
ai_vs_ai :: Board -> Color -> [Move] -> Int -> Int -> Int -> Int -> IO ()
ai_vs_ai board color boarder wDepth bDepth freq turnNum = do
	let movelist = get_allowedmove board color boarder
	if (List.null movelist)
		then
			do
			putStrLn $ printBoard board
			putStrLn ("Turn " ++ (show turnNum))
			putStrLn ("Black has " ++ (show (count_colors board Black)) ++ " points") 
			putStrLn ("White has " ++ (show (count_colors board White)) ++ " points")
			if gameWinner == Empty then putStrLn "Tie!"
				else if gameWinner == Black 
					then do 
						putStrLn "Black Wins!"
					else putStrLn "White Wins!"
		else
			do
			if color == White
			then
				do
				when ((turnNum `mod` freq) == 0) (putStrLn $ printBoard board)
				when ((turnNum `mod` freq) == 0) (putStrLn ("Turn " ++ (show turnNum) ++ ", White's move"))
				let 
					move = (get_aimove board color wDepth movelist)
					boarder1 = update_boarder board move boarder
				ai_vs_ai (make_move move board White) (other_color color) boarder1 wDepth bDepth freq (turnNum + 1)
			else 	
				do
				when ((turnNum `mod` freq) == 0) (putStrLn $ printBoard board)
				when ((turnNum `mod` freq) == 0) (putStrLn ("Turn " ++ (show turnNum) ++ ", Black's move"))
				let 
					move = (get_aimove board color bDepth movelist)
					boarder1 = update_boarder board move boarder
				ai_vs_ai (make_move move board Black) (other_color color) boarder1 wDepth bDepth freq (turnNum + 1)

	where
		gameWinner = winner board
		whiteMove = get_usermove board White
		blackMove = get_usermove board Black

-- Player vs. AI.
player_vs_ai :: Int -> Board -> Color -> [Move] -> IO ()
player_vs_ai depth board color boarder = do
	let movelist = get_allowedmove board color boarder
	if (List.null movelist)
		then
			do
			putStrLn $ printBoard board
			putStrLn ("Black has " ++ (show (count_colors board Black)) ++ " points") 
			putStrLn ("White has " ++ (show (count_colors board White)) ++ " points")
			if gameWinner == Empty then putStrLn "Tie!"
				else if gameWinner == Black 
					then do 
						putStrLn "Black Wins!"
					else putStrLn "White Wins!"
		else
			do		
			putStrLn $ printBoard board
			if color == White
			then
				do
				putStrLn "Move for White player:"
				putStrLn $ List.concat $ List.map show (get_allowedmove board color movelist)
				wm <- whiteMove
				if is_allowedmove board White wm
					then
						player_vs_ai depth (make_move wm board White) (other_color color) (update_boarder board wm boarder)
					else
						do
						putStrLn "Illegal Move!"
						player_vs_ai depth board color boarder
			else 
				do
				putStrLn "AI is thinking of a move for Black Player..."
				let 
					move = (get_aimove board color depth movelist)
					boarder1 = update_boarder board move boarder
				player_vs_ai depth (make_move move board Black) (other_color color) boarder1


	where
		gameWinner = winner board
		whiteMove = get_usermove board White
		blackMove = get_usermove board Black

-- Player vs. Player.
player_vs_player :: Board -> Color -> [Move] -> IO ()
player_vs_player board color boarder = do
	let movelist = get_allowedmove board color boarder
	if (List.null movelist)
		then
			do
			putStrLn $ printBoard board
			putStrLn ("Black has " ++ (show (count_colors board Black)) ++ " points") 
			putStrLn ("White has " ++ (show (count_colors board White)) ++ " points")
			if gameWinner == Empty then putStrLn "Tie!"
				else if gameWinner == Black 
					then do 
						putStrLn "Black Wins!"
					else putStrLn "White Wins!"
		else
			do		
			putStrLn $ printBoard board
			if color == White
			then
				do
				putStrLn "Move for White player:"
				putStrLn $ List.concat $ List.map show (get_allowedmove board color movelist)
				wm <- whiteMove
				if is_allowedmove board White wm
					then
						player_vs_player (make_move wm board White) (other_color color) (update_boarder board wm boarder)
					else
						do
						putStrLn "Illegal Move!"
						player_vs_player board color boarder
			else 
				do
				putStrLn "Move for the Black player: "
				putStrLn $ List.concat $ List.map show (get_allowedmove board color movelist)
				bm <- blackMove
				if is_allowedmove board Black bm
					then
						player_vs_player (make_move bm board Black) (other_color color) (update_boarder board bm boarder)
					else
						do
						putStrLn "Illegal Move!"
						player_vs_player board color boarder
	where
		gameWinner = winner board
		whiteMove = get_usermove board White
		blackMove = get_usermove board Black

