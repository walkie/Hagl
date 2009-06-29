-- TODO needs to be adapted to the new deal

-----------------
-- Tic Tac Toe --
-----------------

data Square = X | O | Empty deriving (Eq, Show)
type Board = [Square]
type Move = Int

mark 1 = X
mark 2 = O

empty :: Board -> [Int]
empty = elemIndices Empty

end :: Board -> PlayerIx -> Bool
end b p = win b p || null (empty b)

avail :: Board -> PlayerIx -> [Move]
avail b _ = empty b

exec :: Board -> PlayerIx -> Move -> Board
exec b p m = take m b ++ mark p : drop (m+1) b

pay :: Board -> PlayerIx -> [Float]
pay b p | win b p = winner 2 p
        | otherwise = tie 2

win :: Board -> PlayerIx -> Bool
win b p = let h = chunk 3 b
              v = transpose h
              d = map (map (b !!)) [[0,4,8],[2,4,6]]
          in or $ map (and . map (mark p ==)) (h ++ v ++ d)

ticTacToe = takeTurns 2 end avail exec pay (replicate 9 Empty)
