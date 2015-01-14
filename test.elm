import List(..)
import Dict
import Graphics.Element (..)
import Text (..)

type alias Row = List Int
type alias Board = List (Row)
type alias Point = {x:Int, y:Int}

board : Board
board = createEmptyBoard

(cols, rows) = (10, 20)
createEmptyBoard : Board
createEmptyBoard = repeat rows (repeat cols 0)

main : Element
main = writeToBoard board {x=0, y=0} |> renderBoard |> flow down

renderBoard : Board -> List Element
renderBoard b = map renderRow b

renderRow : Row -> Element
renderRow r = asText r

writeToBoard : Board -> Point -> Board
writeToBoard board point = indexedMap (\i row -> writeToRow row point i) board

writeToRow : Row -> Point -> Int -> Row
writeToRow row point i = 
    if  | i == point.y  -> writeToCell row point
        | otherwise     -> row

writeToCell : Row -> Point -> Row
writeToCell row point = indexedMap (\i _ -> updateIfRight point i) row

updateIfRight : Point -> Int -> Int
updateIfRight point i = if  | i == point.x  -> 1
                            | otherwise     -> 0