import List(..)
import Dict
import Graphics.Element (..)
import Text (..)

type alias Row = List Int
type alias Board = List (Row)

board : Board
board = createEmptyBoard

(cols, rows) = (10, 20)
createEmptyBoard : Board
createEmptyBoard = repeat rows (repeat cols 0)

main : Element
main = flow down (renderBoard board)

renderBoard : Board -> List Element
renderBoard b = map renderRow b

renderRow : Row -> Element
renderRow r = asText r

--pieces = Dict.fromList [ ("I", [[0,0],[0,-1],[0,1],[0,2]]) ]

type alias Coordinate = (Int, Int)

--writeToBoard : Board -> Coordinate -> Int -> Board
--writeToBoard board x y val = indexedMap writeToRow board

--writeToRow : Row -> Int -> Row
--writeToRow r i = indexedMap writeToCell r

--writeToCell : Int -> Int
--writeToCell c i 