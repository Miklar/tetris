import List ((::))
import List
import Dict
import Graphics.Element (..)
import Text

type alias Row = List Int
type alias Board = List Row
type alias Point = {x:Int, y:Int}
type alias Piece = List Point
type alias State = {board:Board, pos:Point, piece:Piece}

(cols, rows) = (10, 5)
createEmptyBoard : Board
createEmptyBoard = List.repeat rows (List.repeat cols 0)

initialState : State
initialState = {board = createEmptyBoard, pos = {x = 2, y = 2}, piece = [{x=0,y=0},{x=1,y=-1}]}

main : Element
main = initialState |> writePieaceOnBoard |> renderBoard |> flow down

renderBoard : Board -> List Element
renderBoard b = List.map renderRow b

writePieaceOnBoard : State -> Board
writePieaceOnBoard state = List.foldl (\p acc -> addPoints p state.pos |> writePointToBoard acc) createEmptyBoard state.piece

addPoints : Point -> Point -> Point
addPoints p1 p2 = {x = p1.x + p2.x, y = p1.y + p2.y}

renderRow : Row -> Element
renderRow r = Text.asText r

writePointToBoard : Board -> Point -> Board
writePointToBoard b p = 
    List.append 
        (List.take p.y b)
        ((writeToRow (spliceRow p.y b) p.x) :: List.drop (p.y + 1) b)

spliceRow : Int -> Board -> Row
spliceRow pivot board = List.drop pivot board |> List.head

writeToRow : Row -> Int -> Row
writeToRow row pivot = 
    List.append (List.take pivot row) (1 :: List.drop (pivot + 1) row)

