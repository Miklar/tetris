import List ((::))
import List
import Dict
import Graphics.Element (..)
import Text

type alias Row = List Int
type alias Board = List (Row)
type alias Point = {x:Int, y:Int}
type alias Piece = List Point
type alias State = {board:Board, position:Point, piece:Piece}

(cols, rows) = (10, 20)
createEmptyBoard : Board
createEmptyBoard = List.repeat rows (List.repeat cols 0)

initialState : State
initialState = {board = createEmptyBoard, position = {x = 2, y = 2}, piece = [{x=0,y=0},{x=1,y=0}]}

main : Element
main = initialState |> writePieaceOnBoard |> renderBoard |> flow down

writePieaceOnBoard : State -> Board
writePieaceOnBoard state = writeToBoard state


renderBoard : Board -> List Element
renderBoard b = List.map renderRow b

renderRow : Row -> Element
renderRow r = Text.asText r

writeToBoard : State -> Board
writeToBoard state = List.map (\row -> List.append (List.take state.position.x row) (1 :: List.drop (state.position.x + 1) row)) state.board
--writeToBoard state = indexedMap (\i row -> writeToRow row state.position i) state.board

writeToRow : Row -> Point -> Int -> Row
writeToRow row point i = 
    if  | i == point.y  -> writeToCell row point
        | otherwise     -> row

writeToCell : Row -> Point -> Row
writeToCell row point = List.indexedMap (\i _ -> updateIfRight point i) row

updateIfRight : Point -> Int -> Int
updateIfRight point i = if  | i == point.x  -> 1
                            | otherwise     -> 0