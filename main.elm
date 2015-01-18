import List ((::))
import List
import Dict
import Graphics.Element (..)
import Text
import Signal
import Keyboard

type alias Row = List Int
type alias Board = List Row
type alias Point = {x:Int, y:Int}
type alias PieceShape = List Point
type alias Piece = PieceShape
type alias State = {board:Board, pos:Point, piece:Piece}

pieces = Dict.fromList <|
  [ ("I",   [ {x=0, y=0}, {x=0, y=-1}, {x=0, y=1}, {x=0, y=2} ]),
    ( "J",  [ {x=0, y=0}, {x=0, y=-1}, {x=0, y=1}, {x=-1, y=1} ]),
    ( "S",  [ {x=0, y=0}, {x=-1, y=0}, {x=0, y=-1}, {x=1, y=-1} ]),
    ( "Z",  [ {x=0, y=0}, {x=-1, y=-1}, {x=0, y=-1}, {x=1, y=0} ]),
    ( "O",  [ {x=0, y=0}, {x=-1, y=0}, {x=-1, y=1}, {x=0, y=1} ]),
    ( "T",  [ {x=0, y=0}, {x=-1, y=0}, {x=1, y=0}, {x=0, y=1} ]) ]

getPiece name = 
    case Dict.get name pieces of
        Just p      -> p
        Nothing     -> [{x=-1, y=-1}]

(cols, rows) = (10, 20)
createEmptyBoard : Board
createEmptyBoard = List.repeat rows (List.repeat cols 0)

centerTop : Point
centerTop = { x = round (cols/2), y = 1}

main : Signal Element
main = 
    Signal.foldp (\dir k -> { k | x <- dir.x + k.x }) centerTop Keyboard.arrows 
        |> Signal.map (\s -> {board = createEmptyBoard, pos = s, piece = getPiece "T"})
        |> Signal.map writePieaceOnBoard 
        |> Signal.map renderBoard 
        |> Signal.map (flow down)        

renderBoard : Board -> List Element
renderBoard b = List.map renderRow b

writePieaceOnBoard : State -> Board
writePieaceOnBoard state = List.foldl (\p acc -> addPoints p state.pos |> writePointToBoard acc) state.board state.piece

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

