import Graphics.Element (..)
import Keyboard
import Mouse
import Time
import Signal
import Signal (Signal, (<~))
import Text (asText)
import List (..)

    
main : Signal.Signal Element
main = Signal.foldp (\dir k -> {x = k.x + dir.x, y = k.y + dir.y} ) {x=0,y=0} Keyboard.arrows 
        |> Signal.map (\s -> render createEmptyBoard s)
        |> Signal.map asText
        

(cols, rows) = (10, 20)
createEmptyBoard : List(Int)
createEmptyBoard = repeat cols 0

render : List(Int) -> {x:Int, y:Int} -> List(Int)
render l s = append (take s.x l) (1 :: drop s.x l)