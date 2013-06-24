import WebSocket
import Color
import Window
import Graphics.Input as GI
import Graphics.Element as GE

cITEM_HEIGHT = 40
cSIG_WND_WIDTH = min 400 <~ Window.width

ws : Signal String -> Signal String
ws = WebSocket.connect "ws://localhost:8080/websocket"

(newItemField, newItemData) = GI.field ""
(newItemButton, newItemPressed) = GI.button "Add Item"

bordered : Int -> Element -> Element
bordered width =
    color Color.white . container width cITEM_HEIGHT middle .
    color Color.black . container (width - 2)  (cITEM_HEIGHT - 2) middle .
    color Color.white . container (width - 10) (cITEM_HEIGHT - 10) middle .
    GE.width (width - 10) . GE.height (cITEM_HEIGHT - 10)

null l = case l of
           [] -> True
           _::_ -> False

filteredData : Signal String
filteredData = dropIf null "" $ newItemPressed `sampleOn` newItemData

inputs : Signal Element
inputs = beside <~
         (bordered . (\w -> 3 * w `div` 4) <~ cSIG_WND_WIDTH ~ newItemField) ~
         (GE.width . (\w -> 1 * w `div` 4) <~ cSIG_WND_WIDTH ~ constant newItemButton)

split' : [a] -> [a] -> [[a]]
split' sep = reverse . tail . reverse . split sep

outputs : Signal Element
outputs = flow down <~
          (map . bordered <~ cSIG_WND_WIDTH ~ (map Text.plainText . split' "\n" <~ ws filteredData))

main : Signal Element
main = container <~ Window.width ~ Window.height ~ constant midTop ~ (above <~ inputs ~ outputs)
