import StateDiagrams exposing (..)
import GraphicSVG exposing (..)

type Msg = Tick Float GetKeyState

main = gameApp Tick {
                      model = model
                    , view = view
                    , update = update
                    }
{-
 - The basis of your model is a record type with fields or 'variables'
 - you'd like to keep track of
 -}
model = {}

-- the view function renders the current model
view model = collage 800 800 [viewStateDiagram states transitions Nothing Nothing]

-- insert update code here
update msg model = {}
