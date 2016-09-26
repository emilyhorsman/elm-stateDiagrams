module Main exposing (..)

import List
import GraphicSVG exposing (..)
import StateDiagrams exposing (..)


{-
   First, we define the types required for our
   state diagram.

   *Msg    a type required by GraphicsSVG to keep track of
           the state of the program
   *State  For a traffic light, it can only exist in 1 out of
           3 possible states
-}


type Token
    = Comment
    | DOCTYPE
    | Space
    | Html


type Msg
    = Tick Float GetKeyState


type State
    = InitialInsertion
    | BeforeHtml
    | BeforeHead



{-
   Now we define a simplified initial Model. We use a record type
   with 3 fields to keep track of the state.

   *tick       simplified measurement of some change in time
   *state      the current state of a trafficlight
   *transition the transition leading to the current state
-}


model =
    { tick = 0
    , state = InitialInsertion
    , transition = ( InitialInsertion, "" )
    }



{-
   "states" and "transitions" contain what the StateDiagrams
   library will draw to the screen. Both of these lists contain
   something names to draw to the screen and the locations of
   where they will be drawn
-}


states =
    [ ( InitialInsertion, ( 0, 200 ) )
    , ( BeforeHtml, ( 0, 100 ) )
    , ( BeforeHead, ( 0, 0 ) )
    ]


transitions =
    [ ( updateState
      , "comment"
      , [ ( InitialInsertion, ( 120, 210 ) )
        , ( BeforeHtml, ( 120, 110 ) )
        , ( BeforeHead, ( 120, 0 ) )
        ]
      )
    ]



{-
   Using the state diagrams library requires us to write a function
   containing the instructions for updating the state. This function
   takes a State and gives us back a new State.

   For a traffic light, this is fairly trivial.
       - A Green light turns Yellow
       - A Yellow light turns Red
       - A Red light turns Green
-}


updateState : State -> State
updateState t =
    case t of
        otherwise ->
            otherwise


update msg model =
    model


view model =
    collage 768 760 [ viewStateDiagram states transitions (Just model.state) (Just model.transition) ]


main =
    gameApp Tick
        { model = model
        , view = view
        , update = update
        }
