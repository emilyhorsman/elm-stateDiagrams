{- A state diagram representing the autonomous mode of a robot in the FIRST
   Robotics Competition game, Rebound Rumble.

   The robot’s goal is to score points by firing basketballs into hoops.

   Its autonomous mode must advance through the following sequence:

   - Rotate towards other robot on alliance
   - Collect additional basketballs from other robot (feed for 5 seconds)
   - Rotate back to starting position
   - Drive forward to shooting line
   - Aim at top hoop with camera
   - Fire until all basketballs expended or timer runs out
-}


module Main exposing (..)

import List
import GraphicSVG exposing (..)
import StateDiagrams exposing (..)


type Msg
    = Tick Float GetKeyState


type MotorDirection
    = Forward
    | Reverse


type Feeder
    = On MotorDirection
    | Off


startGame state =
    case state of
        Off ->
            On Forward

        otherwise ->
            otherwise

collectedBalls state =
    case state of
        On Forward ->
            Off

        otherwise ->
            otherwise


feederStates =
    [ ( (Off), ( 0, 100 ) )
    , ( (On Forward), ( 0, 0 ) )
    ]


feederTransitions =
    [ ( startGame
      , "start game"
      , [ ( Off, ( -25, 40 ) )
        ]
      )
    , ( collectedBalls
      , "collected balls"
      , [ ( On Forward, ( 25, 60 ) )
        ]
     )
    ]


type alias State =
    { feeder : Feeder
    }


model =
    { tick = 0
    , state =
        { feeder = Off
        }
    , transition =
        { feeder = ( Off, "" )
        }
        -- Provide what is essentially a void transition, since one is expected.
    }


update msg model =
    model


viewFeeder model =
    [ viewStateDiagram feederStates
        feederTransitions
        (Just model.state.feeder)
        (Just model.transition.feeder)
    , rectangle 160 150 |> outlined (dashed 1) black |> move ( 15, 50 )
    , text "Feeder" |> filled black |> move ( -60, 110 )
    ]


view model =
    collage 1024
        760
        (viewFeeder model |> List.map (move ( 0, 200 )))


main =
    gameApp Tick
        { model = model
        , view = view
        , update = update
        }
