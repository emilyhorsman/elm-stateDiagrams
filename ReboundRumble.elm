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


type Motor
    = On MotorDirection
    | Off


type alias Feeder =
    Motor


type alias DriveTrain =
    Motor


type alias HoldingQuantity =
    Int



-- How many balls do we have to shoot?


type RobotDirection
    = LeftPartner
    | RightPartner
    | Hoops


type Position
    = Facing RobotDirection
    | ShootingLine


type alias RobotState =
    ( Position, HoldingQuantity, DriveTrain, DriveTrain )



-- Feeder Component


startFeederGame state =
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
    [ ( startFeederGame
      , "start game"
      , [ ( Off, ( -25, 40 ) )
        ]
      )
    , ( collectedBalls
      , "reached capacity"
      , [ ( On Forward, ( 25, 60 ) )
        ]
      )
    ]



-- Robot State


startRobotPositionGame state =
    case state of
        ( Facing Hoops, holding, Off, Off ) ->
            ( (Facing Hoops), holding, (On Reverse), (On Forward) )

        otherwise ->
            otherwise

stepRobot state =
    case state of
        ( Facing Hoops, holding, On Reverse, On Forward ) ->
            ( Facing LeftPartner, holding, Off, Off )

        otherwise ->
            otherwise

robotStates =
    [ ( ( (Facing Hoops), 2, Off, Off ), ( 0, 0 ) )
    , ( ( (Facing Hoops), 2, (On Reverse), (On Forward) ), ( -100, -40 ) )
    , ( ( (Facing LeftPartner, 2, Off, Off ) ), (-100, -80) )
    ]


robotTransitions =
    [ ( startRobotPositionGame
      , "start game"
      , [ ( ( (Facing Hoops), 2, Off, Off ), ( -120, -10 ) )
        ]
      )
    , ( stepRobot
      , "finishing rotating"
      , [ ( ( (Facing Hoops), 2, On Reverse, On Forward), (-120, -70))
        ]
      )
    ]


type alias State =
    { feeder : Feeder
    , robotPosition : RobotState
    }


model =
    { tick = 0
    , state =
        { feeder = Off
        , robotPosition = ( (Facing Hoops), 2, Off, Off )
        }
    , transition =
        { feeder = ( Off, "" )
        , robotPosition = ( ( (Facing Hoops), 2, Off, Off ), "" )
        }
        -- Provide what is essentially a void transition, since one is expected.
    }



-- Game


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


viewRobot model =
    [ viewStateDiagram robotStates
        robotTransitions
        (Just model.state.robotPosition)
        (Just model.transition.robotPosition)
    ]


view model =
    collage 1024
        1024
        ((viewFeeder model |> List.map (move ( 200, 350 )))
            ++ (viewRobot model |> List.map (move ( 0, 200 )))
        )


main =
    gameApp Tick
        { model = model
        , view = view
        , update = update
        }
