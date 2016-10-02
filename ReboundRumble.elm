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


type AimingState
    = Checking
    | MacroAdjusting
    | MicroAdjusting


type Robot
    = Neutral
    | Collecting
    | Aiming AimingState
    | Shooting
    | FeedingBall



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
      , "collecting started"
      , [ ( Off, ( -25, 40 ) )
        ]
      )
    , ( collectedBalls
      , "shooting complete"
      , [ ( On Forward, ( 25, 60 ) )
        ]
      )
    ]

-- Robot State


startRobotGame state =
    case state of
        Neutral ->
            Collecting

        otherwise ->
            otherwise

autonomousTimerEnds state = Neutral

reachedCapacity state =
    case state of
        Collecting ->
            Aiming Checking

        otherwise ->
            otherwise

collectingTimerEnds state =
    case state of
        Collecting ->
            Aiming Checking

        otherwise ->
            otherwise

speedReached state =
    case state of
        Shooting ->
            FeedingBall

        otherwise ->
            otherwise

shootingTimerEnds state =
    case state of
        FeedingBall ->
            Shooting

        otherwise ->
            otherwise

depletedCapacity state =
    case state of
        Shooting ->
            Collecting

        otherwise ->
            otherwise

robotStates =
    [ ( Neutral, (0, 0) )
    , ( Collecting, (0, -100) )
    , ( Aiming Checking, (100, -250) )
    , ( Aiming MacroAdjusting, (-100, -300) )
    , ( Aiming MicroAdjusting, (-100, -200) )
    , ( Shooting, (0, -400) )
    , ( FeedingBall, (0, -500) )
    ]


robotTransitions =
    [ ( startRobotGame
      , "start game"
      , [ ( Neutral, ( 0, -40 ) )
        ]
      )
    , ( autonomousTimerEnds
      , "autonomous timer ends"
      , [ ( Collecting, ( -150, -40 ) )
        , ( Aiming Checking, ( 200, -150 ) )
        , ( Shooting, ( 350, -200 ) )
        ]
      )
    , ( speedReached
      , "speed reached"
      , [ ( Shooting, ( 100, -450 ) )
        ]
      )
    , ( shootingTimerEnds
      , "shooting timer ends"
      , [ ( FeedingBall, ( -100, -450 ) )
        ]
      )
    , ( depletedCapacity
      , "capacity depleted"
      , [ ( Shooting, ( -300, -200 ) )
        ]
      )
    ]


type alias State =
    { feeder : Feeder
    , robot : Robot
    }


model =
    { tick = 0
    , state =
        { feeder = Off
        , robot = Neutral
        }
    , transition =
        { feeder = ( Off, "" )
        , robot = ( Neutral, "" )
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
    , rectangle 200 150 |> outlined (dashed 1) black |> move ( 0, 50 )
    , text "Feeder" |> filled black |> move ( -95, 110 )
    ]


viewRobot model =
    [ viewStateDiagram robotStates
        robotTransitions
        (Just model.state.robot)
        (Just model.transition.robot)
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
