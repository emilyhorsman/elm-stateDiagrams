{- A state diagram representing the gameplay of a robot in the FIRST
   Robotics Competition game, Rebound Rumble.

   The robot’s goal is to score points by firing basketballs into hoops.
-}


module Main exposing (..)

import String exposing (concat)
import List
import GraphicSVG exposing (..)
import StateDiagrams exposing (..)


type Msg
    = Tick Float GetKeyState


type Robot
    = Neutral
    | Collecting
    | Moving
    | MacroAiming
    | MicroAiming
    | Shooting
    | Priming
    | Firing
    | GameOver



-- Robot State


macroToMicro state =
    if state == MacroAiming then
        MicroAiming
    else
        state


startAiming state =
    if state == Neutral then
        MacroAiming
    else
        state


joystickInput state =
    Moving


stoppedJoystickInput state =
    if state == Moving then
        Neutral
    else
        state


timerEnds state =
    case state of
        Firing ->
            Neutral

        otherwise ->
            GameOver


startCollecting state =
    if state == Neutral then
        Collecting
    else
        state


stopCollecting state =
    if state == Collecting then
        Neutral
    else
        state


aimed state =
    if state == MicroAiming then
        Neutral
    else
        state


firingSequence capacity state =
    if capacity > 0 then
        case state of
            Neutral ->
                Shooting

            Shooting ->
                Priming

            Priming ->
                Firing

            Firing ->
                Shooting

            otherwise ->
                otherwise
    else
        Neutral


states =
    [ ( GameOver, ( -450, 0 ) )
    , ( Neutral, ( -350, 0 ) )
    , ( MacroAiming, ( -200, 75 ) )
    , ( MicroAiming, ( -250, 125 ) )
    , ( Collecting, ( -50, 0 ) )
    , ( Moving, ( 100, -25 ) )
    , ( Shooting, ( 225, -10 ) )
    , ( Priming, ( 350, -10 ) )
    , ( Firing, ( 350, 80 ) )
    ]


transitions =
    [ ( timerEnds
      , "timer ends"
      , [ ( Neutral, ( -400, 15 ) )
        , ( MacroAiming, ( -425, 50 ) )
        , ( MicroAiming, ( -430, 75 ) )
        , ( Moving, ( -300, -60 ) )
        , ( Shooting, ( -150, -75 ) )
        , ( Priming, ( 0, -75 ) )
        ]
      )
    , ( macroToMicro
      , "pointed towards"
      , [ ( MacroAiming, ( -180, 105 ) )
        ]
      )
    , ( startAiming
      , "aim"
      , [ ( Neutral, ( -290, 55 ) )
        ]
      )
    , ( aimed
      , "aimed"
      , [ ( MicroAiming, ( -350, 40 ) )
        ]
      )
    , ( stoppedJoystickInput
      , "joystick released"
      , [ ( Moving, ( -200, -30 ) )
        ]
      )
    , ( startCollecting
      , "collect"
      , [ ( Neutral, ( -200, -10 ) )
        ]
      )
    , ( stopCollecting
      , "collected"
      , [ ( Collecting, ( -200, 10 ) )
        ]
      )
    , ( joystickInput
      , "joystick"
      , [ ( Neutral, ( -100, 30 ) )
        ]
      )
    , ( firingSequence 1
      , "shoot"
      , [ ( Neutral, ( 0, 50 ) )
        ]
      )
    , ( firingSequence 1
      , "starting motors"
      , [ ( Shooting, ( 285, 10 ) )
        ]
      )
    , ( firingSequence 1
      , "shooting speed reached"
      , [ ( Priming, ( 375, 40 ) )
        ]
      )
    , ( firingSequence 1
      , "released"
      , [ ( Firing, ( 200, 40 ) )
        ]
      )
    , ( firingSequence 0
      , "out of capacity"
      , [ ( Firing, ( 0, 80 ) )
        ]
      )
    ]


type alias State =
    Robot


model =
    { tick = 0
    , prevTick = 0
    , holding = 2
    , state = Neutral
    , x = 0
    , y = 0
    , dir = 0
    , joystick = ( 0, 0 )
    , transition =
        ( Neutral, "" )
        -- Provide what is essentially a void transition, since one is expected.
    }


moveRobot ( dX, dY ) model =
    let
        t =
            model.tick - model.prevTick

        factor =
            100

        delta =
            factor * t * dY
    in
        { model
            | y = model.y + (delta * (cos model.dir))
            , x = model.x + (delta * -(sin model.dir))
            , dir = model.dir - (t * dX)
            , state =
                if dY == 0 then
                    stoppedJoystickInput model.state
                else
                    joystickInput model.state
        }



-- Game


tickHandler t state =
    if t >= 90 then
        GameOver
    else
        state


update msg model =
    case msg of
        Tick t ( getKeyState, joystick, _ ) ->
            { model
                | tick = min 90 t
                , prevTick = model.tick
                , joystick = joystick
                , state = tickHandler t model.state
            }
                |> moveRobot joystick


viewRobot model =
    [ viewStateDiagram states
        transitions
        (Just model.state)
        (Just model.transition)
    ]


drawRobot model =
    let
        colour =
            pink

        wheel =
            roundedRect 6 15 3 |> outlined (solid 1) colour
    in
        group
            [ square 50 |> outlined (solid 1) colour
            , triangle 5 |> filled colour |> rotate (pi / 2) |> move ( 0, 20 )
            , wheel |> move ( -25, 10 )
            , wheel |> move ( -25, -10 )
            , wheel |> move ( 25, 10 )
            , wheel |> move ( 25, -10 )
            ]
            |> move ( model.x, model.y )
            |> rotate model.dir


viewGame model =
    [ rectangle 300 500 |> outlined (solid 2) black
    , drawRobot model
    ]


displayTimer model =
    text ("Time: " ++ (model.tick |> round |> toString))
        |> filled black
        |> scale 2


displayPosition model =
    text (concat [ model.x |> round |> toString, ", ", toString model.y, " @ ", toString model.dir ])
        |> filled black
        |> scale 2


view model =
    collage 1024
        1024
        ([ displayTimer model |> move ( -500, 100 )
         , displayPosition model |> move ( -500, 50 )
         ]
            ++ (viewRobot model |> List.map (move ( 0, 300 )))
            ++ (viewGame model |> List.map (move ( 200, -200 )))
        )


main =
    gameApp Tick
        { model = model
        , view = view
        , update = update
        }
