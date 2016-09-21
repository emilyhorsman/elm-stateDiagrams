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

type Msg = Tick Float GetKeyState

type State = Red | Yellow | Green

{-
    Now we define a simplified initial Model. We use a record type
    with 3 fields to keep track of the state.

    *tick       simplified measurement of some change in time
    *state      the current state of a trafficlight
    *transition the transition leading to the current state
-}

model = {
          tick = 0
        , state =  Green
        , transition = (Red, "change")
        }

{-
    "states" and "transitions" contain what the StateDiagrams
    library will draw to the screen. Both of these lists contain
    something names to draw to the screen and the locations of
    where they will be drawn
-}

states =
    [
         (Red,    (0, 200))
    ,    (Yellow, (0, 0))
    ,    (Green,  (0, -200))
    ]
transitions =
    [
        (updateState, "change",
        [((Red),    (100,  0))
        ,((Yellow), (-100,  100))
        ,((Green),  (-100, -100))])
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
updateState t = case t of
    Green -> Yellow
    Yellow -> Red
    Red -> Green

{-
    An update function is required to change the model at discrete
    time intervals. (The specifics of which you do not have to worry about)

    In this case, we update the state every 16th ticks.
-}

update msg model = case msg of
                  Tick t getKeyState -> if model.tick >= 16 then
                  { model | tick = 0, state = (updateState model.state), transition = (model.state, "change") }
                  else
                  { model | tick = model.tick + 1, state = model.state, transition = model.transition }

{-
    the view function produces a statediagram based on the
    model passed.
-}

view model = collage 768 760 [viewStateDiagram states transitions (Just model.state) (Just model.transition)]

{-
    general way to write a gameApp 
-}



main = gameApp Tick {
                          model = model,
                          view = view,
                          update = update
                        }
