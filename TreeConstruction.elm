{- A state diagram representing the tree construction of an HTML document.
   The states represent the mode of the construction. The transitions represent
   received tokens. This diagram thus reflects the process occurrring after
   the input stream has been tokenized.

   See the HTML 5 spec for reference:
   https://www.w3.org/TR/html5/syntax.html#parsing-main-inhtml
-}


module Main exposing (..)

import List
import GraphicSVG exposing (..)
import StateDiagrams exposing (..)


type Msg
    = Tick Float GetKeyState


type Token
    = Comment
    | DOCTYPE
    | Space
    | Html
    | Head



-- The current mode of tree construction in our document.


type State
    = InitialInsertion
    | BeforeHtml
    | BeforeHead
    | InHead
    | InHeadNoscript
    | AfterHead
    | InBody
    | AfterBody
    | Text
    | InTable
    | InTableText
    | InCaption
    | InColumnGroup
    | InTableBody
    | InRow
    | InCell
    | InSelect
    | InTemplate
    | InFrameset


model =
    { tick = 0
    , state = InitialInsertion
    , transition =
        ( InitialInsertion, "" )
        -- Provide what is essentially a void transition, since one is expected.
    }


states =
    [ ( InitialInsertion, ( 0, 360 ) )
    , ( BeforeHtml, ( 0, 310 ) )
    , ( BeforeHead, ( 0, 260 ) )
    ]



-- Provide a List of tuples representing the following.
-- ( transition handler function, label, list of (state, label position) )


transitions =
    [ ( processVoidToken
      , "comment"
      , [ ( InitialInsertion, ( 120, 370 ) )
        , ( BeforeHtml, ( 120, 320 ) )
        , ( BeforeHead, ( 120, 270 ) )
        ]
      )
    , ( processDoctypeToken
      , "DOCTYPE"
      , [ ( InitialInsertion, ( 0, 337 ) )
        ]
      )
    , ( processHtmlToken
      , "html"
      , [ ( BeforeHtml, ( 0, 290 ) )
        ]
      )
    ]


processVoidToken : State -> State
processVoidToken t =
    t


processDoctypeToken : State -> State
processDoctypeToken t =
    case t of
        InitialInsertion ->
            BeforeHtml

        otherwise ->
            otherwise


processHtmlToken : State -> State
processHtmlToken t =
    case t of
        BeforeHtml ->
            BeforeHead

        otherwise ->
            otherwise


update msg model =
    model


view model =
    collage 1024 760 [ viewStateDiagram states transitions (Just model.state) (Just model.transition) ]


main =
    gameApp Tick
        { model = model
        , view = view
        , update = update
        }
