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
    | EOF
    | DOCTYPE
    | Space
    | StartHtml
    | EndHtml
    | StartHead
    | EndHead
    | StartBody
    | EndBody



-- The current mode of tree construction in our document.


type State
    = DownloadedInputStream
    | Tokenized
    | InitialInsertion
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
    | AfterAfterBody
    | Constructed


model =
    { tick = 0
    , state = InitialInsertion
    , transition =
        ( InitialInsertion, "" )
        -- Provide what is essentially a void transition, since one is expected.
    }


states =
    [ ( DownloadedInputStream, (-200, 360) )
    , ( Tokenized, (-200, 310) )
    , ( Constructed, (200, 360) )
    , ( InitialInsertion, ( 0, 360 ) )
    , ( BeforeHtml, ( 0, 310 ) )
    , ( BeforeHead, ( 0, 260 ) )
    , ( InHead, ( -200, 260 ) )
    , ( AfterHead, ( -200, 210 ) )
    , ( Text, (-200, -200) )
    , ( InTemplate, (0, 210) )
    , ( InFrameset, (0, 160) )
    , ( InTable, (0, 130) )
    , ( InSelect, (0, 100) )
    , ( InBody, (-200, 160) )
    , ( AfterBody, (-200, 110) )
    ]



-- Provide a List of tuples representing the following.
-- ( transition handler function, label, list of (state, label position) )


transitions =
    [ ( tokenize
      , "tokenize"
      , [ ( DownloadedInputStream, ( -200, 337 ) )
        ]
      )
    , ( beginTreeConstruction
      , "begin tree"
      , [ ( Tokenized, ( -120, 330 ) )
        ]
      )
    , ( processVoidToken
      , "comment"
      , [ ( InitialInsertion, ( 100, 370 ) )
        , ( BeforeHtml, ( 90, 320 ) )
        , ( BeforeHead, ( 90, 270 ) )
        ]
      )
    , ( processDoctypeToken
      , "<!DOCTYPE>"
      , [ ( InitialInsertion, ( 0, 337 ) )
        ]
      )
    , ( processStartHtmlToken
      , "<html>"
      , [ ( BeforeHtml, ( 0, 290 ) )
        ]
      )
    , ( processStartHeadToken
      , "<head>"
      , [ ( BeforeHead, ( -100, 250 ) )
        ]
      )
    , ( processEndHeadToken
      , "</head>"
      , [ ( InHead, ( -200, 237 ) )
        ]
      )
    , ( processAnyStartTag
      , "valid start"
      , [ ( BeforeHead, ( -100, 270 ) )
        , ( InHead, ( -270, 237 ) )
        ]
      )
    , ( processStartScriptTag
      , "<script>"
      , [ ( InHead, (-300, 50) )
        ]
      )
    , ( processStartTemplateTag
      , "<template>"
      , [ ( InHead, (-100, 220 ) )
        ]
      )
    , ( processStartBodyTag
      , "<body>"
      , [ ( AfterHead, ( -200, 187) )
        ]
      )
    , ( processEndBodyTag
      , "</body>"
      , [ ( InBody, (-200, 137) )
        ]
      )
    , ( processStartFramesetTag
      , "<frameset>"
      , [ ( InBody, ( -110, 160 ) )
        ]
      )
    , ( processStartTableTag
      , "<table>"
      , [ ( InBody, ( -110, 140) )
        ]
      )
    , ( processStartSelectTag
      , "<select>"
      , [ ( InBody, (-110, 120) )
        ]
      )
    , ( processStartTextareaTag
      , "<textarea>"
      , [ ( InBody, (-270, 0) )
        ]
      )
    ]


tokenize : State -> State
tokenize t =
    case t of
        DownloadedInputStream ->
            Tokenized

        otherwise ->
            otherwise


beginTreeConstruction : State -> State
beginTreeConstruction t =
    case t of
        Tokenized ->
            InitialInsertion

        otherwise ->
            otherwise


processEndOfFileToken : State -> State
processEndOfFileToken t = Constructed

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


processStartHtmlToken : State -> State
processStartHtmlToken t =
    case t of
        BeforeHtml ->
            BeforeHead

        otherwise ->
            otherwise


processEndHtmlToken : State -> State
processEndHtmlToken t =
    case t of
        AfterBody ->
            AfterAfterBody

        otherwise ->
            otherwise


processStartHeadToken t =
    case t of
        BeforeHead ->
            InHead

        otherwise ->
            otherwise

processEndHeadToken t =
    case t of
        InHead ->
            AfterHead

        otherwise ->
            otherwise

processStartBodyTag t =
    case t of
        AfterHead ->
            InBody

        otherwise ->
            otherwise

processEndBodyTag t =
    case t of
        InBody ->
            AfterBody

        otherwise ->
            otherwise

processStartScriptTag t =
    case t of
        InHead ->
            Text

        otherwise ->
            otherwise

processStartTemplateTag t =
    case t of
        InHead ->
            InTemplate

        otherwise ->
            otherwise

processStartTextareaTag t =
    case t of
        InBody ->
            Text

        otherwise ->
            otherwise

processStartFramesetTag t =
    case t of
        InBody ->
            InFrameset

        otherwise ->
            otherwise

processStartTableTag t =
    case t of
        InBody ->
            InTable

        otherwise ->
            otherwise

processStartSelectTag t =
    case t of
        InBody ->
            InSelect

        otherwise ->
            otherwise

processAnyStartTag t =
    case t of
        BeforeHead ->
            InHead

        InHead ->
            AfterHead

        otherwise ->
            otherwise


update msg model =
    model


view model =
    collage 1024 760
        [ viewStateDiagram states transitions (Just model.state) (Just model.transition)
        , rectangle 220 80 |> outlined (dashed 1) black |> move (-190, 335)
        , text "“valid start | end” refers to an any other unhandled, valid start or end tag token" |> filled black |> move (-350, -350)
        ]


main =
    gameApp Tick
        { model = model
        , view = view
        , update = update
        }
