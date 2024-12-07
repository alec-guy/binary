module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (..)
import Http
import Json.Decode as D 
import Json.Encode as E



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model = 
                 { failure   : Bool 
                 , loading   : Bool 
                 , success   : Maybe Bin 
                 , textInput : String 
                 }

initModel : Model 
initModel = { failure   = False
            , loading   = False
            , success   = Nothing
            , textInput = ""
            }

type alias Bin =
  { binNum     : String
  , decimal    : String 
  , comp : String
  }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel, getBin "")

-- UPDATE

type Msg
  = Submit
  | TextInput String
  | ButtonType ButtonTypee
  | GotBin (Result Http.Error Bin)

type ButtonTypee = Erase 
                 | Backspace

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Submit ->
      ({model | loading = True}, getBin model.textInput)
    TextInput s -> 
      ({model | loading = False, textInput = model.textInput ++ s}, Cmd.none)

    GotBin result ->
      case result of
        Ok bin ->
          ({model | loading = False, failure = False, success = Just bin}, Cmd.none)

        Err _ ->
          ({model | loading = False, failure = True}, Cmd.none)
    (ButtonType butt) -> 
          case butt of 
          Erase     -> 
           (initModel, Cmd.none)
          Backspace -> 
           ( { model | loading = False
             , textInput = String.dropRight 1 model.textInput
             }
           , Cmd.none 
           )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [class "div-1"]
    [ h2 [] [ text "Binary Number Tool" ]
    , defaultPage model 
    , br [] []
    , displayFailure model.failure 
    , br [] []
    , displayLoading model.loading
    , br [] [] 
    , displaySuccess model.success 
    ]

defaultPage : Model -> Html Msg 
defaultPage model = div 
                  [class "div-2"]
                  [ button [ onClick Submit] [ text "Submit" ]
                  , br [] []
                  , div 
                    [class "div-3"] 
                    [ button [onClick <| TextInput "1"] [          text "1"]
                    , button [onClick <| TextInput "0"]            [text "0"]
                    , button [onClick <| ButtonType Backspace]     [text "<-"]
                    , button [onClick <| ButtonType Erase]         [text "Restart"]
                    , br [] []
                    , text <| "Your input: " ++ model.textInput
                    ]
                  ]
displayFailure : Bool -> Html Msg 
displayFailure b = 
     case b of 
      True  -> 
        div [] [text "I could not process your request"]
      False -> 
        text ""

displayLoading : Bool -> Html Msg 
displayLoading b = 
    case b of 
     True  -> 
       div [] [text "Loading"]    
     False -> text ""
displaySuccess : Maybe Bin -> Html Msg 
displaySuccess mb =  
      case mb of 
       Nothing ->  
         text ""
       (Just r) -> 
         div [] [text <| "Binary: " ++ r.binNum
                ,br [] []
                ,text <| "Decimal: " ++ r.decimal
                ,br [] []
                ,text <| "Complement: " ++ r.comp
                ]

-- HTTP


getBin : String -> Cmd Msg
getBin bin =
  Http.post 
    { url = "/submit"
    , body = Http.jsonBody <| ourRequestValue bin 
    , expect = Http.expectJson GotBin responseDecoder 
    }

ourRequestValue : String -> E.Value 
ourRequestValue bin = E.object [("binNum", E.string bin)]

responseDecoder : D.Decoder Bin
responseDecoder =
   D.map3
   Bin 
   (D.field "binaryNum" D.string) 
   (D.field "decimal" D.string)
   (D.field "comp" D.string)

   