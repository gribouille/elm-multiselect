module Example1 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import MultiSelect


type alias Model =
  { state : MultiSelect.State
  }

type Msg
  = OnState MultiSelect.State


data : List String
data =
  [ "Lorem ipsum dolor sit amet"
  , "Quisque varius"
  , "Vivamus bibendum"
  , "Nullam faucibus"
  , "Nullam a dui aliquam"
  , "Cras sit amet"
  , "In a metus auctor"
  , "Mauris et tellus"
  , "Sed vel nunc"
  , "Aenean efficitur"
  ]


main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }


init : Model
init =
  { state = MultiSelect.init []
  }

view : Model -> Html Msg
view model =
  let
    config = MultiSelect.config "myid" OnState data
  in
    div [ class "ex-select" ]
    [ div []
      [ text <| "Values: " ++ String.join ", " (MultiSelect.values model.state)
      ]
    , MultiSelect.view config model.state
    ]


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnState s ->
       {model | state = s }
