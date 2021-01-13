module MultiSelect exposing
    ( Config, State, config, init, values
    , view
    )

{-| Multiselect component.


# Data

@docs Config, State, config, init, values


# View

@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (checked, class, for, id, placeholder, type_)
import Html.Events exposing (onCheck, onInput)
import List exposing (member)
import Maybe exposing (withDefault)


{-| Opaque type to manage the states of component.
-}
type State
    = State
        { search : Maybe String
        , selected : List String
        , index : Int
        }


{-| Get the selected values.
-}
values : State -> List String
values (State state) =
    state.selected


{-| Initialize the component state.
-}
init : List String -> State
init val =
    State { search = Nothing, selected = val, index = 0 }


{-| Opaque type to configure the component (construct with the `config` function).
-}
type Config msg
    = Config
        { pipe : State -> msg
        , items : List String
        , uid : String
        }


{-| Config constructor.
-}
config : String -> (State -> msg) -> List String -> Config msg
config uid pipe items =
    Config { pipe = pipe, items = items, uid = uid }


{-| Component view.
-}
view : Config msg -> State -> Html msg
view (Config c) (State state) =
    let
        onSearch =
            \s -> c.pipe <| State { state | search = Just s }

        onSelect =
            \s v ->
                c.pipe <|
                    if v then
                        State { state | selected = s :: state.selected }

                    else
                        State { state | selected = List.filter ((/=) s) state.selected }

        filter =
            case state.search of
                Nothing ->
                    Basics.identity

                Just s ->
                    List.filter (String.contains s)

        missing =
            List.filter (\s -> not <| member s c.items) state.selected

        items =
            List.map
                (\item -> viewItem c.uid onSelect (member item state.selected) item)
            <|
                filter (missing ++ c.items)
    in
    div [ class "multiselect" ]
        [ span
            [ class "search" ]
            [ input
                [ type_ "text"
                , class "input grm-filter"
                , Html.Attributes.value <| withDefault "" state.search
                , placeholder "Filter..."
                , onInput onSearch
                ]
                []
            ]
        , div [ class "items" ]
            [ ul [] items ]
        ]


viewItem : String -> (String -> Bool -> msg) -> Bool -> String -> Html msg
viewItem uid onSelect val item =
    div [ class "field item" ]
        [ p [ class "control" ]
            [ div [ class "b-checkbox is-info is-circular" ]
                [ input
                    [ id <| uid ++ item
                    , class "styled"
                    , checked val
                    , type_ "checkbox"
                    , onCheck (onSelect item)
                    ]
                    []
                , label [ for <| uid ++ item ] [ text item ]
                ]
            ]
        ]
