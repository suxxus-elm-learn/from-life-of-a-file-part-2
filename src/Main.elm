module Main exposing (main)

import BoundedSet as SF
import Browser
import Html exposing (Html, div, h3, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, for, id, type_)
import Html.Events exposing (onClick)
import List


type alias Fruits =
    List String


type alias Fruit =
    String


type Msg
    = Select String
    | Deselect String


type alias Model =
    { fruits : Fruits
    , selected : SF.BoundedSet String
    }


selectFruit : SF.BoundedSet String -> Fruit -> Msg
selectFruit selectedFruit fruit =
    let
        isChecked =
            SF.member fruit selectedFruit
    in
    if isChecked then
        Deselect fruit

    else
        Select fruit


doFruitsCheckbox : SF.BoundedSet String -> Fruits -> List (Html Msg)
doFruitsCheckbox selectedFruit =
    List.map
        (\fruit ->
            li [ class "ph3 tl pv2 bb b--light-silver pointer" ]
                [ input
                    [ type_ "checkbox"
                    , onClick <| selectFruit selectedFruit fruit
                    , checked <|
                        SF.member
                            fruit
                            selectedFruit
                    , id fruit
                    ]
                    []
                , label [ for fruit, class "ph3" ] [ text fruit ]
                ]
        )


initialModel : Model
initialModel =
    { fruits = [ "mango", "banana", "apple", "orange", "apricot" ]
    , selected = SF.empty 2
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select fruit ->
            { model | selected = SF.insert fruit model.selected }

        Deselect fruit ->
            { model | selected = SF.remove fruit model.selected }


view : Model -> Html Msg
view model =
    div []
        [ h3 [ class "f3 f3-m " ] [ text "Hello, select 2 fruits please" ]
        , ul
            [ class "list pl0 ml0 center mw5 ba b--light-silver br3"
            ]
          <|
            doFruitsCheckbox
                model.selected
                model.fruits
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
