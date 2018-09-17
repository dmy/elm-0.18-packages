module Main exposing (main)

import Browser
import Html exposing (Html, a, div, h1, h2, input, li, node, p, span, text, ul)
import Html.Attributes exposing (autofocus, class, href, placeholder, rel)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder)


type alias Model =
    { packages : List Package
    , error : Maybe Http.Error
    , search : String
    }


type Msg
    = Packages (Result Http.Error (List Package))
    | Search String


type alias Package =
    { name : String
    , summary : String
    , version : String
    }


getPackages : Cmd Msg
getPackages =
    Http.send Packages <|
        Http.get
            "https://cors-anywhere.herokuapp.com/http://package.elm-lang.org/all-packages?elm-package-version=0.18"
            packagesDecoder


packagesDecoder : Decoder (List Package)
packagesDecoder =
    Decode.list <|
        Decode.map3 Package
            (Decode.field "name" Decode.string)
            (Decode.field "summary" Decode.string)
            (Decode.field "versions" (Decode.index 0 Decode.string))


view : Model -> Html Msg
view model =
    div [ class "center" ]
        [ case model.error of
            Just error ->
                text "Service unavailable."

            Nothing ->
                div [ class "catalog" ]
                    [ input
                        [ placeholder "Search Elm 0.18 packages"
                        , autofocus True
                        , onInput Search
                        ]
                        []
                    , catalog model
                    ]
        , sidebar
        , footer
        ]


sidebar : Html msg
sidebar =
    div [ class "catalog-sidebar" ]
        [ h2 [] [ text "Resources" ]
        , ul []
            [ li []
                [ a [ href "https://web.archive.org/web/20180714175916/https://guide.elm-lang.org/" ]
                    [ text "Elm 0.18 Guide" ]
                ]
            , li [] [ a [ href "https://klaftertief.github.io/elm-search/" ] [ text "Fancy Search" ] ]
            ]
        ]


footer : Html msg
footer =
    div [ class "footer" ]
        [ text "All code for this page "
        , a [ class "grey-link", href "https://github.com/dmy/elm-0.18-packages/" ]
            [ text "is open source" ]
        , text " and written in Elm. Thank you "
        , a [ class "grey-link", href "http://elm-lang.org/" ]
            [ text "Elm" ]
        , text ", "
        , a [ class "grey-link", href "https://web.archive.org" ]
            [ text "Internet Archive" ]
        , text " and "
        , a [ class "grey-link", href "https://cors-anywhere.herokuapp.com" ]
            [ text "Cors Anywhere" ]
        , text "."
        ]


catalog : Model -> Html msg
catalog model =
    let
        search =
            if String.isEmpty model.search then
                "elm-lang/"

            else
                model.search
    in
    div []
        (model.packages
            |> List.filter (\p -> String.contains search p.name)
            |> List.map package
        )


package : Package -> Html msg
package pkg =
    let
        ( author, name ) =
            case String.split "/" pkg.name of
                [ user, repo ] ->
                    ( user ++ "/", repo )

                _ ->
                    ( "", pkg.name )
    in
    div [ class "pkg-summary" ]
        [ div []
            [ h1 []
                [ a [ href (url pkg) ]
                    [ span [ class "light" ] [ text author ]
                    , text name
                    ]
                ]
            , span [ class "pkg-summary-hints" ] [ text pkg.version ]
            ]
        , p [ class "pkg-summary-desc" ] [ text pkg.summary ]
        ]


url : Package -> String
url pkg =
    "https://package.elm-lang.org/packages/" ++ pkg.name ++ "/" ++ pkg.version


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search string ->
            ( { model | search = string }
            , Cmd.none
            )

        Packages result ->
            case result of
                Ok packages ->
                    ( { model | packages = packages }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | error = Just error }
                    , Cmd.none
                    )


main : Program () Model Msg
main =
    Browser.element
        { init = always ( Model [] Nothing "", getPackages )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
