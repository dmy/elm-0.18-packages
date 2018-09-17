module Main exposing (main)

import Browser
import Html exposing (Html, a, div, h1, h2, input, li, node, p, span, text, ul)
import Html.Attributes exposing (autofocus, class, href, placeholder, rel, style)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (g, polygon, rect, svg)
import Svg.Attributes exposing (fill, height, points, stroke, strokeWidth, viewBox)


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
                div [ class "catalog" ]
                    [ h1 [] [ text "Service unavailable" ]
                    , span []
                        [ text "You could try "
                        , a [ href "https://www.google.com/search?tbs=cdr%3A1%2Ccd_max%3A8%2F20%2F2018&q=site%3Apackage.elm-lang.org" ]
                            [ text "searching on google" ]
                        , text " instead."
                        ]
                    ]

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
        [ logo
        , ul
            [ style "padding-left" "0"
            , style "margin-top" "20px"
            ]
            [ li []
                [ a [ href "https://web.archive.org/web/20180714175916/https://guide.elm-lang.org/" ]
                    [ text "Elm 0.18 Guide" ]
                ]
            , li [] [ a [ href "https://klaftertief.github.io/elm-search/" ] [ text "Fancy Search" ] ]
            ]
        , ul
            [ style "padding-left" "0"
            , style "margin-top" "20px"
            ]
            [ li []
                [ a [ href "https://package.elm-lang.org/" ]
                    [ text "Elm 0.19 Packages" ]
                ]
            ]
        ]


logo : Html msg
logo =
    div [ style "display" "flex" ]
        [ svg [ height "30", viewBox "0 0 600 600" ]
            [ g
                [ stroke "#fff", strokeWidth "20px" ]
                [ polygon [ fill "#7fd13bff", points "150,150 300,0 450,0 300,150" ] []
                , polygon [ fill "#f0ad00ff", points "0,300 150,150 150,300" ] []
                , polygon [ fill "#7fd13bff", points "150,150 300,150 300,300 150,300" ] []
                , polygon [ fill "#60b5ccff", points "300,150 600,150 450,300" ] []
                , polygon [ fill "#60b5ccff", points "300,150 600,450 300,450" ] []
                , polygon [ fill "#5a6378ff", points "0,300 300,300 300,600" ] []
                , polygon [ fill "#f0ad00ff", points "300,450 450,600 300,600" ] []
                ]
            ]
        , div
            [ style "color" "black"
            , style "padding-left" "8px"
            ]
            [ div [ style "line-height" "20px" ] [ text "elm 0.18" ]
            , div [ style "line-height" "10px", style "font-size" "0.85em" ] [ text "packages" ]
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
