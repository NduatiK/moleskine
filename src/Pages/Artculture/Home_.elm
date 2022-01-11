module Pages.Artculture.Home_ exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes exposing (style)
import Svg
import Svg.Attributes
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body =
        List.singleton <|
            layout
                [ Font.color white
                , Font.family [ Font.typeface "DM Sans" ]
                ]
            <|
                el
                    [ width fill
                    , height fill
                    , inFront navbar
                    , inFront sideScroller
                    , behindContent background
                    , inFront (el [ width fill, alignBottom ] footer)
                    ]
                    (row
                        [ centerY
                        , centerX
                        , moveDown 50
                        , moveRight 10

                        -- , moveRight 30
                        , spacing 400
                        ]
                        [ column
                            []
                            [ el [ width (px 1), height (px 200) ] none
                            , paragraph
                                [ above
                                    (el
                                        [ moveLeft 600
                                        , moveUp 40
                                        , Font.size 160
                                        , Font.family [ Font.typeface "Libre Baskerville" ]
                                        ]
                                        (text "Sculpture")
                                    )
                                , spacingXY 0 10
                                , Font.size 14
                                , Font.medium
                                , width (px 310)
                                ]
                                [ text "Presentation of new and future contemporary artists from around the world. A history surrounding many of the industry's most famous artisans" ]
                            , enterButton
                            ]
                        , column [ spacing 30 ]
                            [ column
                                [ spacingXY 0 4
                                , Font.size 34
                                ]
                                [ text "Discover"
                                , text "Art History"
                                ]
                            , paragraph
                                [ spacingXY 0 10
                                , Font.size 14
                                , Font.medium
                                , width (px 300)
                                ]
                                [ text "Discover and learn about art and sculptures. In this article we will describe the understanding of art and modern art and how they affect the lives of ordinary people. Check the article now." ]
                            ]
                        ]
                    )
    }


enterButton =
    let
        w =
            180

        h =
            75
    in
    el
        [ moveDown 60
        , alignRight
        , Font.size 14
        , Font.heavy
        , Font.letterSpacing 1.2
        , inFront
            (el
                [ height (px h)
                , width (px w)
                , moveUp (h / 2.4)
                , moveLeft (w / 2.8)
                , rotate (-pi / 11)
                ]
             <|
                html <|
                    Svg.svg
                        [ Svg.Attributes.width (String.fromInt w)
                        , Svg.Attributes.height (String.fromInt h)
                        , Svg.Attributes.viewBox ("-5 -5 " ++ String.fromInt (w + 10) ++ " " ++ String.fromInt (h + 10))
                        ]
                        [ Svg.ellipse
                            [ Svg.Attributes.rx (String.fromInt (w // 2))
                            , Svg.Attributes.ry (String.fromInt (h // 2))
                            , Svg.Attributes.cx (String.fromInt (w // 2))
                            , Svg.Attributes.cy (String.fromInt (h // 2))
                            , Svg.Attributes.fill "transparent"
                            , Svg.Attributes.stroke "#ff5722"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeOpacity "0.4"
                            ]
                            []
                        ]
            )
        , inFront
            (el
                [ height fill
                , width fill
                , centerX
                , centerY
                , alpha 0
                , mouseOver
                    [ alpha 0.4
                    ]
                ]
                (el
                    [ height (px 6)
                    , width fill
                    , moveDown 2
                    , moveLeft 2
                    , alignBottom
                    , htmlAttribute (style "background-color" "#ff5722")
                    , htmlAttribute (style "z-index" "-10")
                    ]
                    none
                )
            )

        -- Svg.ellipse
        --     [ Svg.Attributes.r "50" ]
        --     []
        ]
        (text "ENTER")


navbar =
    row
        [ width fill
        , paddingXY 60 40
        , Font.bold
        , Font.size 13
        , Font.letterSpacing 0.4
        ]
        [ text "ARTCULTURE"
        , row
            [ alignRight, spacing 30 ]
            [ text "SHOP"
            , text "PORTFOLIO"
            , text "ABOUT US"
            ]
        ]


sideScroller =
    column
        [ paddingXY 60 40
        , Font.bold
        , Font.size 13
        , Font.letterSpacing 0.4
        , Font.color (gray 0.7)
        , centerY
        , spacing 16
        , alignRight
        ]
        [ el [ centerX ] (text "01")
        , el [ centerX, width (px 50), height (px 1), Background.color (gray 0.4) ] none
        , el [ centerX, Font.size 24, Font.heavy, Font.color white ] (text "02")
        , el [ centerX, width (px 50), height (px 1), Background.color (gray 0.4) ] none
        , el [ centerX ] (text "03")
        ]


background =
    let
        bgText =
            el
                [ Font.bold
                , Font.size 450
                , Font.letterSpacing 20
                , alpha 0.1
                , centerY
                , centerX
                , moveRight 120
                ]
                (text "noise")

        bgImage =
            el
                [ centerY
                , centerX
                , width (px 450)
                , height (px 587)
                , Background.gradient
                    { angle = 0
                    , steps = [ gray 0.3, gray 0.2 ]
                    }
                , Border.rounded 450
                , clip
                ]
                (image [ width (px 450) ]
                    { src = "sculpture.png"
                    , description = "A sculture of a roman emperor"
                    }
                )
    in
    el
        [ width fill
        , height fill
        , clip
        , inFront bgText
        , inFront bgImage
        ]
        none


footer =
    row
        [ width fill
        , paddingXY 60 40
        , Font.extraBold
        , Font.size 13
        , Font.letterSpacing 0.4
        ]
        [ row
            [ spacing 30 ]
            [ el [] (text "FB")
            , el [] (text "/")
            , el [] (text "TW")
            , el [] (text "/")
            , el [] (text "IG")
            ]
        , column
            [ alignRight, spacing 3 ]
            [ el [ Background.color white, height (px 2), width (px 36) ] none
            , el [ Background.color white, height (px 2), width (px 36) ] none
            , el [ Background.color white, height (px 2), width (px 24), alignRight ] none
            ]
        ]



-- COLORS


white =
    rgb 1 1 1


gray intensity =
    rgb intensity intensity intensity
