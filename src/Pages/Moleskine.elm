module Pages.Moleskine exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import FeatherIcons
import Gen.Params.Moleskine exposing (Params)
import Html
import Html.Attributes exposing (class, id, style)
import Page
import Process
import Request
import Shared
import Svg
import Svg.Attributes as SvgA
import Task
import Time
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { selectedColor : Color
    , selectedRuling : Ruling
    , loopingRulings : Bool
    , lastRuling : Maybe Ruling
    , offset : { currentIndex : Int, percent : Float }
    }


type alias Image =
    Int


images =
    List.range 0 2


init : ( Model, Cmd Msg )
init =
    ( { selectedColor = Black
      , loopingRulings = True
      , selectedRuling = Ruled
      , lastRuling = Nothing
      , offset = { currentIndex = 0, percent = 0.4 }
      }
    , switchRulingCmd
    )


switchRulingCmd =
    Process.sleep 800
        |> Task.attempt (\_ -> NextRuling)



-- UPDATE


type Msg
    = Tick
    | SelectRuling Ruling
    | SelectColor Color
    | NextRuling


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            let
                { currentIndex, percent } =
                    model.offset
            in
            if percent >= 1 then
                ( { model | offset = { percent = 0, currentIndex = remainderBy (List.length images) (currentIndex + 1) } }
                , Cmd.none
                )

            else
                ( { model | offset = { percent = percent + 0.015, currentIndex = currentIndex } }
                , Cmd.none
                )

        SelectRuling ruling ->
            ( { model | selectedRuling = ruling }
            , Cmd.none
            )

        SelectColor color ->
            ( { model | selectedColor = color }
            , Cmd.none
            )

        NextRuling ->
            if model.loopingRulings && model.lastRuling /= Just Dot then
                ( { model
                    | lastRuling = Just model.selectedRuling
                    , selectedRuling = nextRuling (Maybe.withDefault Dot model.lastRuling)
                  }
                , switchRulingCmd
                )

            else
                ( { model | loopingRulings = False }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 100 (\_ -> Tick)
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Homepage"
    , body =
        List.singleton <|
            layout
                [ Font.color (gray 1)
                , Font.family
                    [ Font.typeface "DM Sans"
                    ]
                , Font.letterSpacing 0.1
                , Font.regular
                , height fill
                ]
            <|
                el
                    [ width fill
                    , style "min-height" "100vh"
                        |> htmlAttribute
                    , padding 14
                    , Background.color (rgba 1 1 1 0.4)
                    , behindContent background
                    ]
                    (column
                        [ Background.color (rgba 1 1 1 0.8)
                        , width fill
                        , height fill
                        , paddingEach
                            { top = 10
                            , left = 50
                            , right = 60
                            , bottom = 80
                            }
                        ]
                        [ navbar
                        , column
                            [ paddingEach
                                { top = 10
                                , left = 100
                                , right = 0
                                , bottom = 0
                                }
                            , spacing 30
                            , width fill
                            , height fill
                            ]
                            [ row [ spacing 10, Font.size 14, Font.color (gray 0.3), Font.regular ]
                                [ el [] (text "Home")
                                , icon FeatherIcons.chevronRight 16 []
                                , el [] (text "Notebooks")
                                , icon FeatherIcons.chevronRight 16 []
                                , el [ Font.color (gray 1) ] (text "The Classic Notebook")
                                ]
                            , row
                                [ width fill
                                , height fill
                                ]
                                [ bookDetails model
                                , bookImage model.offset model.selectedRuling
                                ]
                            ]
                        ]
                    )
    }


background =
    let
        triangle =
            Svg.polygon
                [ SvgA.fill "rgb(134, 125, 122)"
                , SvgA.preserveAspectRatio "none"
                , SvgA.points "700 0 1200 0 1200 800 1100 800"
                ]
                []

        topStrip =
            Svg.polygon
                [ SvgA.fill "rgb(252 102 59)"
                , SvgA.preserveAspectRatio "none"
                , SvgA.height "100%"
                , SvgA.points "0 0 1200 0 1200 14 0 14"
                ]
                []

        bottomStrip =
            Svg.polygon
                [ SvgA.fill "rgb(252 102 59)"
                , SvgA.points "0 786 1200 786 1200 800 0 800"
                ]
                []

        leftStrip =
            Svg.polygon
                [ SvgA.fill "rgb(252 102 60)"
                , SvgA.height "100%"
                , SvgA.points "0 0 0 800 13 800 13 0"
                ]
                []
    in
    row
        [ width fill
        , height fill
        , style "max-height" "100%"
            |> htmlAttribute
        ]
        [ html <|
            Svg.svg
                [ SvgA.viewBox "0 0 1200 800"
                , SvgA.width "100%"
                , SvgA.height "100%"
                , SvgA.preserveAspectRatio "none"
                ]
                [ Svg.polygon
                    [ SvgA.fill "white"
                    , SvgA.points "0 0 1200 0 1200 800 0 800"
                    , SvgA.opacity "0.8"
                    ]
                    []
                , topStrip
                , leftStrip
                , bottomStrip
                , triangle
                ]
        ]


bookDetails model =
    column
        [ paddingXY 0 10
        , width fill
        , spacing 30
        ]
        [ textColumn
            [ Font.letterSpacing -0.4
            , Font.bold
            , Font.family [ Font.typeface "Libre Baskerville" ]
            , Font.size 50
            ]
            [ paragraph [] [ text "The Classic" ]
            , paragraph [] [ text "Notebook" ]
            ]
        , textColumn
            [ spacing 4
            , Font.color (gray 0.7)
            , Font.size 15
            ]
            [ paragraph [] [ text "The iconic, legendary notebook used by artists, writers and" ]
            , paragraph [] [ text "thinkers over the past 2 centuries" ]
            ]
        , selectColor model.selectedColor
        , selectRuling model.selectedRuling
        , pricing
        ]


selectColor selectedColor =
    column
        [ width fill
        , spacing 12
        , paddingEach
            { top = 24
            , right = 0
            , bottom = 0
            , left = 0
            }
        ]
        [ el [ Font.bold, Font.size 13, Font.letterSpacing 0.8 ] (text ((colorToText >> String.toUpper) selectedColor))
        , row [ spacing 16 ]
            [ circle Black selectedColor
            , circle Navy selectedColor
            , circle Brown selectedColor
            , circle Pink selectedColor
            , circle Blue selectedColor
            ]
        ]


type Color
    = Black
    | Navy
    | Brown
    | Pink
    | Blue


colorToColor color =
    case color of
        Black ->
            rgb255 30 28 26

        Navy ->
            rgb255 0 51 73

        Brown ->
            rgb255 94 78 65

        Pink ->
            rgb255 202 119 122

        Blue ->
            rgb255 107 175 186


colorToText color =
    case color of
        Black ->
            "Black"

        Navy ->
            "Navy"

        Brown ->
            "Brown"

        Pink ->
            "Pink"

        Blue ->
            "Blue"


circle color_ selectedColor =
    let
        selected =
            selectedColor == color_

        color =
            colorToColor color_
    in
    el
        (if selected then
            [ padding 3
            , height (px 38)
            , width (px 38)
            , Background.color white
            , Border.color color
            , Border.width 2
            , Border.rounded 100
            , centerY
            ]

         else
            [ padding 0
            , height (px 36)
            , width (px 36)
            , Background.color color
            , Border.rounded 100
            , centerY
            , onClick (SelectColor color_)
            ]
        )
        (el
            [ Background.color color
            , height fill
            , width fill
            , Border.rounded 100
            ]
            none
        )


selectRuling selectedRuling =
    column
        [ width fill
        , spacing 12
        ]
        [ el [ Font.bold, Font.size 13, Font.letterSpacing 0.8 ] ((rulingToText >> String.toUpper >> text) selectedRuling)
        , row [ spacing 16 ]
            [ rulingSquare Dot selectedRuling
            , rulingSquare Ruled selectedRuling
            , rulingSquare Square selectedRuling
            , rulingSquare Plain selectedRuling
            ]
        ]


type Ruling
    = Dot
    | Ruled
    | Square
    | Plain


nextRuling ruling =
    case ruling of
        Dot ->
            Ruled

        Ruled ->
            Square

        Square ->
            Plain

        Plain ->
            Dot


rulingToText ruling =
    case ruling of
        Dot ->
            "Dot"

        Ruled ->
            "Ruled"

        Square ->
            "Square"

        Plain ->
            "Plain"


rulingSquare ruling selectedRuling =
    let
        selected =
            ruling == selectedRuling

        attr size cornerRadius bWidth =
            [ Border.color (gray 1)
            , Border.width bWidth
            , Border.rounded cornerRadius
            , width (px size)
            , height (px size)
            , onClick (SelectRuling ruling)
            ]
    in
    el
        (attr 34 4 1
            ++ (if selected then
                    [ behindContent
                        (el
                            (attr 44 8 2 ++ [ moveUp 6, moveLeft 6 ])
                            none
                        )
                    , Background.color white
                    ]

                else
                    [ Background.color (gray 0.04)
                    ]
               )
        )
    <|
        case ruling of
            Dot ->
                let
                    dot =
                        el [ width (px 1), height (px 1), Background.color (gray 1) ] none
                in
                column [ spaceEvenly, width fill, height fill, padding 7 ]
                    [ row [ spaceEvenly, width fill ] [ dot, dot, dot ]
                    , row [ spaceEvenly, width fill ] [ dot, dot, dot ]
                    , row [ spaceEvenly, width fill ] [ dot, dot, dot ]
                    ]

            Ruled ->
                let
                    line =
                        el [ width fill, height (px 1), Background.color (gray 1) ] none
                in
                column [ spaceEvenly, width fill, height fill, paddingXY 0 10 ]
                    [ line
                    , line
                    ]

            Square ->
                let
                    hLine =
                        el [ width fill, height (px 1), Background.color (gray 1) ] none

                    vLine =
                        el [ height fill, width (px 1), Background.color (gray 1) ] none

                    vLines =
                        row [ spaceEvenly, width fill, height fill, paddingXY 10 0 ]
                            [ vLine
                            , vLine
                            ]

                    hLines =
                        column [ spaceEvenly, width fill, height fill, paddingXY 0 10 ]
                            [ hLine
                            , hLine
                            ]
                in
                el
                    [ width fill
                    , height fill
                    , inFront hLines
                    ]
                    vLines

            Plain ->
                none


pricing =
    let
        spacer =
            el [ width (px 1), height (px buttonSize), Background.color (gray 0.2) ] none

        buttonSize =
            50
    in
    column
        [ spacing 24
        , paddingEach
            { top = 24
            , right = 0
            , bottom = 0
            , left = 0
            }
        ]
        [ row [ width fill ]
            [ el [ alignTop, Font.size 15, Font.letterSpacing 0.8 ] (text "$")
            , el [ alignTop, Font.size 40, Font.letterSpacing 0.8, moveUp 5 ] (text "24")
            , el [ alignTop, Font.size 15, Font.letterSpacing 0.8 ] (text ".95")
            , paragraph [ alignRight, Font.size 15, width shrink ]
                [ text "Get "
                , el [ Font.color orange, Font.bold ] (text "25% OFF ")
                , text "if you buy 5 or more."
                ]
            ]
        , row
            [ spacing 16
            , paddingEach
                { top = 0
                , right = 10
                , bottom = 0
                , left = 0
                }
            ]
            [ row
                [ inFront
                    (el
                        [ Border.color (gray 0.2)
                        , Border.width 1
                        , Border.rounded 4
                        , height fill
                        , width fill
                        ]
                        none
                    )
                , Border.rounded 4
                , Background.color (gray 0.05)
                ]
                [ el [ width (px buttonSize), height (px buttonSize), Font.size 32, Font.light, Font.color (gray 0.3) ]
                    (el [ centerX, centerY ] (text "-"))
                , spacer
                , el [ width (px buttonSize), height (px buttonSize), Font.size 20, Background.color white ] (el [ centerX, centerY ] (text "1"))
                , spacer
                , el [ width (px buttonSize), height (px buttonSize), Font.size 32, Font.light, Font.color (gray 0.3) ]
                    (el [ centerX, centerY ] (text "+"))
                ]
            , el [ height (px buttonSize), Background.color orange, Font.color white, paddingXY (buttonSize // 2) 0, Border.rounded 4 ]
                (el [ centerX, centerY ] (text "Add to cart"))
            ]
        ]


bookImage { percent, currentIndex } ruling =
    let
        now =
            0

        space =
            60
    in
    column
        [ width fill
        , alignTop
        , height fill
        , spacing space
        , behindContent (rulingBg ruling)
        ]
        [ el
            [ centerX
            , moveDown 10
            , dropShadow
                { offset = ( 35, 10 + 30 )
                , blur = 40
                , color = orangeWithA 0.2
                }
            ]
            (image
                [ width (px 290)
                , moveLeft 30
                , rotate 0.2
                , dropShadow
                    { offset = ( 25, 40 )
                    , blur = 30
                    , color = rgba 0 0 0 0.3
                    }
                ]
                { src = "Book.png"
                , description = "An image of a moleskin notebook"
                }
            )
        , row
            [ spacing 10
            , centerX
            , moveLeft 40
            ]
            (images
                |> List.indexedMap (imagePreview percent currentIndex)
            )
        ]


rulingBg ruling =
    let
        size =
            1000

        attr =
            [ width (px size)
            , height (px size)
            ]

        pattern =
            case ruling of
                Dot ->
                    Svg.rect
                        [ SvgA.width "2"
                        , SvgA.height "2"
                        , SvgA.fill "black"
                        , SvgA.x "0"
                        , SvgA.y "0"
                        , SvgA.rx "1"
                        ]
                        []

                Ruled ->
                    Svg.rect
                        [ SvgA.width "100%"
                        , SvgA.height "1"
                        , SvgA.fill "black"
                        , SvgA.x "0"
                        , SvgA.y "9"
                        ]
                        []

                Square ->
                    Svg.svg
                        []
                        [ Svg.rect
                            [ SvgA.width "100%"
                            , SvgA.height "1"
                            , SvgA.fill "black"
                            , SvgA.x "0"
                            , SvgA.y "0"
                            ]
                            []
                        , Svg.rect
                            [ SvgA.width "1"
                            , SvgA.height "100%"
                            , SvgA.fill "black"
                            , SvgA.x "0"
                            , SvgA.y "0"
                            ]
                            []
                        ]

                Plain ->
                    Svg.rect [] []
    in
    el
        [ clip
        , width fill
        , height fill
        , style "max-height" "100%"
            |> htmlAttribute
        ]
    <|
        (html <|
            Svg.svg
                [ SvgA.viewBox "0 0 300 300"
                , SvgA.width "100%"
                , SvgA.height "100%"
                ]
                [ Svg.defs []
                    [ Svg.pattern
                        [ SvgA.id "ruling"
                        , SvgA.width "20"
                        , SvgA.height "20"
                        , SvgA.patternUnits "userSpaceOnUse"
                        ]
                        [ pattern
                        ]
                    , Svg.linearGradient
                        [ SvgA.id "fadeGradient"
                        , SvgA.x2 "0"
                        , SvgA.y2 "1"
                        ]
                        [ Svg.stop [ SvgA.offset "0", SvgA.stopColor "white", SvgA.stopOpacity "0.5" ] []
                        , Svg.stop [ SvgA.offset "0.7", SvgA.stopColor "white", SvgA.stopOpacity "0" ] []
                        ]
                    , Svg.mask
                        [ SvgA.id "fadeMask"
                        , SvgA.maskContentUnits "objectBoundingBox"
                        ]
                        [ Svg.rect [ SvgA.width "1", SvgA.height "1", SvgA.fill "url(#fadeGradient)" ] []
                        ]
                    ]
                , Svg.rect
                    [ SvgA.width "300"
                    , SvgA.height "300"
                    , SvgA.fill "url(#ruling)"
                    , SvgA.mask "url(#fadeMask)"
                    ]
                    []
                ]
        )


imagePreview percent currentImageNo imageLink imageNo =
    let
        w =
            40

        sliderH =
            2
    in
    column [ spacing 10 ]
        [ el [ width (px 56), centerX ]
            (image [ width fill, centerX ]
                { description = "Image " ++ String.fromInt imageNo ++ " of " ++ (List.length >> String.fromInt) images
                , src = String.fromInt imageNo ++ ".png"
                }
            )
        , if imageNo == currentImageNo then
            el [ width (px w), height (px sliderH), Background.color (orangeWithA 0.4), centerX ]
                (el [ width (px (round (w * percent))), height (px sliderH), Background.color orange ] none)

          else
            el [ width (px 40), height (px 2), Background.color (gray 0.1), centerX ] none
        ]


navbar =
    row
        [ width fill
        , paddingEach
            { top = 40
            , right = 0
            , bottom = 40
            , left = 0
            }
        , Font.medium
        , Font.size 15
        , spacing 60
        , Font.letterSpacing 0.4
        ]
        [ row
            [ Font.family [ Font.typeface "Libre Baskerville" ]
            , Font.letterSpacing 0.8
            , Font.bold
            , Font.size 18
            ]
            [ text "MOLESKINE"
            , el [ Font.size 12, moveUp 4 ] (text "®")
            ]
        , row
            [ spacing 60
            , Font.family
                [ Font.typeface "Circular Std Book"
                ]
            ]
            [ el
                [ below
                    (el
                        [ width (px 30)
                        , height (px 2)
                        , Background.color orange
                        , centerX
                        , moveDown 6
                        ]
                        none
                    )
                ]
                (text "Notebooks")
            , text "Pens & Pencils"
            , text "Bags"
        --    , text "Accessories"
            ]
        , row
            [ alignRight
            , spacing 30
            ]
            [ icon FeatherIcons.user 24 []
            , icon FeatherIcons.shoppingBag
                24
                [ inFront
                    (el
                        [ Background.color orange
                        , width (px 14)
                        , height (px 14)
                        , alignRight
                        , moveUp 3
                        , moveRight 3
                        , Border.rounded 8
                        ]
                        (el
                            [ centerX
                            , centerY
                            , Font.color white
                            , Font.size 9
                            ]
                            (text "1")
                        )
                    )
                ]
            ]
        ]



--ICONS


icon icon_ size_ attr =
    icon_
        |> FeatherIcons.withSize size_
        |> FeatherIcons.toHtml []
        |> html
        |> el attr



-- COLORS


white =
    rgb 1 1 1


orangeWithA a =
    rgba255 252 102 59 a


orange =
    orangeWithA 1


toStr color =
    let
        c =
            toRgb color
    in
    "rgba("
        ++ String.fromInt (round (c.red * 255))
        ++ ","
        ++ String.fromInt (round (c.green * 255))
        ++ ","
        ++ String.fromInt (round (c.blue * 255))
        ++ ","
        ++ String.fromFloat c.alpha
        ++ ")"


gray intensity =
    rgb (1 - intensity) (1 - intensity) (1 - intensity)


darkness =
    rgb255 134 125 122


dropShadow o =
    style "filter"
        ("drop-shadow("
            ++ String.fromInt (Tuple.first o.offset)
            ++ "px "
            ++ String.fromInt (Tuple.second o.offset)
            ++ "px "
            ++ String.fromInt o.blur
            ++ "px "
            ++ toStr o.color
            ++ ")"
        )
        |> htmlAttribute
