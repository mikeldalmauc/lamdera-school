module Style exposing (..)

import Element as El exposing (Attribute, rgb, rgba255)
import Element.Font as Font
import Svg
import Svg.Attributes as SvgAttrs
import Html
import Element exposing (Element)
import Element exposing (rgba)


gray90 = rgb  0.1 0.1 0.1
gray80 = rgb  0.2 0.2 0.2
gray5 = rgb  0.95 0.95 0.95
buttonSelectedColor = rgb 0.3 0.8 0.3
blueGlowColor = darkBlueColor
blue = rgba255 59 153 252 1
darkBlueColor = rgb 0.2 0.2 1.0
greenColor = rgb 0.2 1.0 0.2
transparent = rgba255 0 0 0 0


debugGreen = rgb 0.333 0.949 0
debugBlue = rgb 0 0.847 0.847
debugOrange = rgb 1 0.455 0
debugRed = rgb 0.973 0 0.208
debugPurple = rgb 0.898 0 0.788


brandFontAttrs : List (Attribute msg)
brandFontAttrs =
    [brandFont, Font.color gray5, Font.letterSpacing 3, Font.wordSpacing 1.4]

baseFontAttrs : List (Attribute msg)
baseFontAttrs =
    [baseFont400, Font.color gray5, Font.hairline, Font.letterSpacing 1.2, Font.wordSpacing 1.2]

secondaryFontAttrs : List (Attribute msg)
secondaryFontAttrs =
    [baseFont400, Font.letterSpacing 1.2, Font.wordSpacing 1.2]



brandFont : Attribute msg
brandFont = Font.family
            [ Font.external
                { name = "EB Garamond"
                , url = "https://fonts.googleapis.com/css2?family=EB+Garamond:wght@800&display=swap"
                }
            , Font.serif
            ]


baseFont100 : Attribute msg
baseFont100 = Font.family
            [ Font.external
                { name = "Montserrat"
                , url = "https://fonts.googleapis.com/css2?family=Montserrat:wght@100&display=swap"
                }
            , Font.sansSerif
            ]


baseFont400 : Attribute msg
baseFont400 = Font.family
            [ Font.external
                { name = "Montserrat"
                , url = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400&display=swap"
                }
            , Font.sansSerif
            ]


doubleArrow : Int -> Element msg
doubleArrow size = 
    El.html <|
        Svg.svg
        [ SvgAttrs.height <| String.fromInt size
        , SvgAttrs.width <| String.fromInt size
        , SvgAttrs.viewBox "0 0 24 24"
        ][
            Svg.path [SvgAttrs.d "M12,2A10,10,0,1,0,22,12,10.011,10.011,0,0,0,12,2Zm0,18a8,8,0,1,1,8-8A8.009,8.009,0,0,1,12,20Z"
            , SvgAttrs.fill "white"]
            []
        ,   Svg.polygon [SvgAttrs.points "15.293 7.293 10.586 12 15.293 16.707 16.707 15.293 13.414 12 16.707 8.707 15.293 7.293"
            , SvgAttrs.fill "white"] [] 
        ,   Svg.polygon [SvgAttrs.points "12.707 8.707 11.293 7.293 6.586 12 11.293 16.707 12.707 15.293 9.414 12 12.707 8.707" 
            , SvgAttrs.fill "white"] [] 
        ]


arrow : Int -> Element msg
arrow size = 
    El.html <|
        Svg.svg
        [ SvgAttrs.height <| String.fromInt size
        , SvgAttrs.width <| String.fromInt size
        , SvgAttrs.viewBox "0 0 24 24"
        ][
            Svg.path [SvgAttrs.d "M12,2A10,10,0,1,0,22,12,10.011,10.011,0,0,0,12,2Zm0,18a8,8,0,1,1,8-8A8.009,8.009,0,0,1,12,20Z"
            , SvgAttrs.fill "white"]
            []
        ,   Svg.polygon [SvgAttrs.points "13.293 7.293 8.586 12 13.293 16.707 14.707 15.293 11.414 12 14.707 8.707 13.293 7.293"
            , SvgAttrs.fill "white"] [] 
        ]
