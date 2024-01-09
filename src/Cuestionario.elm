module Cuestionario exposing (..)


import Browser.Dom exposing (Element)

import Element as El exposing (Element, el, text, row, width, height, spacing, centerY, padding, column, row)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element exposing (centerX)
import Dict exposing (Dict)
import Debug exposing (toString)
import Element exposing (paddingXY)
import Element.Input as Input

import Style exposing (..)

import Html.Attributes as HAttrs
import Html exposing (col)
import Element.Font exposing (justify)
import Html exposing (pre)

type alias Preguntas = Dict Int Pregunta

type alias Model = {
        idPreguntaActual: Int,
        preguntaActual: Pregunta,
        preguntas: Preguntas
    }

type alias PreguntaRespuestaUnica = { 
        pregunta: String, 
        opcionesResp: List String, 
        respuesta: Maybe Int
    }

type alias PreguntaMultiRespuesta = {
        pregunta: String, 
        opcionesResp: List String, 
        respuestas: Maybe (List Int)
    }

type Pregunta = PreguntaMR PreguntaMultiRespuesta
    | PreguntaRU PreguntaRespuestaUnica

type Msg = NoOp
    | EligeRespuesta Int
    | SaltarAPregunta Int
    | SiguientePregunta
    | AnteriorPregunta
    | PrimeraPregunta
    | UltimaPregunta


init : ( Model, Cmd Msg )
init =
    ( { 
        idPreguntaActual = 2
    ,   preguntaActual = PreguntaRU { pregunta = "Pregunta " ++ toString 1, opcionesResp = ["Respuesta 1", "Respuesta 2", "Respuesta 3"], respuesta = Nothing }
    ,   preguntas = Dict.fromList 
            <| List.indexedMap (\index pregunta -> (index + 1, pregunta))
                [
                    PreguntaMR { pregunta = "Pregunta Multi " ++ toString 1, opcionesResp = ["Respuesta 1", "Respuesta 2", "Respuesta 3"], respuestas = Nothing }
                ]
                ++ (List.range 2 120 |> List.map (\index -> (index, PreguntaRU { pregunta = "Pregunta " ++ toString index, opcionesResp = ["Respuesta 1", "Respuesta 2", "Respuesta 3"], respuesta = Nothing })))
    }, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EligeRespuesta idRespuesta ->
            let
                preguntaActualUpdated = marcarRespuesta idRespuesta model.preguntaActual 
                preguntasDict = Dict.insert model.idPreguntaActual preguntaActualUpdated model.preguntas
            in
            ( { model | preguntas = preguntasDict, preguntaActual = preguntaActualUpdated }, Cmd.none )


        SaltarAPregunta id -> 
            let
                preguntaAct = Dict.get id model.preguntas
            in
                case preguntaAct of
                    Just pregunta -> 
                        ( { model | preguntaActual = pregunta, idPreguntaActual = id }, Cmd.none )
                    Nothing -> 
                        ( model, Cmd.none )
        
        SiguientePregunta ->
            let
                preguntaAct = Dict.get (model.idPreguntaActual  + 1) model.preguntas
            in
                case preguntaAct of
                    Just pregunta ->
                        ( { model | preguntaActual = pregunta, idPreguntaActual = model.idPreguntaActual + 1 }, Cmd.none )
                    Nothing ->
                        ( model, Cmd.none )    
                        
        AnteriorPregunta ->
             let
                preguntaAct = Dict.get (model.idPreguntaActual  - 1) model.preguntas
            in
                case preguntaAct of
                    Just pregunta ->
                        ( { model | preguntaActual = pregunta, idPreguntaActual = model.idPreguntaActual - 1 }, Cmd.none )
                    Nothing ->
                        ( model, Cmd.none )    

        PrimeraPregunta ->  
            let
                preguntaAct = Dict.get 1 model.preguntas
            in
                case preguntaAct of
                    Just pregunta ->
                        ( { model | preguntaActual = pregunta, idPreguntaActual = 1 }, Cmd.none )
                    Nothing ->
                        ( model, Cmd.none )

        UltimaPregunta ->  
            let
                preguntaAct = Dict.get (Dict.size model.preguntas) model.preguntas
            in
                case preguntaAct of
                    Just pregunta ->
                        ( { model | preguntaActual = pregunta, idPreguntaActual = Dict.size model.preguntas }, Cmd.none )
                    Nothing ->
                        ( model, Cmd.none )


marcarRespuesta :Int ->  Pregunta ->  Pregunta
marcarRespuesta idRespuesta pregunta = 
    case pregunta of 
        PreguntaRU p -> 
            case p.respuesta of
                Just _ -> PreguntaRU { p  | respuesta = Nothing } 
                Nothing -> PreguntaRU { p  | respuesta = Just idRespuesta }

        PreguntaMR p -> 
            case p.respuestas of
                Just idrs -> 
                    if List.member idRespuesta idrs then 
                        let
                            idrsSin = List.filter (\id -> id /= idRespuesta) idrs
                        in
                            if List.length idrsSin == 0 then
                                PreguntaMR { p  | respuestas = Nothing } 
                            else
                                PreguntaMR { p  | respuestas = Just idrsSin } 
                    else 
                        PreguntaMR { p  | respuestas = Just (idRespuesta :: idrs) } 
                    
                Nothing -> PreguntaMR {p | respuestas = Just [idRespuesta]}
    
    --  let
    --     preguntaAct = 
    --         if (Just idRespuesta) == model.preguntaActual.respuesta then
    --             let preg = model.preguntaActual in { preg  | respuesta = Nothing } 
    --         else
    --             let preg = model.preguntaActual in { preg  | respuesta = Just idRespuesta } 


tieneRespuesta : Int -> Preguntas -> Bool
tieneRespuesta id preguntas =
    case Dict.get id preguntas of
        Just pregunta ->
            case pregunta of 
                PreguntaRU p -> 
                    case p.respuesta of
                        Just _ ->
                            True
                        Nothing ->
                            False

                PreguntaMR p -> 
                    case p.respuestas of
                        Just _ ->
                            True
                        Nothing ->
                            False
        Nothing ->
            False
            

view : Model -> Element Msg
view model =
    let
        preguntaView = viewPregunta model.idPreguntaActual model.preguntaActual
        sliderView =  viewSlider model
        
    in
        column [centerY, centerX, spacing 30] 
            <| [ preguntaView, sliderView]


viewPregunta : Int -> Pregunta -> Element Msg
viewPregunta idPregunta pregunta =
    case pregunta of 
        PreguntaRU p -> 
            viewPreguntaRespuestaUnica idPregunta p
            
        PreguntaMR p -> 
            viewPreguntaMultiRespuesta idPregunta p
            

viewPreguntaRespuestaUnica : Int -> PreguntaRespuestaUnica -> Element Msg
viewPreguntaRespuestaUnica idPregunta pregunta =
    el [centerY, centerX] 
        <| column [spacing 10] 
            <| [( el [padding 15, Font.alignLeft, Font.bold] <| text (toString idPregunta ++ ". " ++ pregunta.pregunta)) 
                , viewRespuestasRU pregunta.respuesta pregunta.opcionesResp
                ]

viewRespuestasRU : Maybe Int -> List String -> Element Msg
viewRespuestasRU selected respuestas =
    Input.radio
        [ paddingXY 40 5
        , spacing 20
        ]
        { onChange = EligeRespuesta
        , selected = selected
        , label = Input.labelHidden "Respuesta"
        , options =
            List.indexedMap (
                \index respuesta ->
                    Input.option index (text respuesta)
            ) respuestas
        }
        
viewPreguntaMultiRespuesta :  Int -> PreguntaMultiRespuesta -> Element Msg
viewPreguntaMultiRespuesta idPregunta pregunta  =
    el [centerY, centerX] 
        <| column [spacing 10] 
            <| [( el [padding 15, Font.alignLeft, Font.bold] <| text (toString idPregunta ++ ". " ++ pregunta.pregunta)) 
                , viewRespuestasMulti pregunta.respuestas pregunta.opcionesResp
                ]

viewRespuestasMulti : Maybe (List Int) -> List String -> Element Msg
viewRespuestasMulti selected opcionesResp =
    let
        selectedResps = case selected of
                Just ids -> ids
                Nothing -> [] 
    in
        column [ spacing 11]
            <|  List.indexedMap (\index respuesta -> 
                    Input.checkbox [paddingXY 40 5]
                        { onChange = \b -> EligeRespuesta index
                        , icon = Input.defaultCheckbox
                        , checked = List.member index selectedResps
                        , label = Input.labelRight [paddingXY 5 0] (text respuesta)
                        }
                    ) opcionesResp


viewSlider : Model -> Element Msg
viewSlider model =
    let
        arrowStyle = [ 
              padding 0
            , El.pointer ]
        buttonStyle = [ 
                padding 8
            , width <| El.px 28
            , El.pointer
            , El.mouseOver [Font.color darkBlueColor] ]

        anterior = Input.button
            arrowStyle
            { onPress = Just AnteriorPregunta
            , label = Style.arrow 27
            }

        siguiente = Input.button
            arrowStyle
            { onPress = Just SiguientePregunta
            , label = el [El.htmlAttribute <| HAttrs.attribute "style" "transform: rotate(180deg);"] <| Style.arrow 24
            }

        primera = Input.button
            arrowStyle
            { onPress = Just PrimeraPregunta
            , label = Style.doubleArrow 27
            }

        ultima = Input.button
            arrowStyle
            { onPress = Just UltimaPregunta
            , label = el [El.htmlAttribute <| HAttrs.attribute "style" "transform: rotate(180deg);"] <| Style.doubleArrow 24
            }

        dictSize = Dict.size model.preguntas

        (firstIndex, lastIndex) = surroundingIndices dictSize model.idPreguntaActual
        
        estiloMarcador = \index -> 
                (if index == model.idPreguntaActual then [Background.color blueColor] else [Background.color gray80])
            -- ++  (if tieneRespuesta index model.preguntas then [Font.color greenColor] else [])
            ++  [Border.width 0, Border.color gray5, Border.rounded 3]
            ++  [Font.size 10, Font.bold, Font.center]
            ++  [width <| El.px 23]
            ++  buttonStyle

        label = \index -> 
            column [Font.center, centerY, centerX, spacing 3] [el [centerX] <| text <| toString index
                , el [centerX, Border.rounded 2, width <| El.px 12, height <| El.px 3
                    , Background.color (if tieneRespuesta index model.preguntas then greenColor else gray90)] El.none]

        indices = 
            List.map (\index -> 
                Input.button
                    (estiloMarcador index) 
                    { onPress = Just <| SaltarAPregunta index
                    , label = label index
                    })    
            <| List.range firstIndex lastIndex

    in
        row [centerY, centerX, spacing 4] 
            <| List.concat [ [primera, anterior], [text " "], indices, [text " "], [siguiente, ultima] ]


surroundingIndices : Int -> Int -> (Int, Int)
surroundingIndices dictSize currentIndex =
    if dictSize < 5 then
        (1, dictSize)
    else  
        if dictSize - currentIndex < 2 then
            (dictSize - 4, dictSize)
        else
            if currentIndex < 3 then
                (1, 5)
            else 
               (currentIndex - 2, currentIndex + 2)
