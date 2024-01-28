module Cuestionario exposing (..)


import Browser.Dom exposing (Element)

import Element exposing (Element, el, text, row, width, height, spacing, centerY, padding, column, row, none, paddingXY, centerX, moveRight, moveLeft)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element exposing (centerX)
import Dict exposing (Dict)
import Debug exposing (toString)
import Element.Input as Input

import Style exposing (..)

import Html.Attributes as HAttrs
import DnDList.Groups

type alias Preguntas = Dict Int Pregunta

type alias Model = {
        idPreguntaActual: Int,
        preguntaActual: Pregunta,
        preguntas: Preguntas,
        dnd: DnDList.Groups.Model
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

type alias PreguntaMultiRespuestaLibre = {
        titulo: String,
        parrafo: List PreguntaMRL
    }

type alias PreguntaDragAndDrop = {
        titulo: String,
        preguntas: List String,
        respuestas: List Item
    }

type Group
    = NoAsignada
    | Asignada

type Category = Empty | Answer

type alias Item =
    { category : Category
    , group : Group
    , value : String
    }


type PreguntaMRL = 
      Texto String
    | HuecoRespuesta (Maybe String)

type Pregunta = PreguntaMR PreguntaMultiRespuesta
    | PreguntaRU PreguntaRespuestaUnica
    | PreguntaMRL PreguntaMultiRespuestaLibre
    | PreguntaDND PreguntaDragAndDrop


type Msg = NoOp
    | EligeRespuesta Int
    | RellenaRespuesta Int String

    | SaltarAPregunta Int
    | SiguientePregunta
    | AnteriorPregunta
    | PrimeraPregunta
    | UltimaPregunta
    | DragAndDropMsg DnDList.Groups.Msg


init : ( Model, Cmd Msg )
init =
    ( { 
        idPreguntaActual = 4
    ,   preguntaActual = PreguntaRU { pregunta = "Pregunta " ++ toString 1, opcionesResp = ["Respuesta 1", "Respuesta 2", "Respuesta 3"], respuesta = Nothing }
    ,   preguntas = Dict.fromList 
            <| List.indexedMap (\index pregunta -> (index + 1, pregunta))
                [
                    PreguntaMR { pregunta = "Pregunta Multi " ++ toString 1, opcionesResp = ["Respuesta 1", "Respuesta 2", "Respuesta 3"], respuestas = Nothing }
                ,   PreguntaMRL { titulo = "Completa el texto" , parrafo = [Texto "Pregunta MRsL 1 ", HuecoRespuesta Nothing, Texto ". Pregunta MRL 2 ", HuecoRespuesta Nothing, Texto ". Pregunta MRL 3 ", HuecoRespuesta Nothing]}
                ,   PreguntaDND { titulo = "Arrastra las opciones a su lugar correcto", preguntas = ["El pez es ", "El pajaro ", "El oso "], 
                        respuestas = [
                          Item Empty Asignada "vacio" 
                        , Item Empty Asignada "vacio"
                        , Item Empty Asignada "vacio"
                        , Item Answer NoAsignada "de color rojo."
                        , Item Answer NoAsignada "vuela."
                        , Item Answer NoAsignada "come miel."
                        , Item Answer Asignada "de color sssss."
                        ]}
                ]
                ++ (List.range 4 120 |> List.map (\index -> (index, PreguntaRU { pregunta = "Pregunta " ++ toString index, opcionesResp = ["Respuesta 1", "Respuesta 2", "Respuesta 3"], respuesta = Nothing })))
    ,   dnd = system.model
    }, Cmd.none )


-- SYSTEM DND

config : DnDList.Groups.Config Item
config =
    { beforeUpdate = \_ _ list -> list
    , listen = DnDList.Groups.OnDrag
    , operation = DnDList.Groups.Swap
    , groups =
        { listen = DnDList.Groups.OnDrop
        , operation = DnDList.Groups.Swap
        , comparator = comparator
        , setter = setter
        }
    }


system : DnDList.Groups.System Item Msg
system =
    DnDList.Groups.create config DragAndDropMsg

comparator : Item -> Item -> Bool
comparator item1 item2 =
    item1.group == item2.group


setter : Item -> Item -> Item
setter item1 item2 = 
    { item2 | group = item1.group }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    system.subscriptions model.dnd


--- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        RellenaRespuesta id respuesta -> 
            let
                preguntaActualUpdated = case model.preguntaActual of 
                    PreguntaMRL p -> 
                        let
                            pUpdated = List.indexedMap (\index section -> 
                                case section of 
                                    HuecoRespuesta _ -> 
                                        if index == id then 
                                            if String.isEmpty respuesta then 
                                                HuecoRespuesta Nothing
                                            else
                                                HuecoRespuesta <| Just respuesta
                                        else 
                                            section
                                    _ -> 
                                        section
                                ) p.parrafo
                        in
                            PreguntaMRL {p | parrafo = pUpdated}
                    _ -> 
                        model.preguntaActual
                preguntasDict = Dict.insert model.idPreguntaActual preguntaActualUpdated model.preguntas
            in
            ( { model | preguntas = preguntasDict, preguntaActual = preguntaActualUpdated }, Cmd.none )
            

        EligeRespuesta idRespuesta ->
            let
                preguntaActualUpdated = marcarRespuesta idRespuesta model.preguntaActual 
                preguntasDict = Dict.insert model.idPreguntaActual preguntaActualUpdated model.preguntas
            in
            ( { model | preguntas = preguntasDict, preguntaActual = preguntaActualUpdated }, Cmd.none )

        DragAndDropMsg dndMsg ->
            case model.preguntaActual of 
                PreguntaDND preguntaDnD -> 
                    let
                        ( dnd, respuestasActualizadas ) =
                            system.update dndMsg model.dnd preguntaDnD.respuestas
                        preguntaActualUpdated = PreguntaDND { preguntaDnD | respuestas = respuestasActualizadas } 

                        preguntasDict = Dict.insert model.idPreguntaActual preguntaActualUpdated model.preguntas
                    in
                    ( { model | dnd = dnd, preguntas = preguntasDict, preguntaActual = preguntaActualUpdated } , system.commands dnd )

                _ -> 
                    ( model, Cmd.none )

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


marcarRespuesta : Int ->  Pregunta ->  Pregunta
marcarRespuesta idRespuesta pregunta = 
    case pregunta of 
        PreguntaRU p -> 
            case p.respuesta of
                Just resp -> 
                    if resp == idRespuesta then 
                        PreguntaRU { p  | respuesta = Nothing } 
                    else 
                        PreguntaRU { p  | respuesta = Just idRespuesta }
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

        _ -> 
            pregunta


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
                
                PreguntaMRL p ->
                    List.any (\section -> 
                        case section of 
                            HuecoRespuesta res -> 
                                case res of
                                    Just _ -> 
                                        True
                                    Nothing -> 
                                        False
                            _ -> 
                                False
                        ) p.parrafo

                PreguntaDND p ->
                    List.any (\item -> 
                        case (item.group, item.category) of 
                            (Asignada, Answer) -> 
                                True
                            _ -> 
                                False
                        ) 
                    <| List.take (List.length p.preguntas) p.respuestas
        Nothing ->
            False
            

-- VIEW


view : Model -> Element Msg
view model =
    let
        preguntaView = viewPregunta model.idPreguntaActual model.preguntaActual model.dnd
        sliderView =  viewSlider model
        
    in
        column [centerY, centerX, spacing 30] 
            <| [ preguntaView, sliderView]


viewPregunta : Int -> Pregunta -> DnDList.Groups.Model -> Element Msg
viewPregunta idPregunta pregunta dnd =
    case pregunta of 
        PreguntaRU p -> 
            viewPreguntaRespuestaUnica idPregunta p
            
        PreguntaMR p -> 
            viewPreguntaMultiRespuesta idPregunta p
            
        PreguntaMRL p -> 
           viewPreguntaMultiRespuestaLibre idPregunta p

        PreguntaDND p -> 
            viewPreguntaDragAndDrop idPregunta p dnd


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

viewPreguntaMultiRespuestaLibre : Int -> PreguntaMultiRespuestaLibre -> Element Msg
viewPreguntaMultiRespuestaLibre idPregunta pregunta =
    el [centerY, centerX] 
        <| column [spacing 10] 
            <| [( el [padding 15, Font.alignLeft, Font.bold] <| text (toString idPregunta ++ ". " ++ pregunta.titulo)) 
                , viewRespuestasMRL pregunta
                ]
                
viewRespuestasMRL : PreguntaMultiRespuestaLibre -> Element Msg
viewRespuestasMRL pregunta = 
    Element.paragraph [] <| 
        List.indexedMap (\ index section -> 
            case section of 
                Texto t -> 
                    text t
                HuecoRespuesta respuesta -> 
                    Input.text [paddingXY 5 5, Font.color gray90]   
                        { onChange = \s -> RellenaRespuesta index s
                        , text = case respuesta of 
                            Just res -> res
                            Nothing -> ""
                        , placeholder = Nothing
                        , label = Input.labelHidden <| "Respuesta" ++ (toString index)
                        }
            ) pregunta.parrafo


viewPreguntaDragAndDrop : Int -> PreguntaDragAndDrop -> DnDList.Groups.Model ->  Element Msg
viewPreguntaDragAndDrop idPregunta pregunta dnd =
    let
        -- Dividir la lista en dos partes
        -- Especificar el índice donde deseas dividir la lista
        indiceDeDivision = List.length pregunta.preguntas

        listWithGlobalIndex = List.indexedMap Tuple.pair pregunta.respuestas

        (answers, unanswered) =
                Tuple.pair (List.take indiceDeDivision listWithGlobalIndex) (List.drop indiceDeDivision listWithGlobalIndex) 

        answersArea = 
            List.map2 (\p (globalIndex, answ) ->  row [paddingXY 5 5] [
                text p
            ,  itemView dnd pregunta.respuestas globalIndex answ
            ]) pregunta.preguntas answers

        unansweredArea = 
            List.map (\(globalIndex, answ) ->  row [paddingXY 5 5] [itemView dnd pregunta.respuestas globalIndex answ]
            ) unanswered

            

    in
        el [centerY, centerX] 
            <| column [spacing 10] 
                <| [( el [padding 15, Font.alignLeft, Font.bold] <| text (toString idPregunta ++ ". " ++ pregunta.titulo)) ]
                    ++ 
                    [Element.column 
                            []
                            (answersArea ++ unansweredArea ++
                            [
                            ghostView dnd pregunta.respuestas
                            ])
                            ]

itemStyle : String -> List (Element.Attribute Msg)
itemStyle itemId =
    [Element.htmlAttribute (HAttrs.id itemId)
    , Font.color gray5
    , Background.color debugPurple
    ]

itemView : DnDList.Groups.Model -> List Item -> Int -> Item -> Element Msg
itemView dnd items globalIndex {category, group, value } =
    let
        itemId : String
        itemId =
            "id-" ++ String.fromInt globalIndex

        style = case category of
            Empty ->
                itemStyle itemId
                ++ List.map Element.htmlAttribute (system.dropEvents globalIndex itemId)
            _ ->
                List.map Element.htmlAttribute (system.dragEvents globalIndex itemId)
                ++ List.map Element.htmlAttribute (system.dropEvents globalIndex itemId)
                ++ itemStyle itemId
        
    in
    case ( system.info dnd, maybeDragItem dnd items ) of
        -- Casos que se dan cuando hay un arrastramiento en curso
        ( Just { dragIndex }, Just dragItem ) ->

            -- Este caso se da cuando un elemento es vacio y 
            -- if group == NoAsignada && dragItem.group /= group then
            --     el
            --         (style
            --         --  ++ List.map Element.htmlAttribute (system.dropEvents globalIndex itemId)
            --         )
            --         (text value)

            -- -- Este caso se da cuando un elemento se coloca encima de otro
            -- else if group == NoAsignada && dragItem.group == group then
            --      el
            --         (style
            --         )
            --         (text value)

            -- -- Este caso representa a los elementos que no están siendo arrastrados ( mientras otro si lo está)
            -- else 
            
            if dragIndex /= globalIndex then
                el
                (style
                    -- ++ List.map Element.htmlAttribute (system.dropEvents globalIndex itemId)
                )
                (text "debug 3")

            -- Este caso representa al hueco al que el elemento que está siendo arrastrado va a posicionarse
            else
                el
                    (style
                    )
                    (text "debug 4")

        _ ->
            if group == NoAsignada then
                el
                    (style
                    )
                    (text value)

            else
                el
                    (
                        Element.pointer
                        :: style
                        -- ++ List.map Element.htmlAttribute (system.dragEvents globalIndex itemId)
                    )
                    (text value)


ghostView : DnDList.Groups.Model -> List Item -> Element Msg
ghostView dnd items =
    case maybeDragItem dnd items of
        -- Este caso es un placeholder para el elemnto que va aser arrastrado
        Just { group, value} ->
            el
                ( itemStyle "ghost" ++ List.map Element.htmlAttribute (system.ghostStyles dnd)
                )
                (text value)
        -- Este caso es un placeholder para el elemnto que va aser arrastrado, no debería verse si no hay un elemento arrastrandose
        Nothing ->
             el
                (List.map Element.htmlAttribute (system.ghostStyles dnd)
                    ++ itemStyle "ghost"
                )
                (text "shadow debug")



maybeDragItem : DnDList.Groups.Model -> List Item -> Maybe Item
maybeDragItem dnd items =
    system.info dnd
        |> Maybe.andThen (\{ dragIndex } -> items |> List.drop dragIndex |> List.head)





-- Slider

viewSlider : Model -> Element Msg
viewSlider model =
    let
        arrowStyle = [ 
              padding 0
            , Element.pointer ]
        buttonStyle = [ 
                padding 8
            , width <| Element.px 28
            , Element.pointer
            , Element.mouseOver [Font.color darkBlueColor] ]

        anterior = Input.button
            arrowStyle
            { onPress = Just AnteriorPregunta
            , label = Style.arrow 27
            }

        siguiente = Input.button
            arrowStyle
            { onPress = Just SiguientePregunta
            , label = el [Element.htmlAttribute <| HAttrs.attribute "style" "transform: rotate(180deg);"] <| Style.arrow 24
            }

        primera = Input.button
            arrowStyle
            { onPress = Just PrimeraPregunta
            , label = Style.doubleArrow 27
            }

        ultima = Input.button
            arrowStyle
            { onPress = Just UltimaPregunta
            , label = el [Element.htmlAttribute <| HAttrs.attribute "style" "transform: rotate(180deg);"] <| Style.doubleArrow 24
            }

        dictSize = Dict.size model.preguntas

        (firstIndex, lastIndex) = surroundingIndices dictSize model.idPreguntaActual
        
        estiloMarcador = \index -> 
                    (if index == model.idPreguntaActual then [Background.color blue] else [Background.color gray80])
                ++
                [ Border.width 0, Border.color gray5, Border.rounded 3
                , Font.size 10, Font.bold, Font.center
                , width <| Element.px 23
                ] ++ buttonStyle

        label = \index -> 
            column [Font.center, centerY, centerX, spacing 3] [el [centerX] <| text <| toString index
                , el [centerX, Border.rounded 2, width <| Element.px 12, height <| Element.px 3
                    , Background.color (if tieneRespuesta index model.preguntas then greenColor else gray90)] Element.none]

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
