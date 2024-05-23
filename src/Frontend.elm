module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Lamdera
import Types exposing (..)
import Url
import Cuestionario as Cuestionario
import Element as El exposing (layout, explain, map)
import Element.Background as Background
import Html.Attributes exposing (style)
import Style exposing (..)

type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , cuestionario = Tuple.first Cuestionario.init
      }
    , Cmd.none
    )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Sub.batch[
        CuestionarioMsg (Cuestionario.subscriptions model.cuestionario)
        , onResize 
            (\width height ->
                DeviceClassified { width = width, height = height })
    ]

update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        DeviceClassified flags ->
                ( { model | device = (Element.classifyDevice flags), dimensions = flags } , Cmd.none)

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        CuestionarioMsg cusestionarioMsg ->
            let
                ( cuestionario, cmd ) =
                    Cuestionario.update cusestionarioMsg model.cuestionario
            in
            ( { model | cuestionario = cuestionario }, Cmd.map CuestionarioMsg cmd )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body = [
               El.layout ( [ 
                    Background.color Style.gray90
                ] ++ baseFontAttrs
               )
                <| El.map (\msg -> CuestionarioMsg msg) (Cuestionario.view model.cuestionario)
        ]
    }