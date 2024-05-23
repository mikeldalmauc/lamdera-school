module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Cuestionario

type alias FrontendModel =
    { key : Key
    , device : Device
    , dimensions : Flags
    , message : String
    , cuestionario : Cuestionario.Model
    }
type alias Flags =
    { width : Int
    , height : Int
    }

type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | CuestionarioMsg Cuestionario.Msg
    | NoOpFrontendMsg
    | DeviceClassified Flags

type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend