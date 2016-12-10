port module Storage exposing (..)

import Models exposing (..)


-- Input port (from JS) --


port getStorage : (Model -> msg) -> Sub msg


-- Output port (send to JS) --


port setStorage : Model -> Cmd msg
