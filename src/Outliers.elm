module Outliers exposing (..)


expectJsonResponse : Json.Decoder body -> Http.Response body
expectJsonResponse decoder =
    Http.expectStringResponse (\{ url, status, headers, body } -> Json.decodeString decoder body |> Result.map (\jsonBody -> { url = url, status = status, headers = headers, body = jsonBody }))


expectAuthenticationResponse =
    Http.expectStringResponse
        (\{ url, status, headers, body } ->
            Json.decodeString successDecoder body
                |> Result.map
                    (\jsonBody ->
                        let
                            _ =
                                Debug.log "auth" { url = url, status = status, headers = headers, body = jsonBody }
                        in
                            jsonBody
                    )
        )
