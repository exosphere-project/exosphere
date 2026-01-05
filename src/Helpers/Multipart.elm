module Helpers.Multipart exposing
    ( Boundary
    , Header
    , Multipart
    , addAttachment
    , addMultipart
    , addStringPart
    , alternative
    , boundary
    , header
    , mixed
    , string
    )

import Base64


type Multipart
    = Multipart ContentType Boundary (List Part)


type alias ContentType =
    String


type Boundary
    = Boundary String


boundary : String -> Boundary
boundary =
    String.toList
        >> List.map (Char.toCode >> clamp 32 126 >> Char.fromCode)
        >> String.fromList
        >> String.padLeft 38 '='
        >> String.padRight 40 '='
        >> Boundary


mixed : Boundary -> Multipart
mixed b =
    Multipart "multipart/mixed" b []


alternative : Boundary -> Multipart
alternative b =
    Multipart "multipart/alternative" b []


{-| Multipart headers, basically the same as Http headers
-}
type Header
    = Header String String


header : String -> String -> Header
header =
    Header


headerToString : Header -> String
headerToString (Header k v) =
    k ++ ": " ++ v


headersToString : List Header -> String
headersToString =
    List.map headerToString
        >> String.join crlf



-- Commonly used headers


mimeVersionHeader : Header
mimeVersionHeader =
    header "MIME-Version" "1.0"


attachmentHeader : String -> Header
attachmentHeader filename =
    header "Content-Disposition" ("attachment; filename=\"" ++ filename ++ "\"")


{-| The multipart body parts
-}
type Part
    = Part (List Header) String
    | Nested Multipart


addMultipart : Multipart -> Multipart -> Multipart
addMultipart nested (Multipart type_ b parts) =
    Multipart type_ b (parts ++ [ Nested nested ])


addStringPart : String -> List Header -> String -> Multipart -> Multipart
addStringPart contentType headers content (Multipart type_ b parts) =
    Multipart type_ b <|
        (parts ++ [ Part (header "Content-Type" contentType :: headers) content ])



-- Quick helper to add a string part as a named file attachment


addAttachment : String -> String -> List Header -> String -> Multipart -> Multipart
addAttachment contentType filename headers =
    addStringPart contentType (attachmentHeader filename :: headers)


{-| Content encoding to properly support extended ascii and beyond
-}
type ContentEncoding
    = Encoding7Bit
    | Encoding8Bit
    | EncodingBase64


determineEncodingFor : String -> ContentEncoding
determineEncodingFor content =
    let
        maxCharCode =
            content
                |> String.toList
                |> List.map Char.toCode
                |> List.maximum
                |> Maybe.withDefault 0
    in
    if maxCharCode < 128 then
        Encoding7Bit

    else if maxCharCode < 256 then
        Encoding8Bit

    else
        EncodingBase64


encodeContent : String -> ( Header, String )
encodeContent content =
    case determineEncodingFor content of
        Encoding7Bit ->
            ( header "Content-Transfer-Encoding" "7bit", content )

        Encoding8Bit ->
            ( header "Content-Transfer-Encoding" "8bit", content )

        EncodingBase64 ->
            ( header "Content-Transfer-Encoding" "base64"
            , Base64.encode content
            )


partToString : Part -> String
partToString part =
    case part of
        Part headers content ->
            let
                ( contentTransferEncodingHeader, encodedContent ) =
                    encodeContent content
            in
            String.concat
                [ headersToString (contentTransferEncodingHeader :: headers)
                , crlf
                , crlf
                , encodedContent
                ]

        Nested multipart ->
            string multipart


string : Multipart -> String
string (Multipart type_ (Boundary bs) parts) =
    case parts of
        [] ->
            ""

        part :: [] ->
            partToString part

        list ->
            let
                headers =
                    [ mimeVersionHeader
                    , header "Content-Transfer-Encoding" "7bit"
                    , header "Content-Type" (type_ ++ "; boundary=\"" ++ bs ++ "\"")
                    , header "Number-Attachments" (String.fromInt <| List.length list)
                    ]
            in
            String.join
                (crlf ++ crlf ++ "--" ++ bs ++ crlf)
                (headersToString headers
                    :: List.map partToString list
                )
                ++ crlf
                ++ crlf
                ++ "--"
                ++ bs
                ++ "--"


crlf : String
crlf =
    "\u{000D}\n"
