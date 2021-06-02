module Types.HelperTypes exposing (Hostname, IPv4AddressPublicRoutability(..), Password, Url, Uuid)


type alias Url =
    String


type alias Hostname =
    String


type alias Uuid =
    String


type alias Password =
    String


type IPv4AddressPublicRoutability
    = PrivateRfc1918Space
    | PublicNonRfc1918Space
