module Types.OpenstackTypes exposing (Endpoint, EndpointInterface(..), Service, ServiceCatalog, ServiceName(..))

import Types.HelperTypes as HelperTypes



{-
   Types that match structure of data returned from OpenStack API, used for
   decoding JSON. Not necessarily how we ultimately want to store in the app.
-}


type alias ServiceCatalog =
    List Service


type alias Service =
    { name : String
    , type_ : String
    , endpoints : List Endpoint
    }


type ServiceName
    = Glance
    | Nova
    | Neutron


type alias Endpoint =
    { interface : EndpointInterface
    , url : HelperTypes.Url
    }


type EndpointInterface
    = Public
    | Admin
    | Internal
