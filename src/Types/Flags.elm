module Types.Flags exposing (ConfigurationFlags, Flags, flagsDecoder)

import Json.Decode exposing (Decoder, Error(..), Value, bool, int, maybe, nullable, string, succeed, value)
import Json.Decode.Pipeline exposing (required)
import Types.HelperTypes as HelperTypes exposing (OpenIdConnectLoginConfig)


type alias Flags =
    String


type alias ThemePalettes =
    { light : PaletteColors
    , dark : PaletteColors
    }


type alias PaletteColors =
    { primary : RGBTriplet
    , secondary : RGBTriplet
    }


type alias RGBTriplet =
    { r : Int
    , g : Int
    , b : Int
    }


type alias ConfigurationFlags =
    -- Flags intended to be configured by cloud operators
    { showDebugMsgs : Bool
    , cloudCorsProxyUrl : Maybe HelperTypes.Url
    , urlPathPrefix : Maybe String
    , appTitle : Maybe String
    , topBarShowAppTitle : Bool
    , palette : Maybe ThemePalettes
    , logo : Maybe String
    , favicon : Maybe String
    , defaultLoginView : Maybe String
    , aboutAppMarkdown : Maybe String
    , supportInfoMarkdown : Maybe String
    , userSupportEmailAddress : Maybe String
    , userSupportEmailSubject : Maybe String
    , openIdConnectLoginConfig :
        Maybe HelperTypes.OpenIdConnectLoginConfig
    , localization : Maybe HelperTypes.Localization
    , clouds : Value
    , instanceConfigMgtRepoUrl : Maybe String
    , instanceConfigMgtRepoCheckout : Maybe String
    , sentryConfig : Maybe HelperTypes.SentryConfig

    -- Flags that Exosphere sets dynamically
    , localeGuessingString : String
    , width : Int
    , height : Int
    , storedState : Maybe Value
    , randomSeed0 : Int
    , randomSeed1 : Int
    , randomSeed2 : Int
    , randomSeed3 : Int
    , epoch : Int
    , themePreference : Maybe String
    , timeZone : Int
    }


flagsDecoder : Decoder ConfigurationFlags
flagsDecoder =
    succeed ConfigurationFlags
        |> required "showDebugMsgs" bool
        |> required "cloudCorsProxyUrl" (maybe string)
        |> required "urlPathPrefix" (maybe string)
        |> required "appTitle" (maybe string)
        |> required "topBarShowAppTitle" bool
        |> required "palette" (nullable themePalettesDecoder)
        |> required "logo" (maybe string)
        |> required "favicon" (maybe string)
        |> required "defaultLoginView" (maybe string)
        |> required "aboutAppMarkdown" (maybe string)
        |> required "supportInfoMarkdown" (maybe string)
        |> required "userSupportEmailAddress" (maybe string)
        |> required "userSupportEmailSubject" (maybe string)
        |> required "openIdConnectLoginConfig" (nullable openIdConnectLoginConfigDecoder)
        |> required "localization" (nullable localizationDecoder)
        |> required "clouds" value
        |> required "instanceConfigMgtRepoUrl" (maybe string)
        |> required "instanceConfigMgtRepoCheckout" (maybe string)
        |> required "sentryConfig" (nullable sentryConfigDecoder)
        |> required "localeGuessingString" string
        |> required "width" int
        |> required "height" int
        |> required "storedState" (maybe value)
        |> required "randomSeed0" int
        |> required "randomSeed1" int
        |> required "randomSeed2" int
        |> required "randomSeed3" int
        |> required "epoch" int
        |> required "themePreference" (maybe string)
        |> required "timeZone" int


themePalettesDecoder : Decoder ThemePalettes
themePalettesDecoder =
    succeed ThemePalettes
        |> required "light" paletteColorsDecoder
        |> required "dark" paletteColorsDecoder


paletteColorsDecoder : Decoder PaletteColors
paletteColorsDecoder =
    succeed PaletteColors
        |> required "primary" rgbTripletDecoder
        |> required "secondary" rgbTripletDecoder


rgbTripletDecoder : Decoder RGBTriplet
rgbTripletDecoder =
    succeed RGBTriplet
        |> required "r" int
        |> required "g" int
        |> required "b" int


openIdConnectLoginConfigDecoder : Decoder OpenIdConnectLoginConfig
openIdConnectLoginConfigDecoder =
    succeed OpenIdConnectLoginConfig
        |> required "keystoneAuthUrl" string
        |> required "webssoKeystoneEndpoint" string
        |> required "oidcLoginIcon" string
        |> required "oidcLoginButtonLabel" string
        |> required "oidcLoginButtonDescription" string


localizationDecoder : Decoder HelperTypes.Localization
localizationDecoder =
    succeed HelperTypes.Localization
        |> required "openstackWithOwnKeystone" string
        |> required "openstackSharingKeystoneWithAnother" string
        |> required "unitOfTenancy" string
        |> required "maxResourcesPerProject" string
        |> required "pkiPublicKeyForSsh" string
        |> required "virtualComputer" string
        |> required "virtualComputerHardwareConfig" string
        |> required "cloudInitData" string
        |> required "commandDrivenTextInterface" string
        |> required "staticRepresentationOfBlockDeviceContents" string
        |> required "blockDevice" string
        |> required "nonFloatingIpAddress" string
        |> required "floatingIpAddress" string
        |> required "publiclyRoutableIpAddress" string
        |> required "graphicalDesktopEnvironment" string


sentryConfigDecoder : Decoder HelperTypes.SentryConfig
sentryConfigDecoder =
    succeed HelperTypes.SentryConfig
        |> required "dsnPublicKey" string
        |> required "dsnHost" string
        |> required "dsnProjectId" string
        |> required "releaseVersion" string
        |> required "environmentName" string
