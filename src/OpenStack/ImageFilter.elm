module OpenStack.ImageFilter exposing (ImageFilter(..))

import OpenStack.Types as OSTypes


type ImageFilter
    = ImageUuidFilter OSTypes.ImageUuid
    | ImageVisibilityFilter OSTypes.ImageVisibility
    | ImageNameFilter String
    | ImageOsDistroFilter String
    | ImageOsVersionFilter String


applyImageFilter : ImageFilter -> List OSTypes.Image -> List OSTypes.Image
applyImageFilter filter images =
    let
        filterFunc : OSTypes.Image -> Bool
        filterFunc image =
            case filter of
                ImageUuidFilter uuid ->
                    image.uuid == uuid

                ImageVisibilityFilter visibility ->
                    image.visibility == visibility

                ImageNameFilter name ->
                    image.name == name

                ImageOsDistroFilter osDistro ->
                    image.osDistro
                        |> Maybe.map (\osDistro_ -> osDistro_ == osDistro)
                        |> Maybe.withDefault False

                ImageOsVersionFilter osVersion ->
                    image.osVersion
                        |> Maybe.map (\osVersion_ -> osVersion_ == osVersion)
                        |> Maybe.withDefault False
    in
    List.filter filterFunc images
