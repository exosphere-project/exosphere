module OpenStack.ImageFilter exposing (ImageFilter(..))

import OpenStack.Types as OSTypes


type ImageFilter
    = ImageUuidFilter OSTypes.ImageUuid
    | ImageVisibilityFilter OSTypes.ImageVisibility
    | ImageNameFilter String
    | ImageOsDistroFilter String
    | ImageOsVersionFilter String
