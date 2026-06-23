module JsonRender.Render exposing
    ( Model, init
    , Msg, update
    , Effect(..)
    , view
    )

{-| Render a validated [`Spec`](JsonRender-Spec#Spec) to `Html`, driven by host-owned
state, with actions flowing back out as [`Effect`](#Effect) values.

This is a small TEA component. The host owns the json-render `state` and passes it to
[`view`](#view) on every frame; the renderer reads it but never mutates it behind the
host's back. The only local state the renderer owns is the pending confirm dialog. User
actions (button presses, checkbox toggles) surface to the host as `Effect`s the host
applies — exactly mirroring the framework-neutral host↔renderer contract.

Because the output is Elm `Html` (no `innerHTML`, no script escape hatch), the rendered
tree is **XSS-safe by construction**.


# State

@docs Model, init


# Messages

@docs Msg, update


# Effects (out to the host)

@docs Effect


# View

@docs view

-}

import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import JsonRender.Expr as Expr exposing (Context)
import JsonRender.Spec as Spec
    exposing
        ( ActionBinding
        , Confirm
        , Props(..)
        , Repeat
        , Spec
        , UIElement
        )


{-| Renderer-local UI state. Holds only the pending confirm dialog (if any); the
host owns everything else.
-}
type Model
    = Model { pendingConfirm : Maybe Emit }


{-| The initial (empty) renderer state: no dialog open.
-}
init : Model
init =
    Model { pendingConfirm = Nothing }


{-| A fully-resolved intent to emit an action. Params are resolved against the press-time
context (so per-row scope is captured); confirm strings are resolved too.
-}
type alias Emit =
    { verb : String
    , params : Value
    , confirm : Maybe ResolvedConfirm
    }


type alias ResolvedConfirm =
    { title : String
    , message : String
    , confirmLabel : String
    , cancelLabel : String
    , variant : String
    }


{-| Renderer messages. Opaque to the host: the host maps `Html Msg` into its own message
type and feeds `Msg`s back through [`update`](#update), acting on the returned
[`Effect`](#Effect).
-}
type Msg
    = Pressed Emit
    | ConfirmAccepted
    | ConfirmDismissed
    | Toggled String Bool


{-| What the renderer asks the host to do. The renderer never performs the side effect
itself.

  - `EmitAction { verb, params }` — a wired `on:press` fired (and any `confirm` was
    accepted). `params` are already expression-resolved. The host re-checks the verb
    against its allowlist and performs the side effect.
  - `EmitStateChange { path, value }` — a two-way input (`$bindState` / `$bindItem`
    checkbox) was toggled. The host treats this as the source of truth, writes it at the
    given absolute JSON Pointer, and re-projects state.

-}
type Effect
    = EmitAction { verb : String, params : Value }
    | EmitStateChange { path : String, value : Value }


{-| Advance renderer-local state in response to a `Msg`, optionally yielding an `Effect`
for the host to apply.

`confirm` is honored here: a press carrying a confirm opens the dialog and emits nothing;
the `EmitAction` only fires once the user accepts.

-}
update : Msg -> Model -> ( Model, Maybe Effect )
update msg (Model model) =
    case msg of
        Pressed emit ->
            case emit.confirm of
                Just _ ->
                    ( Model { model | pendingConfirm = Just emit }, Nothing )

                Nothing ->
                    ( Model model, Just (emitAction emit) )

        ConfirmAccepted ->
            case model.pendingConfirm of
                Just emit ->
                    ( Model { model | pendingConfirm = Nothing }, Just (emitAction emit) )

                Nothing ->
                    ( Model model, Nothing )

        ConfirmDismissed ->
            ( Model { model | pendingConfirm = Nothing }, Nothing )

        Toggled path value ->
            ( Model model, Just (EmitStateChange { path = path, value = Encode.bool value }) )


emitAction : Emit -> Effect
emitAction emit =
    EmitAction { verb = emit.verb, params = emit.params }



-- VIEW


{-| Render the spec against the current host-owned `state`. The returned `Html Msg`
includes the confirm dialog overlay when one is pending.
-}
view : Spec -> Value -> Model -> Html Msg
view spec state (Model model) =
    Html.div [ Attr.class "jr-root" ]
        [ renderElement spec (Expr.rootContext state) spec.root
        , confirmOverlay model.pendingConfirm
        ]


renderElement : Spec -> Context -> String -> Html Msg
renderElement spec ctx id =
    case Dict.get id spec.elements of
        Just element ->
            renderUIElement spec ctx element

        Nothing ->
            -- Cannot happen for a decoded spec (child refs are validated); fail-closed stub.
            Html.div [ Attr.class "jr-error" ]
                [ Html.text ("Missing element: " ++ id) ]


renderUIElement : Spec -> Context -> UIElement -> Html Msg
renderUIElement spec ctx element =
    let
        childrenHtml =
            case element.repeat of
                Just repeat ->
                    repeatChildren spec ctx element repeat

                Nothing ->
                    List.map (renderElement spec ctx) element.children
    in
    renderComponent ctx element childrenHtml


repeatChildren : Spec -> Context -> UIElement -> Repeat -> List (Html Msg)
repeatChildren spec ctx element repeat =
    let
        items =
            arrayAt repeat.statePath ctx.state

        renderRow index item =
            let
                rowCtx =
                    Expr.childContext repeat.statePath index item ctx
            in
            List.map (renderElement spec rowCtx) element.children
    in
    List.concat (List.indexedMap renderRow items)


renderComponent : Context -> UIElement -> List (Html Msg) -> Html Msg
renderComponent ctx element childrenHtml =
    case element.props of
        CardP props ->
            renderCard ctx props childrenHtml

        StackP props ->
            renderStack props childrenHtml

        TextP props ->
            Html.span [ Attr.class "jr-text" ]
                [ Html.text (Expr.resolveDisplay ctx props.value) ]

        BadgeP props ->
            renderBadge ctx props

        ButtonP props ->
            renderButton ctx element props

        CheckboxP props ->
            renderCheckbox ctx props

        FindingsTableP props ->
            renderFindingsTable ctx props


renderCard : Context -> Spec.CardProps -> List (Html Msg) -> Html Msg
renderCard ctx props childrenHtml =
    let
        titleHtml =
            case props.title of
                Just expr ->
                    [ Html.h2 [ Attr.class "jr-card__title" ]
                        [ Html.text (Expr.resolveDisplay ctx expr) ]
                    ]

                Nothing ->
                    []
    in
    Html.div [ Attr.class "jr-card" ] (titleHtml ++ childrenHtml)


renderStack : Spec.StackProps -> List (Html Msg) -> Html Msg
renderStack props childrenHtml =
    let
        directionClass =
            case props.direction of
                Spec.Row ->
                    "jr-stack--row"

                Spec.Col ->
                    "jr-stack--col"
    in
    Html.div
        [ Attr.class ("jr-stack " ++ directionClass)
        , Attr.attribute "data-gap" (String.fromInt props.gap)
        ]
        childrenHtml


renderBadge : Context -> Spec.BadgeProps -> Html Msg
renderBadge ctx props =
    let
        state =
            Expr.resolveDisplay ctx props.value
    in
    Html.span
        [ Attr.class ("jr-badge jr-badge--" ++ badgeTone state)
        , Attr.attribute "data-state" state
        ]
        [ Html.text state ]


{-| Tone mapping from a per-row `scanState` string. Identical to the Track A island so
both renderers match (`pinned-format-reference.md` §"STILL-UNCERTAIN" item 3).
-}
badgeTone : String -> String
badgeTone state =
    case state of
        "idle" ->
            "neutral"

        "queued" ->
            "info"

        "running" ->
            "info"

        "done" ->
            "success"

        "error" ->
            "danger"

        _ ->
            "neutral"


renderButton : Context -> UIElement -> Spec.ButtonProps -> Html Msg
renderButton ctx element props =
    let
        handler =
            case pressEmit ctx element of
                Just emit ->
                    [ Events.onClick (Pressed emit) ]

                Nothing ->
                    []
    in
    Html.button
        (Attr.class "jr-button" :: Attr.type_ "button" :: handler)
        [ Html.text (Expr.resolveDisplay ctx props.label) ]


pressEmit : Context -> UIElement -> Maybe Emit
pressEmit ctx element =
    Dict.get "press" element.on
        |> Maybe.andThen List.head
        |> Maybe.map (buildEmit ctx)


buildEmit : Context -> ActionBinding -> Emit
buildEmit ctx binding =
    { verb = binding.action
    , params = Expr.resolveParams ctx binding.params
    , confirm = Maybe.map (resolveConfirm ctx) binding.confirm
    }


resolveConfirm : Context -> Confirm -> ResolvedConfirm
resolveConfirm ctx confirm =
    { title = Expr.resolveDisplay ctx confirm.title
    , message = Expr.resolveDisplay ctx confirm.message
    , confirmLabel = Maybe.withDefault "Confirm" confirm.confirmLabel
    , cancelLabel = Maybe.withDefault "Cancel" confirm.cancelLabel
    , variant = confirm.variant
    }


renderCheckbox : Context -> Spec.CheckboxProps -> Html Msg
renderCheckbox ctx props =
    let
        isChecked =
            props.checked |> Maybe.map (Expr.resolveBool ctx) |> Maybe.withDefault False

        writeBack =
            props.checked |> Maybe.andThen (Expr.writeBackPath ctx)

        handler =
            case writeBack of
                Just path ->
                    [ Events.onCheck (Toggled path) ]

                Nothing ->
                    []

        labelHtml =
            case props.label of
                Just expr ->
                    [ Html.span [ Attr.class "jr-checkbox__label" ]
                        [ Html.text (Expr.resolveDisplay ctx expr) ]
                    ]

                Nothing ->
                    []
    in
    Html.label [ Attr.class "jr-checkbox" ]
        (Html.input
            (Attr.type_ "checkbox" :: Attr.checked isChecked :: handler)
            []
            :: labelHtml
        )


renderFindingsTable : Context -> Spec.FindingsTableProps -> Html Msg
renderFindingsTable ctx props =
    let
        value =
            Expr.resolve ctx props.bind
    in
    if isNull value then
        Html.div [ Attr.class "jr-findings jr-findings--empty" ]
            [ Html.text "No findings yet" ]

    else
        renderFindings props.groupBy value


renderFindings : String -> Value -> Html Msg
renderFindings groupBy value =
    let
        findings =
            Decode.decodeValue (Decode.list (Decode.dict Decode.value)) value
                |> Result.withDefault []

        groups =
            groupFindings groupBy findings
    in
    Html.div [ Attr.class "jr-findings" ]
        (List.map renderFindingGroup groups)


renderFindingGroup : ( String, Int ) -> Html Msg
renderFindingGroup ( label, count ) =
    Html.div [ Attr.class ("jr-findings__group jr-findings__group--" ++ label) ]
        [ Html.span [ Attr.class "jr-findings__severity" ] [ Html.text label ]
        , Html.span [ Attr.class "jr-findings__count" ] [ Html.text (String.fromInt count) ]
        ]


groupFindings : String -> List (Dict.Dict String Value) -> List ( String, Int )
groupFindings groupBy findings =
    findings
        |> List.foldr
            (\finding acc ->
                let
                    key =
                        Dict.get groupBy finding
                            |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                            |> Maybe.withDefault "ungrouped"
                in
                Dict.update key (Maybe.withDefault 0 >> (+) 1 >> Just) acc
            )
            Dict.empty
        |> Dict.toList



-- CONFIRM DIALOG


confirmOverlay : Maybe Emit -> Html Msg
confirmOverlay pending =
    case pending |> Maybe.andThen .confirm of
        Just confirm ->
            Html.div [ Attr.class ("jr-confirm jr-confirm--" ++ confirm.variant) ]
                [ Html.div [ Attr.class "jr-confirm__box" ]
                    [ Html.h3 [ Attr.class "jr-confirm__title" ] [ Html.text confirm.title ]
                    , Html.p [ Attr.class "jr-confirm__message" ] [ Html.text confirm.message ]
                    , Html.div [ Attr.class "jr-confirm__actions" ]
                        [ Html.button
                            [ Attr.class "jr-confirm__cancel"
                            , Attr.type_ "button"
                            , Events.onClick ConfirmDismissed
                            ]
                            [ Html.text confirm.cancelLabel ]
                        , Html.button
                            [ Attr.class "jr-confirm__confirm"
                            , Attr.type_ "button"
                            , Events.onClick ConfirmAccepted
                            ]
                            [ Html.text confirm.confirmLabel ]
                        ]
                    ]
                ]

        Nothing ->
            Html.text ""



-- HELPERS


arrayAt : String -> Value -> List Value
arrayAt pointer state =
    Expr.getByPath pointer state
        |> Maybe.andThen (Decode.decodeValue (Decode.list Decode.value) >> Result.toMaybe)
        |> Maybe.withDefault []


isNull : Value -> Bool
isNull value =
    Decode.decodeValue (Decode.null True) value
        |> Result.withDefault False
