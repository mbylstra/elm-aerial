import AutoComplete

type Msg =
      AutoCompleteMsg AutoComplete.Msg
    | MouseLeave
    | MouseEnter

udpate : Msg -> Model -> Model
update msg model =
    case msg of
        M1 ->
            model
        M2 ->
            model
        AutoCompleteMsg autoCompleteMsg ->
            let
                (autoComplete, maybeOutMsg) =
                    AutoComplete.update autoCompleteMsg model.autoComplete
                newModel = { model | autoComplete = autoComplete }
            in
                handleAutoCompleteOutMsg maybeOutMsg newModel

handleAutoCompleteOutMsg maybeOutMsg model =
    case outMsg of
        Just outMsg ->
            case outMsg of
                -- have to admit this stuff is repetitive
                AutoComplete.MouseLeave ->
                    update MouseLeave newModel
                AutoComplete.MouseEnter ->
                    update MouseEnter newModel
                _ ->
                    newModel
        Nothing ->
            newModel




--------------------------------------------------------------------------------
import AutoComplete

type Msg =
    | MouseLeave
    | MouseEnter
    | AutoCompleteMsg AutoComplete.Msg


autoCompleteUpdateConfig =
    { onMouseLeave = Just MouseLeave
    , onMouseEnter = Just MouseEnter
    , blah = Nothing
    , blah = Nothing
    }

udpate : Msg -> Model -> Model
update msg model =
    case msg of
        M1 ->
            model
        M2 ->
            model
        AutoCompleteMsg autoCompleteMsg ->
            let
                (autoComplete, outMsg) =
                    AutoComplete.update
                        autoCompleteUdpateConfig
                        autoCompleteMsg
                        model.autoComplete
                newModel = { model | autoComplete = autoComplete }

            in
                case outMsg of
                    Just outMsg ->
                        update outMsg newModel
                    Nothing ->
                        newModel



{-

There are pros and cons to previous approach:
    pros:
        - less arguments to the update function. consistent will all TEA components
        - you have access to the model, so you can do some kind of translation between
        the outmsg event and one of your own events. Eg: rather than route and mapClick
        event to an internal message and call the update function again, just
        update the model there and then. The config pattern does not provide
        this option at all.
        WHy do we need to recurse through the update function again?.
    cons:
        definitely potential to fuck up the double update and provide the wrong
        model to one of them (very not DRY). Maybe this can be fixed though.
        -- I dunno, I still think there's something in this idea of not
            having an outmsg type.
    statemate:
        Not sure which is more or less confusing. The huge update arg list makes it confusing,
        but that should be solved with a record.


-}

--  third option

import AutoComplete

type Msg =
      AutoCompleteMsg AutoComplete.Msg
    | MouseLeave
    | MouseEnter

udpate : Msg -> Model -> Model
update msg model =
    case msg of
        M1 ->
            model
        M2 ->
            model
        AutoCompleteMsg autoCompleteMsg ->
            let
                (autoComplete, maybeOutMsg) =
                    AutoComplete.update autoCompleteMsg model.autoComplete
                newModel = { model | autoComplete = autoComplete }
            in
                handleAutoCompleteOutMsg maybeOutMsg newModel

handleAutoCompleteOutMsg maybeOutMsg model =
    case outMsg of
        Just outMsg ->
            (outMsgToFunction outMsg) model
        Nothing ->
            newModel

outMsgToFunction outMsg model =
        case outMsg of
            -- have to admit this stuff is repetitive
            AutoComplete.MouseLeave ->
                mouseLeave
            AutoComplete.MouseEnter ->
                mouseEnter
            _ ->
                (\_ -> model)


mouseLeave model =
    model -- do something to model

mouseEnter model =
    model -- do something to model
