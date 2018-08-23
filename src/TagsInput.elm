module TagsInput
    exposing
        ( Config
        , Msg
        , State
        , config
        , getTags
        , initialState
        , update
        , view
        )

{-| This library provides a flexible widget that allows the user to input
multiple "tags" (strings), which are displayed alongside the input. We expect
this to be a text input, and provide good keyboard event handling accordingly,
but allow for a user-defined view function. You can also pass in a set of
strings to use for autocomplete when entering a new tag.

See TagsInput.Views for predefined views.


# The Elm Architecture

@docs initialState, update, view


# Configuration

@docs config


# Output

@docs getTags


# Types

@docs Config, Msg, State

-}

import Html exposing (Html)
import Set exposing (Set)


{-| Messages for the input to update its internal state.
-}
type Msg
    = NoOp
    | SetIsFocused Bool
    | SetPendingTag String
    | CommitPendingTag
    | EditTag Bool Int
    | ShiftSelectedTag Int
    | DeleteTag Int


{-| The internal state of the input.
-}
type State
    = State
        { tags : List String
        , selectedIndex : Maybe Int
        , pendingTag : String
        , isFocused : Bool
        , ignoreSubmit : Bool
        }


{-| A configuration that describes how to display the input.
-}
type Config msg
    = Config
        { toMsg : Msg -> msg
        , view : State -> Config msg -> Set String -> Html msg
        }


{-| Create a [`State`](#State) describing a new input containing the given
initial list of tags to display.
-}
initialState : List String -> State
initialState tags =
    State
        { tags = tags
        , selectedIndex = Nothing
        , pendingTag = ""
        , isFocused = False
        , ignoreSubmit = False
        }


{-| Create a [`Config`](#Config) for an input using the given message tagger and custom
view function.
-}
config :
    (Msg -> msg)
    ->
        ({ tags : List String
         , selectedIndex : Maybe Int
         , pendingTag : String
         , isFocused : Bool
         , completeTags : Set String
         , focusTagger : Bool -> msg
         , inputTagger : String -> msg
         , keyTagger : Int -> msg
         , tagSelectKeyboardTagger : Int -> msg
         , tagSelectMouseTagger : Int -> msg
         , tagDeleteTagger : Int -> msg
         , submitMsg : msg
         }
         -> Html msg
        )
    -> Config msg
config toMsg viewFunc =
    let
        view =
            \(State state) (Config config) completeTags ->
                viewFunc
                    { tags = state.tags
                    , selectedIndex = state.selectedIndex
                    , pendingTag = state.pendingTag
                    , isFocused = state.isFocused
                    , completeTags = completeTags
                    , focusTagger = SetIsFocused >> toMsg
                    , inputTagger = SetPendingTag >> toMsg
                    , keyTagger = keyHandler (State state) >> toMsg
                    , tagSelectKeyboardTagger = EditTag True >> toMsg
                    , tagSelectMouseTagger = EditTag False >> toMsg
                    , tagDeleteTagger = DeleteTag >> toMsg
                    , submitMsg = CommitPendingTag |> toMsg
                    }
    in
    Config { toMsg = toMsg, view = view }


{-| Return the list of tags currently being displayed by the input, including
the text currently being edited. The result includes no duplicates but preserves
the order in which the tags are displayed.
-}
getTags : State -> List String
getTags (State state) =
    state.tags
        ++ (if
                String.isEmpty state.pendingTag
                    || List.member state.pendingTag state.tags
            then
                []
            else
                [ state.pendingTag ]
           )


{-| Update the internal state of the input.
-}
update : Msg -> State -> State
update msg (State state) =
    State
        (case msg of
            NoOp ->
                state

            SetIsFocused f ->
                { state | isFocused = f }

            SetPendingTag tag ->
                { state | pendingTag = tag, selectedIndex = Nothing }

            CommitPendingTag ->
                if state.ignoreSubmit then
                    { state | ignoreSubmit = False }
                else
                    { state
                        | pendingTag = ""
                        , tags =
                            if
                                String.isEmpty state.pendingTag
                                    || List.member state.pendingTag state.tags
                            then
                                state.tags
                            else
                                state.tags ++ [ state.pendingTag ]
                    }

            ShiftSelectedTag shift ->
                { state
                    | selectedIndex =
                        case state.selectedIndex of
                            Just i ->
                                let
                                    i2 =
                                        i + shift
                                in
                                if 0 <= i2 && i2 < List.length state.tags then
                                    Just i2
                                else
                                    Nothing

                            Nothing ->
                                if shift > 0 then
                                    Just 0
                                else
                                    Just (List.length state.tags - 1)
                }

            EditTag isEnter index ->
                let
                    (State newState) =
                        update (DeleteTag index) (State state)
                in
                { newState
                    | pendingTag =
                        Maybe.withDefault ""
                            (state.tags |> List.drop index |> List.head)
                    , selectedIndex = Nothing
                    , ignoreSubmit = isEnter
                }

            DeleteTag index ->
                { state
                    | selectedIndex =
                        Maybe.map
                            (\i ->
                                clamp 0
                                    (List.length state.tags - 2)
                                    (if i > index then
                                        i - 1
                                     else
                                        i
                                    )
                            )
                            state.selectedIndex
                    , tags = List.take index state.tags ++ List.drop (index + 1) state.tags
                }
        )


{-| The view for a given [`Config`](#Config), [`State`](#State), and set of
completion labels.
-}
view : State -> Config msg -> Set String -> Html msg
view state (Config config) completeLabels =
    config.view state (Config config) completeLabels


keyHandler : State -> number -> Msg
keyHandler (State state) k =
    -- If there is no pending input text, use the keyboard to navigate the
    -- existing labels.
    if not (String.isEmpty state.pendingTag) then
        NoOp
    else
        case k of
            -- Left arrow: move selected tag left.
            37 ->
                ShiftSelectedTag -1

            -- Right arrow: move select tag right.
            39 ->
                ShiftSelectedTag 1

            -- Delete: remove selected tag.
            46 ->
                case state.selectedIndex of
                    Just i ->
                        DeleteTag i

                    Nothing ->
                        NoOp

            -- Backspace: if a tag is selected, delete it; otherwise, edit the
            -- last tag (remove it and place its contents into the input).
            8 ->
                case state.selectedIndex of
                    Just i ->
                        DeleteTag i

                    Nothing ->
                        EditTag False (List.length state.tags - 1)

            -- Enter: edit selected tag.
            13 ->
                case state.selectedIndex of
                    Just i ->
                        EditTag True i

                    Nothing ->
                        NoOp

            _ ->
                NoOp
