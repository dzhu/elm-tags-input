module TagsInput.Views exposing (bulmaTagsView, completingInput, configForBulma)

{-| Some predefined views for use with `TagsInput`. For each view, we expose a
full [`Config`](#Config) to use with this module, as well as a view

@docs configForBulma, bulmaTagsView, completingInput

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onBlur, onFocus, onInput, onSubmit)
import Html.Keyed as Keyed
import Json.Decode
import Set exposing (Set)
import TagsInput exposing (Config, Msg, config)


{-| Create HTML elements that implement a text input with dropdown
completion. You provide an ID for the list of completions, a list of additional
attributes to give the text input, and the set of strings to complete against.
-}
completingInput : String -> List (Attribute msg) -> Set String -> List (Html msg)
completingInput listId inputAttributes completions =
    [ datalist [ id listId ]
        (List.map (\l -> option [ value l ] []) (Set.toList completions))
    , input ([ class "input", list listId, type_ "text" ] ++ inputAttributes) []
    ]


{-| Tags-only view for Bulma.
-}
bulmaTagsView :
    (Int -> String -> msg)
    -> Maybe (Int -> String -> msg)
    -> Bool
    -> Maybe Int
    -> List String
    -> List (Html msg)
bulmaTagsView tagToMsg deleteToMsg allActive activeIndex tags =
    let
        makeTagLink : Int -> String -> Html msg
        makeTagLink index tag =
            div [ class "control" ]
                [ div [ class "tags has-addons" ]
                    [ a
                        [ classList
                            [ ( "tag", True )
                            , ( "is-primary", allActive || activeIndex == Just index )
                            ]
                        , Html.Events.onClick (tagToMsg index tag)
                        ]
                        [ text tag ]
                    , case deleteToMsg of
                        Just fn ->
                            a
                                [ class "tag is-danger is-delete"
                                , Html.Events.onClick (fn index tag)
                                ]
                                []

                        Nothing ->
                            text ""
                    ]
                ]
    in
    List.indexedMap makeTagLink tags


{-| Create a [`Config`](TagsInput#Config) containing a view function intended to
fit in a [Bulma](https://bulma.io) page, with the given message tagger and extra
attributes on the text input.
-}
configForBulma : (Msg -> msg) -> List (Attribute msg) -> Config msg
configForBulma toMsg attributes =
    config toMsg
        (\viewInput ->
            -- Use keyed HTML so that the input is not replaced and retains focus after
            -- being submitted.
            Keyed.node "div"
                [ classList
                    [ ( "field", True )
                    , ( "is-grouped", True )
                    , ( "input", True )
                    , ( "is-focused", viewInput.isFocused )
                    ]
                ]
                (List.map2 (,)
                    viewInput.tags
                    (bulmaTagsView
                        (\ind tag -> viewInput.tagSelectMouseTagger ind)
                        (Just (\ind tag -> viewInput.tagDeleteTagger ind))
                        False
                        viewInput.selectedIndex
                        viewInput.tags
                    )
                    ++ [ ( "input"
                         , div
                            [ class "control is-expanded" ]
                            -- Wrap in a form instead of checking for the Enter
                            -- keypress so that pressing Enter while the completion
                            -- dropdown is exposed does the completion instead of
                            -- adding the tag.
                            [ Html.form
                                [ onSubmit viewInput.submitMsg
                                , style [ ( "margin", "0" ) ]
                                ]
                                (completingInput
                                    "complete-list-id"
                                    -- Do some non-Bulma-standard style hacking
                                    -- so that the grandparent div looks like a
                                    -- text input and the actual input fits
                                    -- inside it seamlessly.
                                    ([ style
                                        [ ( "padding", "0" )
                                        , ( "border-width", "0" )
                                        , ( "height", "initial" )
                                        , ( "box-shadow", "initial" )
                                        ]
                                     , onFocus (viewInput.focusTagger True)
                                     , onBlur (viewInput.focusTagger False)
                                     , onInput viewInput.inputTagger
                                     , on "keydown" (Json.Decode.map viewInput.keyTagger keyCode)
                                     , value viewInput.pendingTag
                                     ]
                                        ++ attributes
                                    )
                                    viewInput.completeTags
                                )
                            ]
                         )
                       ]
                )
        )
