module EditableTable exposing (EditState, TableEvent(..), editNothing, editRow, newRow, viewTableRows)

import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Html.Events exposing (onClick)
import Maybe.Extra
import Misc exposing (bootstrapIcon)


type TableEvent
    = StartEdit Int
    | AcceptEdit
    | CancelEdit
    | RemoveRow Int
    | AddRow
    | SaveNewRow


type EditState
    = NoEdit
    | New
    | Exists Int


type alias EditRowFunc msg =
    Maybe Int -> List (Table.Cell msg)


type alias DisplayRowFunc entry msg =
    Int -> entry -> List (Table.Cell msg)


type alias TableEventFunc msg =
    TableEvent -> msg


editNothing =
    NoEdit


editRow =
    Exists


newRow =
    New


displayRow : TableEventFunc msg -> DisplayRowFunc entry msg -> Int -> entry -> Table.Row msg
displayRow eventMsg displayCells i t =
    Table.tr []
        (displayCells i t
            ++ [ Table.td []
                    [ Button.button
                        [ Button.outlinePrimary
                        , Button.attrs [ Spacing.ml1, onClick (eventMsg (StartEdit i)) ]
                        ]
                        [ bootstrapIcon "pencil" ]
                    ]
               ]
        )


editableRow : TableEventFunc msg -> EditRowFunc msg -> Maybe Int -> Table.Row msg
editableRow eventMsg editCells maybe_i =
    let
        basic_buttons =
            [ Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ onClick (eventMsg AcceptEdit) ]
                ]
                [ bootstrapIcon "check" ]
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs
                    [ Spacing.ml1
                    , onClick (eventMsg CancelEdit)
                    ]
                ]
                [ bootstrapIcon "x" ]
            ]

        remove_button =
            maybe_i
                |> Maybe.map
                    (\i ->
                        [ Button.button
                            [ Button.outlinePrimary
                            , Button.attrs
                                [ Spacing.ml1
                                , onClick (eventMsg (RemoveRow i))
                                ]
                            ]
                            [ bootstrapIcon "trash" ]
                        ]
                    )
                |> Maybe.withDefault []

        buttons =
            basic_buttons ++ remove_button
    in
    Table.tr []
        (editCells maybe_i
            ++ [ Table.td [] buttons ]
        )


viewTableRows :
    List ( Int, entry )
    -> DisplayRowFunc entry msg
    -> EditRowFunc msg
    -> TableEventFunc msg
    -> EditState
    -> List (Table.Row msg)
viewTableRows numberedEntries displayCells editCells eventMsg editState =
    let
        addRow : Maybe (Table.Row msg)
        addRow =
            case editState of
                New ->
                    Just (editableRow eventMsg editCells Nothing)

                _ ->
                    Nothing

        toRow : ( Int, entry ) -> Table.Row msg
        toRow ( i, t ) =
            case editState of
                Exists j ->
                    if i == j then
                        editableRow eventMsg editCells (Just i)

                    else
                        displayRow eventMsg displayCells i t

                _ ->
                    displayRow eventMsg displayCells i t
    in
    Maybe.Extra.cons addRow (List.map toRow numberedEntries)
