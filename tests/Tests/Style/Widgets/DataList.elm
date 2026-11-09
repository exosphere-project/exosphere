module Tests.Style.Widgets.DataList exposing (listQuerySuite)

import Dict
import Expect
import Set
import Style.Widgets.DataList as DataList
import Test exposing (Test, describe, test)
import Types.HelperTypes as HelperTypes


filter : String -> DataList.FilterSelectionValue -> DataList.Filter {}
filter id default =
    { id = id
    , label = id
    , chipPrefix = id
    , filterOptions = \_ -> Dict.empty
    , filterTypeAndDefaultValue = default
    , onFilter = \_ _ -> True
    }


defaults : DataList.SelectedFilterOptions
defaults =
    DataList.getDefaultFilterOptions
        [ filter "creator" (DataList.MultiselectOption (Set.singleton "alice"))
        , filter "creationTime" (DataList.UniselectOption DataList.UniselectNoChoice)
        , filter "status" (DataList.MultiselectOption Set.empty)
        ]


throughModel : HelperTypes.ListQuery -> HelperTypes.ListQuery
throughModel listQuery =
    DataList.toQuery defaults (DataList.initWithQuery defaults listQuery)


listQuerySuite : Test
listQuerySuite =
    describe "a data list's search and filters as a URL query"
        [ test "an empty query leaves every filter at its default, and reads back empty" <|
            \_ ->
                Expect.equal { search = "", filters = Dict.empty }
                    (throughModel { search = "", filters = Dict.empty })
        , describe "a query survives the list model unchanged"
            (List.map
                (\( description, listQuery ) ->
                    test description <|
                        \_ -> Expect.equal listQuery (throughModel listQuery)
                )
                [ ( "search text", { search = "web 1", filters = Dict.empty } )
                , ( "a default filter emptied", { search = "", filters = Dict.singleton "creator" [ "" ] } )
                , ( "a default filter changed", { search = "", filters = Dict.singleton "creator" [ "alice", "bob" ] } )
                , ( "several multiselect values", { search = "", filters = Dict.singleton "status" [ "Active", "Shelved" ] } )
                , ( "a uniselect choice", { search = "", filters = Dict.singleton "creationTime" [ "past day" ] } )
                , ( "everything at once"
                  , { search = "db"
                    , filters = Dict.fromList [ ( "creator", [ "" ] ), ( "creationTime", [ "past day" ] ), ( "status", [ "Active" ] ) ]
                    }
                  )
                ]
            )
        , describe "a query is read leniently and written back canonically"
            (List.map
                (\( description, listQuery, expected ) ->
                    test description <|
                        \_ -> Expect.equal expected (throughModel listQuery)
                )
                [ ( "a filter set to its default is left out"
                  , { search = "", filters = Dict.singleton "creator" [ "alice" ] }
                  , { search = "", filters = Dict.empty }
                  )
                , ( "a key naming no filter is dropped"
                  , { search = "", filters = Dict.singleton "colour" [ "blue" ] }
                  , { search = "", filters = Dict.empty }
                  )
                , ( "multiselect values are sorted and deduplicated"
                  , { search = "", filters = Dict.singleton "status" [ "Shelved", "Active", "Shelved" ] }
                  , { search = "", filters = Dict.singleton "status" [ "Active", "Shelved" ] }
                  )
                , ( "an empty value among others is ignored"
                  , { search = "", filters = Dict.singleton "status" [ "", "Active" ] }
                  , { search = "", filters = Dict.singleton "status" [ "Active" ] }
                  )
                , ( "an emptied uniselect equals its default and is left out"
                  , { search = "", filters = Dict.singleton "creationTime" [ "" ] }
                  , { search = "", filters = Dict.empty }
                  )
                , ( "a uniselect keeps its first value"
                  , { search = "", filters = Dict.singleton "creationTime" [ "past day", "past week" ] }
                  , { search = "", filters = Dict.singleton "creationTime" [ "past day" ] }
                  )
                ]
            )
        ]
