-module(btd_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([init_per_suite/1, end_per_suite/1]).
-export([all/0, btd_insert_test/1, btd_remove/1]).

all() -> [btd_insert_test, btd_remove].

init_per_suite(Config) ->
    T1 = lab2:insert(10, 10, lab2:empty()),
    T2 = lab2:insert(5, 5, T1),
    T3 = lab2:insert(6, 6, T2),
    T4 = lab2:insert(3, 3, T3),
    T5 = lab2:insert(4, 4, T4),
    T6 = lab2:insert(2, 2, T5),
    T7 = lab2:insert(1, 1, T6),
    [{tree, T7} | Config].

end_per_suite(_Config) ->
    ok.

btd_insert_test(_Config) ->
    T1 = lab2:insert(10, 10, lab2:empty()),
    ?assertEqual({node, 10, 10, {node, "nil"}, {node, "nil"}}, T1),
    T2 = lab2:insert(5, 5, T1),
    ?assertEqual({node, 10, 10, {node, 5, 5, {node, "nil"}, {node, "nil"}}, {node, "nil"}}, T2),
    T3 = lab2:insert(6, 6, T2),
    ?assertEqual(
        {node, 10, 10, {node, 5, 5, {node, "nil"}, {node, 6, 6, {node, "nil"}, {node, "nil"}}},
            {node, "nil"}},
        T3
    ),
    T4 = lab2:insert(3, 3, T3),
    ?assertEqual(
        {node, 10, 10,
            {node, 5, 5, {node, 3, 3, {node, "nil"}, {node, "nil"}},
                {node, 6, 6, {node, "nil"}, {node, "nil"}}},
            {node, "nil"}},
        T4
    ).

btd_remove(Config) ->
    {badmatch, Tree} = lists:keyfind(tree, 1, Config).
    % T1 = lab2:remove(3, Tree),
    % ?assertEqual(
    %     {node, 10, 10,
    %         {node, 5, 5,
    %             {node, 2, 2, {node, 1, 1, {node, "nil"}, {node, "nil"}},
    %                 {node, 4, 4, {node, "nil"}, {node, "nil"}}},
    %             {node, 6, 6, {node, "nil"}, {node, "nil"}}},
    %         {node, "nil"}},
    %     T1
    % ).
