-module(btd_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([init_per_suite/1, end_per_suite/1]).
-export([all/0, btd_insert_test/1, btd_remove_test/1, find_key_test/1]).

-export([btd_check_associativity/1, btd_neutral_check/1]).
all() -> [btd_check_associativity, btd_neutral_check, btd_insert_test, btd_remove_test, find_key_test].

init_per_suite(Config) ->
    T1 = util:create_tree_from_list([{10, 10}, {5, 5}, {6, 6}, {3, 3}, {4, 4}, {2, 2}, {1, 1}]),
    T2 = util:create_tree_from_list([
        {41, 253}, {51, 4}, {71, 82}, {54, 856}, {91, 436}, {216, 24}, {118, 813}
    ]),
    T3 = util:create_tree_from_list([
        {477, 923}, {58351, 35684}, {53871, 86982}, {1854, 8674956}, {6891, 43936}, {21936, 2184}, {10618, 81713}
    ]),
    [{tree1, T1}, {tree2, T2}, {tree3, T3} | Config].

end_per_suite(_Config) ->
    ok.

btd_insert_test(_Config) ->
    T1 = balance_tree:insert_and_balance(10, 10, lab2:empty()),
    ?assertEqual({node, 10, 10, {node, 'nil'}, {node, 'nil'}}, T1),
    T2 = balance_tree:insert_and_balance(5, 5, T1),
    ?assertEqual({node, 10, 10, {node, 5, 5, {node, 'nil'}, {node, 'nil'}}, {node, 'nil'}}, T2),
    T3 = balance_tree:insert_and_balance(6, 6, T2),
    ?assertEqual(
        {node, 5, 5, {node, 'nil'},
            {node, 10, 10, {node, 6, 6, {node, 'nil'}, {node, 'nil'}}, {node, 'nil'}}},
        T3
    ),
    T4 = balance_tree:insert_and_balance(3, 3, T3),
    ?assertEqual(
        {node, 5, 5, {node, 3, 3, {node, 'nil'}, {node, 'nil'}},
            {node, 10, 10, {node, 6, 6, {node, 'nil'}, {node, 'nil'}}, {node, 'nil'}}},
        T4
    ).

btd_remove_test(Config) ->
    case lists:keyfind(tree1, 1, Config) of
        {tree1, Tree} ->
            T1 = lab2:remove(3, Tree),
            ?assertEqual(
                {node, 5, 5,
                    {node, 2, 2, {node, 1, 1, {node, 'nil'}, {node, 'nil'}},
                        {node, 4, 4, {node, 'nil'}, {node, 'nil'}}},
                    {node, 10, 10, {node, 6, 6, {node, 'nil'}, {node, 'nil'}}, {node, 'nil'}}},
                T1
            );
        _ ->
            ?assertThrow(badmatch, "Incorrect matching")
    end.

find_key_test(Config) ->
    case lists:keyfind(tree1, 1, Config) of
        {tree1, Tree} ->
            ?assertEqual({ok, 5}, lab2:find(5, Tree)),
            ?assertEqual({ok, 6}, lab2:find(6, Tree)),
            ?assertEqual(undefined, lab2:find(11, Tree));
        _ ->
            ?assertThrow(badmatch, "Incorrect matching")
    end.

btd_neutral_check(_) ->
    btd_neutral_check_rec(100).

btd_neutral_check_rec(0) ->
    ok;
btd_neutral_check_rec(N) ->
    Tree = generate_random_tree(),
    ?assert(lab2:is_equal_trees(Tree, lab2:merge_trees(Tree, lab2:empty()))),
    ?assert(lab2:is_equal_trees(Tree, lab2:merge_trees(lab2:empty(), Tree))),
    btd_neutral_check_rec(N - 1).

% Тест на ассоциативность объединения деревьев
btd_check_associativity(_) ->
    btd_check_associativity_rec(100).

btd_check_associativity_rec(0) ->
    ok;
btd_check_associativity_rec(Count) ->
    Tree1 = generate_random_tree(),
    Tree2 = generate_random_tree(),
    Tree3 = generate_random_tree(),

    Result1 = lab2:merge_trees(lab2:merge_trees(Tree1, Tree2), Tree3),
    Result2 = lab2:merge_trees(Tree1, lab2:merge_trees(Tree2, Tree3)),

    ?assert(lab2:is_equal_trees(Result1, Result2)),
    % Рекурсивный вызов для следующего теста
    btd_check_associativity_rec(Count - 1).

% Функция для генерации случайного дерева
generate_random_tree() ->
    RandomList = generate_random_list(),
    util:create_tree_from_list(RandomList).

% Функция для генерации случайного списка ключ-значение
generate_random_list() ->
    lists:map(fun(_) -> {rand:uniform(1000), random_value()} end, lists:seq(1, rand:uniform(20))).

% Генерация случайного значения
random_value() ->
    case rand:uniform(3) of
        1 -> rand:uniform(1000);
        2 -> lists:seq(1, rand:uniform(10));
        3 -> integer_to_list(rand:uniform(1000))
    end.
