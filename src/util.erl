-module(util).

-export([create_tree_from_list_foldl/1, create_tree_from_list_foldr/1]).
-export([tree_to_list/1, create_tree_from_list/1]).

-import(lab2, [empty/0, insert/3]).
-import(balance_tree, [insert_and_balance/3]).

tree_to_list(Tree) ->
    tree_to_list(Tree, []).

tree_to_list({node, 'nil'}, Acc) ->
    Acc;
tree_to_list({node, Key, Value, LeftNode, RightNode}, Acc) ->
    tree_to_list(LeftNode, [{Key, Value} | tree_to_list(RightNode, Acc)]).

create_tree_from_list(List) ->
    create_tree_from_list(List, lab2:empty()).

create_tree_from_list([], Acc) ->
    Acc;
create_tree_from_list([H | T], Acc) ->
    {Key, Value} = H,
    create_tree_from_list(T, balance_tree:insert_and_balance(Key, Value, Acc)).

% обертка над insert
insert({Key, Value}, Tree) ->
    lab2:insert(Key, Value, Tree).

create_tree_from_list_foldl(List) ->
    lists:foldl(fun insert/2, empty(), List).

create_tree_from_list_foldr(List) ->
    lists:foldr(fun insert/2, empty(), List).
