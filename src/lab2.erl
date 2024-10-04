-module(lab2).
-include("lab2.hrl").
-export([
    insert/3, empty/0, remove/2, find/2, filter_tree/1
]).
-export([merge_trees/2, is_equal_trees/2]).
-export([filter/2]).
-import(balance_tree, [balance_tree/1, insert_and_balance/3]).
-import(util, [tree_to_list/1, create_tree_from_list/1]).

-spec insert(Key :: integer(), Value :: any(), Tree :: tree()) -> Tree :: tree().
-spec remove(Key :: integer(), Tree :: tree()) -> Tree :: tree().
-spec find(Key :: integer(), Tree :: tree()) -> Value :: any().
-spec merge_trees(Tree1 :: tree(), Tree2 :: tree()) -> Tree :: tree().
-spec is_equal_trees(Tree1 :: tree(), Tree2 :: tree()) -> boolean().

empty() -> {node, 'nil'}.

insert(Key, Value, {node, 'nil'}) ->
    {node, Key, Value, empty(), empty()};
insert(NewKey, NewValue, {node, Key, Value, LeftNode, RightNode}) when
    NewKey < Key
->
    {node, Key, Value, insert(NewKey, NewValue, LeftNode), RightNode};
insert(NewKey, NewValue, {node, Key, Value, LeftNode, RightNode}) when
    NewKey > Key
->
    {node, Key, Value, LeftNode, insert(NewKey, NewValue, RightNode)};
insert(Key, NewValue, {node, Key, _, LeftNode, RightNode}) ->
    {node, Key, NewValue, LeftNode, RightNode}.

remove(_, {node, 'nil'}) ->
    empty();
remove(SearchKey, {node, Key, Value, LeftNode, RightNode}) when SearchKey < Key ->
    {node, Key, Value, remove(SearchKey, LeftNode), RightNode};
remove(SearchKey, {node, Key, Value, LeftNode, RightNode}) when SearchKey > Key ->
    {node, Key, Value, LeftNode, remove(SearchKey, RightNode)};
remove(SearchKey, {node, SearchKey, _, LeftNode, RightNode}) when
    LeftNode == {node, 'nil'}
->
    RightNode;
remove(SearchKey, {node, SearchKey, _, LeftNode, RightNode}) ->
    {node, DKey, DValue, DRightNode, DLeftNode} = LeftNode,
    {node, DKey, DValue, remove(DKey, {node, DKey, DValue, DRightNode, DLeftNode}), RightNode}.

find(_, {node, 'nil'}) ->
    undefined;
find(SearchKey, {node, Key, _, _, RightNode}) when
    SearchKey > Key
->
    find(SearchKey, RightNode);
find(SearchKey, {node, Key, _, LeftNode, _}) when
    SearchKey < Key
->
    find(SearchKey, LeftNode);
find(SearchKey, {node, SearchKey, Value, _, _}) ->
    {ok, Value}.

merge_lists(List1, List2) ->
    merge_lists(List1, List2, []).

merge_lists([], [], Acc) ->
    lists:reverse(Acc);
merge_lists([H1 | T1], [], Acc) ->
    merge_lists(T1, [], [H1 | Acc]);
merge_lists([], [H2 | T2], Acc) ->
    merge_lists([], T2, [H2 | Acc]);
merge_lists([{Key, Value1} | T1], [{Key, _} | T2], Acc) ->
    merge_lists(T1, T2, [{Key, Value1} | Acc]);
merge_lists([H1 | T1], [H2 | T2], Acc) ->
    {Key1, _} = H1,
    {Key2, _} = H2,
    case Key2 > Key1 of
        true -> merge_lists(T1, [H2 | T2], [H1 | Acc]);
        false -> merge_lists([H1 | T1], T2, [H2 | Acc])
    end.

merge_trees(Tree1, Tree2) ->
    TL1 = tree_to_list(Tree1),
    TL2 = tree_to_list(Tree2),
    List = merge_lists(TL1, TL2),
    create_tree_from_list(List).

is_equal_trees(Tree1, Tree2) ->
    TL1 = tree_to_list(Tree1),
    TL2 = tree_to_list(Tree2),
    TL1 == TL2.

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Acc) -> Acc;
    filter(Pred, [H | T], Acc) -> case  Pred(H) of
    true -> filter(Pred, T, [H | Acc]);
    false -> filter(Pred, T, Acc)
end.

filter_tree(Tree) ->
    List = util:tree_to_list(Tree),
    NewList = filter(fun({Key, _}) -> Key > 5 end, List),
    util:create_tree_from_list(NewList).
