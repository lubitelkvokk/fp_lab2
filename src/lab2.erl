-module(lab2).

-export([
    insert/3, empty/0, remove/2, find/2, balance_tree/1, insert_and_balance/3
]).
-export([merge_trees/2]).
-export([is_equal_trees/2]).


-type tree() :: {node, 'nil'} | {node, Key::integer(), Value::any(), Left::tree(), Right::tree()}.
-spec insert(Key::integer(), Value::any(), Tree::tree()) -> Tree::tree().
-spec remove(Key::integer(), Tree::tree()) -> Tree::tree().
-spec find(Key::integer(), Tree::tree()) -> Value::any().
-spec balance_tree(Tree::tree()) -> Tree::tree().
-spec insert_and_balance(Key::integer(), Value::any(), Tree::tree()) -> Tree::tree().
-spec merge_trees(Tree1::tree(), Tree2::tree()) -> Tree::tree().
-spec is_equal_trees(Tree1::tree(), Tree2::tree()) -> boolean().

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


find_max_depth(Tree) ->
    find_max_depth(Tree, 0).

find_max_depth({node, 'nil'}, Depth) ->
    Depth;
find_max_depth({node, _, _, LeftNode, RightNode}, Depth) ->
    LeftDepth = find_max_depth(LeftNode, Depth + 1),
    RightDepth = find_max_depth(RightNode, Depth + 1),
    max(LeftDepth, RightDepth).


balance_tree({node, 'nil'}) ->
    empty();
balance_tree({node, Key, Value, LeftNode, RightNode}) ->
    Diff = find_max_depth(LeftNode) - find_max_depth(RightNode),
    case Diff of
        Depth when Depth > 1 ->
            {_, LeftKey, LeftValue, LeftDLNode, RightDLNode} = LeftNode,
            NewRightNode = {node, Key, Value, RightDLNode, RightNode},
            {node, LeftKey, LeftValue, balance_tree(LeftDLNode), balance_tree(NewRightNode)};
        Depth when Depth < -1 ->
            {_, RightKey, RightValue, LeftDRNode, RightDRNode} = RightNode,
            NewLeftNode = {node, Key, Value, LeftNode, LeftDRNode},
            {node, RightKey, RightValue, balance_tree(NewLeftNode), RightDRNode};
        _ ->
            {node, Key, Value, LeftNode, RightNode}
    end.


insert_and_balance(Key, Value, Tree) ->
    T = insert(Key, Value, Tree),
    balance_tree(T).


tree_to_list(Tree) ->
    tree_to_list(Tree, []).

tree_to_list({node, 'nil'}, Acc) ->
    Acc;
tree_to_list({node, Key, Value, LeftNode, RightNode}, Acc) ->
    tree_to_list(LeftNode, [{Key, Value} | tree_to_list(RightNode, Acc)]).

merge_lists(List1, List2) ->
    merge_lists(List1, List2, []).

merge_lists([], [], Acc) -> lists:reverse(Acc);
merge_lists([H1 | T1], [], Acc) -> merge_lists(T1, [], [H1 | Acc]);
merge_lists([], [H2 | T2], Acc) -> merge_lists([], T2, [H2 | Acc]);
merge_lists([{Key, Value1} | T1], [{Key, _} | T2], Acc) ->
    merge_lists(T1, T2, [{Key, Value1} | Acc]);
merge_lists([H1 | T1], [H2 | T2], Acc) ->
    {Key1, _} = H1,
    {Key2, _} = H2,
    case Key2 > Key1 of
        true -> merge_lists(T1, [H2 | T2], [H1 | Acc]);
        false -> merge_lists([H1 | T1], T2, [H2 | Acc])
    end.

create_tree_from_list(List) ->
    create_tree_from_list(List, empty()).

create_tree_from_list([], Acc) -> Acc;
create_tree_from_list([H | T], Acc) ->
    {Key, Value} = H,
    create_tree_from_list(T, insert_and_balance(Key, Value, Acc)).
    
merge_trees(Tree1, Tree2) ->
    TL1 = tree_to_list(Tree1),
    TL2 = tree_to_list(Tree2),
    List = merge_lists(TL1, TL2),
    create_tree_from_list(List).

is_equal_trees(Tree1, Tree2) ->
    TL1 = tree_to_list(Tree1),
    TL2 = tree_to_list(Tree2),
    TL1 == TL2.