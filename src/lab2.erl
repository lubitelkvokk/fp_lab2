-module(lab2).

-export([insert/3, empty/0, remove/2, find/2, find_max_depth/1, balance_tree/1, insert_and_balance/3]).

empty() -> {node, "nil"}.

insert(Key, Value, {node, "nil"}) ->
    {node, Key, Value, empty(), empty()};
insert(NewKey, NewValue, {node, Key, Value, LeftNode, RightNode}) when
    NewKey < Key
->
    {node, Key, Value, insert(NewKey, NewValue, LeftNode), RightNode};
insert(NewKey, NewValue, {node, Key, Value, LeftNode, RightNode}) when
    NewKey > Key
->
    {node, Key, Value, LeftNode, insert(NewKey, NewValue, RightNode)};
% когда ключи совпадают - меняем только Value узла
insert(Key, NewValue, {node, Key, _, LeftNode, RightNode}) ->
    {node, Key, NewValue, LeftNode, RightNode}.

remove(_, {node, "nil"}) ->
    empty();
remove(SearchKey, {node, Key, Value, LeftNode, RightNode}) when SearchKey < Key ->
    {node, Key, Value, remove(SearchKey, LeftNode), RightNode};
remove(SearchKey, {node, Key, Value, LeftNode, RightNode}) when SearchKey > Key ->
    {node, Key, Value, LeftNode, remove(SearchKey, RightNode)};
remove(SearchKey, {node, SearchKey, _, LeftNode, RightNode}) when
    LeftNode == {node, "nil"}
->
    RightNode;
remove(SearchKey, {node, SearchKey, _, LeftNode, RightNode}) ->
    {node, DKey, DValue, DRightNode, DLeftNode} = LeftNode,
    {node, DKey, DValue, remove(DKey, {node, DKey, DValue, DRightNode, DLeftNode}), RightNode}.

find(_, {node, "nil"}) ->
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

find_max_depth({node, "nil"}, Depth) ->
    Depth;
find_max_depth({node, _, _, LeftNode, RightNode}, Depth) ->
    LeftDepth = find_max_depth(LeftNode, Depth + 1),
    RightDepth = find_max_depth(RightNode, Depth + 1),
    max(LeftDepth, RightDepth).

balance_tree({node, "nil"}) ->
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

