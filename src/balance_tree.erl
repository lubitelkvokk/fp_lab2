-module(balance_tree).
-include("lab2.hrl").
-import(lab2, [empty/0, insert/3]).
-export([balance_tree/1, insert_and_balance/3]).

-spec balance_tree(Tree :: tree()) -> Tree :: tree().
-spec insert_and_balance(Key :: integer(), Value :: any(), Tree :: tree()) -> Tree :: tree().

find_max_depth(Tree) ->
    find_max_depth(Tree, 0).

find_max_depth({node, 'nil'}, Depth) ->
    Depth;
find_max_depth({node, _, _, LeftNode, RightNode}, Depth) ->
    LeftDepth = find_max_depth(LeftNode, Depth + 1),
    RightDepth = find_max_depth(RightNode, Depth + 1),
    max(LeftDepth, RightDepth).

balance_tree({node, 'nil'}) ->
    lab2:empty();
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
