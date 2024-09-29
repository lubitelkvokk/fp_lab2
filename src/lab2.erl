-module(lab2).

-export([insert/3, empty/0, remove/2, find/2]).

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
