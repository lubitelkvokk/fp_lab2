%% lab2.hrl

-type tree() ::
    {node, 'nil'} | {node, Key :: integer(), Value :: any(), Left :: tree(), Right :: tree()}.
