% Binary tree example.
-module(tree).

-export([empty/0,insert/3,lookup/2,has_value/2,t_has_value/2]).

% A node in a binary can be representated as
%   {node, {Key, Val, Smaller, Larger}}
% where smaller and larger are also nodes.
%
% An empty tree is represented as a nil node
%   {node, 'nil'}
% It has no key, no value and no leafs.
%
% When inserting a new value as leaf, we
% create its leaf nodes as nil nodes.
%
% Keys is what we compare on (to retrieve values),
% we're building a key-value store.
%
% Overwrite the value of a key when we want
% to insert a new value for that key.

% The key is to already create leaf nodes when
% a node is created, not when its needed.


% Creates a new tree...
empty() ->
    {node, 'nil'}.

% Insert(Key, Value, Tree)
%
% Base case: when on an empty tree or we arrive
% at a nil node (a leaf without key/value), replace it
% with our key/value and add nill Smaller/Larger nodes.
insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
% When our key is smaller then the given key, traverse into the
% Smaller node...
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
% When our key is bigger then the given key, traverse into the Bigger node...
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
% When the keys are the same, replace the value...t
insert(Key, NewVal, {node, {Key, _, Smaller, Larger}}) ->
    {node, {Key, NewVal, Smaller, Larger}}.

% Given a key, finds the corresponding value
lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(SearchKey, {node, {Key, _, Smaller, _}}) when SearchKey < Key ->
    lookup(SearchKey, Smaller);
lookup(SearchKey, {node, {Key, _, _, Larger}}) when SearchKey > Key ->
    lookup(SearchKey, Larger).

% Checks if a value is present in the tree, ignoring
% the keys.
%
% We go deeper and deeper to find a result, if find,
% it must "bubble up" the tree a branch at a time.
has_value(_, {node, 'nil'}) -> false;
has_value(Val, {node, {_, Val, _, _}}) -> true;
has_value(Val, {node, {_, _, Left, Right}}) ->
    case has_value(Val, Left) of
        true -> true;
        false -> has_value(Val, Right)
    end.

% Instead of letting a true value "bubble up", we
% immediately jump out of the flow when we find a value.
%
% Because a true value will stop the traversal immediately,
% we no longer need to check the return values in has_value1.
t_has_value(Val, Tree) ->
    try has_value1(Val, Tree) of
        false -> false
    catch
        true -> true
    end.
has_value1(_, {node, 'nil'}) -> false;
has_value1(Val, {node, {_, Val, _, _}}) -> throw(true);
has_value1(Val, {node, {_, _, Left, Right}}) ->
    has_value1(Val, Left),
    has_value(Val, Right).
