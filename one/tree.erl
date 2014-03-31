% Binary tree example.
-module(tree).

-export([empty/0,insert/3,lookup/2]).

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

lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
    {ok, Val};
lookup(SearchKey, {node, {Key, _, Smaller, _}}) when SearchKey < Key ->
    lookup(SearchKey, Smaller);
lookup(SearchKey, {node, {Key, _, _, Larger}}) when SearchKey > Key ->
    lookup(SearchKey, Larger).
