% Specific logic, allows the other modules to be concerned
% about when to use what func.

-module(erlcount_lib).
-export([find_erl/1,regex_count/2]).

% We include a header file which contains the record #file_info{} so we can access
% stats about a read file.
-include_lib("kernel/include/file.hrl").

% Finds the .erl files recursively.
%
% Reads all the files in a directory and queues them, takes the first one out
% and processes it. If its a directory, reads all the contents to queue it,
% if its an .erl file schedules it to be processed (wordcounted), anything else
% will just make the next item from the queue to be worked upon.

find_erl(FileName, Queue) ->
    {ok, F=#file_info{}} = file:read_file_info(FileName),
    case F#file_info.type of
        directory -> handle_directory(FileName, Queue);
        regular -> handle_regular_file(FileName, Queue);
        _Other -> dequeue_and_run(Queue)
    end.


% Put all its content in the queue and dequeues the queue (start processing a
% file in the queue).
%
handle_directory(DirName, Queue) ->
    case file:list_dir(DirName) of
        {ok, []} ->
            % Nothing to queue, start processing a file...
            dequeue_and_run(Queue);
        {ok, Files} ->
            % Queue all the files and start procesing a file...
            dequeue_and_run(enqueue_many(DirName, Files, Queue))
    end.

% Checks whether file ends with .erl.
%
% We return the name to the caller, and a function which it can call to continue
% searching for files (CFS, Continuation Passing Style - but not like the callbacks
% in node).
%
handle_regular_file(FileName, Queue) ->
    case filename:extension(FileName) of
        ".erl" ->
            {continue, FileName, fun() -> dequeue_and_run(Queue) end};
        _NonElr ->
            dequeue_and_run(Queue)
    end.


% Pops an item from the queue and examines it.
%
dequeue_and_run(Queue) ->
    case queue:out(Queue) of
        {empty, _} -> done;
        {{value, File}, NewQueue} -> find_erl(File, NewQueue)
    end.

% Adds files to the queue.
%
enqueue_many(DirName, Files, Queue) ->
    F = fun(FileName, Q) -> queue:in(filename:join(DirName, FileName), Q) end,
    lists:foldl(F, Queue, Files).

find_erl(DirName) ->
    find_erl(DirName, queue:new()).

% Counts occurences of regex matches.
%
regex_count(Re, Str) ->
    case re:run(Str, Re, [global]) of
        nomatch -> 0;
        {match, List} -> length(List)
    end.
