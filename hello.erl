-module(hello).
-export([hello/0]).

hello() ->
	io:fwrite("Hello world!\n", []).