{application, erlcount,
    [{vsn, "1.0.0"},
     {description, "Run regular expressions on erlang source files."},
     {modules, [erlcount,erlcount_counter,erlcount_dispatch,erlcount_lib,erlcount_sup]},
     {applications, [stdlib,kernel,ppool]},
     {registered, [erlcount]},
     {mod, {erlcount, []}},
     {env,
        [{directory, '.'},
         {regex,["if\\s.+->", "case\\s.+\\sof"]},
         {max_files, 10}]}
]}.
