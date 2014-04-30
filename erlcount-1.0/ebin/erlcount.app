{application, erlcount,
    [{vsn, "1.0.0"},
     {modules, [erlcount,erlcount_counter,erlcount_dispatch,erlcount_lib,erlcount_sup]},
     {applications, [ppool]}
     {registered, [erlcount]},
     {mod, {erlcount, []}},
     {env,
        [{directory, '.'},
         {regex,["if\\s.+->", "case\\s.+\\sof"]},
         {max_files, 10}]}
]}.
