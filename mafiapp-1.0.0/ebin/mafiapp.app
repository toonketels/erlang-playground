{application, mafiapp,
    [{description, "Help the boss keep track of its friends"},
     {vsn, "1.0.0"},
     {modules, [mafiapp, mafiapp_sup]},
     {applications, [kernel, stdlib, mnesia]}]}.
