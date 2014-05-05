{application, m8ball,
    [{vsn, "1.0.0"},
     {description, "Answers all questions"},
     {modules, [m8ball, m8ball_server, m8ball_sup]},
     {applications, [stdlib, kernel, crypto]},
     {registered, [m8ball, m8ball_server, m8ball_sup]},
     {mod, {m8ball, []}},
     {env, [
        {answers, {<<"Yes">>,
                   <<"No">>,
                   <<"Maybe">>,
                   <<"Could be">>}}]}]}.
