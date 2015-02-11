{application, charsrv,
    [{description, "The Character Server"},
     {vsn, "1.0"},
     {modules, [charsrv_app, charsrv_sup, charsrv, 
        char]},
     {registered, [charsrv, charsrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {charsrv_app, []}},
     {start_phases, []},
     {env, [
        {start_area, 'start_area@macbook'}
     ]}
    ]}.

