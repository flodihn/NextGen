{application, connsrv,
    [{description, "The Connection Server"},
     {vsn, "1.0"},
     {modules, [connsrv_app, connsrv_sup, connsrv, connection]},
     {registered, [connsrv, connsrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {connsrv_app, []}},
     {start_phases, []},
     {env, [
        {accsrv, 'accsrv@Leader1'},
        {start_area, 'start_area@Leader1'},
        {charsrv, 'charsrv@Leader1'}
     ]}
    ]}.

