{application, connsrv,
    [{description, "The Connection Server"},
     {vsn, "1.0"},
     {modules, [connsrv_app, connsrv_sup, connsrv, connection]},
     {registered, [connsrv, connsrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {connsrv_app, []}},
     {start_phases, []},
     {env, [
        {accsrv, 'accsrv@macbook'},
        {start_area, 'areasrv@macbook'},
        {charsrv, 'charsrv@macbook'}
     ]}
    ]}.

