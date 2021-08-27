{application, connsrv,
    [{description, "The Connection Server"},
     {vsn, "1.0"},
     {modules, [connsrv_app, connsrv_sup, connsrv, connection]},
     {registered, [connsrv, connsrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {connsrv_app, []}},
     {start_phases, []},
     {env, [
        {accsrv, 'accsrv@DESKTOP-K00TFKB'},
        {start_area, 'start_area@DESKTOP-K00TFKB'},
        {charsrv, 'charsrv@DESKTOP-K00TFKB'}
     ]}
    ]}.

