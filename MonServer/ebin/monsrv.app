{application, monsrv,
    [{description, "The Monitor Server"},
     {vsn, "1.0"},
     {modules, [monsrv_app, monsrv_sup, monsrv, 
        mon]},
     {registered, [monsrv, monsrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {monsrv_app, []}},
     {start_phases, []},
     {env, [
        {mem_check_nodes, ['conn1@Leader1', 
            'accsrv@flodis-desktop', 'charsrv@Leader1',
            'start_area@Leader1]}
        ]}
    ]}.

