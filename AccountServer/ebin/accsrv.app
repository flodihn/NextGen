{application, accsrv,
    [{description, "The Account Server"},
     {vsn, "1.0"},
     {modules, [accsrv_app, accsrv_sup, accsrv, 
        account]},
     {registered, [accsrv, accsrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {accsrv_app, []}},
     {start_phases, []}
    ]}.

