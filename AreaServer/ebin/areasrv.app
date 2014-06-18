{application, areasrv,
    [{description, "The Area Server"},
     {vsn, "1.0"},
     {modules, [areasrv_app, areasrv_sup, areasrv, area]},
     {registered, [areasrv, areasrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {areasrv_app, []}},
     {start_phases, []},
     {env, [
        {monsrv, ['monsrv@christian']},
        {libs_auto_load, [libstd_sup, libplayer_sup, libenv_sup,
            libtree_sup, libid_sup, libsave_sup, libdist_sup, libtest_sup]}
     ]}
    ]}.

