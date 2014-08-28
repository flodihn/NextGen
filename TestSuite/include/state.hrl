-record(client_state, {cmds_sent=0, bytes_sent=0, cmds_recv=0, 
    bytes_recv=0, recv_proc}).
-record(recv_state, {id, bytes_recv=0, cmds_recv=0, resp_times=[]}).

