-record(client_state, {cmds_sent=0, bytes_sent=0, cmds_recv=0, 
    bytes_recv=0, recv_proc, resp_times=[], avg_resp_time=0, pos, 
	time_since_last_cmd, last_vel, last_dir}).
-record(recv_state, {id, bytes_recv=0, cmds_recv=0, resp_times=[], 
	avg_resp_timea=0, debug_output=false}).

