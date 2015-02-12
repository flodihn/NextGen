%% @type dict(). A dictionary from the dict module. 
%% @type obj(). #obj{id = string, type = atom(),  properties = dict(),
%% last_tick = int(), tick_interval=int()}
 
-record(obj, {
    id, 
    type, 
    properties, 
    tick_interval=2000}).

