%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright Christian Flodihn
%% @doc
%% @end
%%----------------------------------------------------------------------

%%@docfile "doc/id.edoc"

-module(observer_logger).

-include("char.hrl").
-include("vec.hrl").

% API
-export([
    init/0
    ]).

% handlers
-export([
	get_loop_procs/0,
	create_area/0,
    log/1,
	view_log/1,
	add_test_entries/0
    ]).

-record(obpos_log, {id, data, time}).

%%----------------------------------------------------------------------
%% @spec init() -> ok
%% @private
%% @doc
%% Initiates the logger.
%% @end
%%----------------------------------------------------------------------
init() ->
    {ok, []}.

create_area() ->
	mnesia:start(),
    mnesia:create_table(obpos_log,
    	[{ram_copies, [node()]},
    	{attributes, record_info(fields, obpos_log)}]),
    ok.

get_loop_procs() ->
	[obpos_log].	

%%----------------------------------------------------------------------
%% @doc
%% @spec log(Data) -> ok
%% where
%%      Data = any() 
%% @end
%%----------------------------------------------------------------------
log({sync_pos, {id, Id}, {log, Data}}) ->
    mnesia:dirty_write(#obpos_log{id=Id, data=Data, time=erlang:now()});

log(Unknown) ->
	error_logger:info_report({ignoring_log_request, Unknown}).


view_log(LogType) ->
	traverse_table_and_show(LogType).

traverse_table_and_show(Table_name)->
    Iterator =  fun(Rec,_)->
    	io:format("~p~n",[Rec]),
		[]
    end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(
				transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.

add_test_entries() ->
	RandVec = #vec{
		x=random:uniform(100),
		y=random:uniform(100),
		z=random:uniform(100)},
	log({sync_pos,
		{id, <<"test@foobar#123456789">>}, {log, RandVec}}).
