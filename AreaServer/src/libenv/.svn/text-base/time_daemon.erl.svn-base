%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Christian Flodihn
%% @doc
%% The time daemon keeps track of the area day/night cycles, days, months
%% and year. The time daemon is started by the supervisor libenv.sup.
%% @end
%%----------------------------------------------------------------------
-module(libenv.time_daemon).
-import(error_logger).
-import(mnesia).

-import(libstd).

-export([
    init/0,
    time_init/0,
    daemon/1,
    set_skybox/2,
    set_time_of_day_duration/2,
    get_time_of_day_duration/1,
    get_skybox/0,
    get_skybox/1,
    set_ambient_light/2,
    get_ambient_light/1,
    get_ambient_light/0
    %add_day/1,
    %get_days/0
    ]).

-include("env_data.hrl").

%%----------------------------------------------------------------------
%% @private
%% @spec init() -> {ok, Pid} | {error, already_started}
%% @doc
%% This function starts the time daemon process.
%% @end
%%----------------------------------------------------------------------
init() ->
    case whereis(?MODULE) of
        undefined ->
            spawn_link(?MODULE, time_init, []);
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    {error, already_started};
                false ->
                    unregister(?MODULE),
                    spawn_link(?MODULE, time_init, [])
            end
    end.

%%----------------------------------------------------------------------
%% @private
%% @spec time_init() -> ok
%% @doc
%% This is a helper function for init/0.
%% @end
%%----------------------------------------------------------------------
time_init() ->
    case get_time_of_day() of
        no_time_of_day ->
            set_time_of_day(morning);
        {error, Reason}->
            error_logger:info_report([{warning, Reason}])
    end,
    ?MODULE:daemon(now()).

%%----------------------------------------------------------------------
%% @private
%% @spec daemon(Time) -> ok
%% where
%%      Time = tuple()
%% @doc
%% This function runs in an never ending loop, it updates every other
%% second, changing time of day accoringly. The Time is a tuple returned
%% from erlang:now().
%% @end
%%----------------------------------------------------------------------
daemon(Time) ->
    receive 
        Anything ->
            error_logger:info_report([{?MODULE, Anything}]),
            ?MODULE:daemon(Time)
    after 2000 ->
        case get_time_of_day() of
            {time_of_day, TimeOfDay} ->
                calc_next_time_of_day(Time, TimeOfDay),
                ?MODULE:daemon(Time);
            {error, _Reason} ->
                ?MODULE:daemon(Time)
        end
    end.

%%----------------------------------------------------------------------
%% @private
%% @spec calc_next_time_of_day(Time, TimeOfDay) -> ok
%% where
%%      Time = tuple(),
%%      TimeOfDay = atom()
%% @doc
%% Calculates next time of day will happen. If true this function will
%% call next_time_of_day/1.
%% @end
%%----------------------------------------------------------------------
calc_next_time_of_day(Time, TimeOfDay) ->
    MicroDiff = timer:now_diff(Time, now()),
    MinuteDiff = round(abs(MicroDiff/1000000)/60),
    {time_duration, TimeOfDayDuration} = get_time_of_day_duration(
        TimeOfDay),
    case MinuteDiff >= TimeOfDayDuration of
        true ->
            next_time_of_day(TimeOfDay);
        false ->
            pass
    end.

%%----------------------------------------------------------------------
%% @private
%% @spec next_time_of_day(TimeOfDay) -> ok
%% where
%%      Time = tuple(),
%%      TimeOfDay = atom()
%% @doc
%% This function changes the time of day.
%% @end
%%----------------------------------------------------------------------
next_time_of_day(morning) ->
    %SkyBox = get_skybox(noon),
    libstd.srv:area_event({new_time_of_day, noon}),
    %error_logger:info_report([{time_of_day_change, noon, SkyBox}]),
    set_time_of_day(noon);

next_time_of_day(noon) ->
    %SkyBox = get_skybox(afternoon),
    libstd.srv:area_event({new_time_of_day, afternoon}),
    %error_logger:info_report([{time_of_day_change, afternoon, SkyBox}]),
    set_time_of_day(afternoon);

next_time_of_day(afternoon) ->
    %SkyBox = get_skybox(evening),
    libstd.srv:area_event({new_time_of_day, evening}),
    %error_logger:info_report([{time_of_day_change, evening, SkyBox}]),
    set_time_of_day(evening);

next_time_of_day(evening) ->
    %SkyBox = get_skybox(night),
    libstd.srv:area_event({new_time_of_day, night}),
    %error_logger:info_report([{time_of_day_change, night, SkyBox}]),
    set_time_of_day(night);

next_time_of_day(night) ->
    %SkyBox = get_skybox(morning),
    libstd.srv:area_event({new_time_of_day, morning}),
    %error_logger:info_report([{time_of_day_change, morning, SkyBox}]),
    set_time_of_day(morning).

%%----------------------------------------------------------------------
%% @spec set_time_of_day_duration(TimeOfDay, Minutes) -> ok
%% where
%%      TimeOfDay = atom(),
%%      Time = int()
%% @doc
%% Sets how long a time of day will last in minutes.
%% @end
%%----------------------------------------------------------------------
set_time_of_day_duration(TimeOfDay, Minutes) ->
     F = fun() ->
        mnesia:write(#env_data{key=
        list_to_atom(atom_to_list(TimeOfDay) ++ "_time"), 
        value=Minutes})
    end,
    mnesia:transaction(F),
    ok.

%%----------------------------------------------------------------------
%% @spec set_skybox(TimeOfDay, Skybox) -> ok
%% where
%%      TimeOfDay = atom(),
%%      Skybox = int()
%% @doc
%% Binds a skybox to a time of day.
%% @end
%%----------------------------------------------------------------------
set_skybox(TimeOfDay, Skybox) ->
     F = fun() ->
        mnesia:write(#env_data{key=
            list_to_atom(atom_to_list(TimeOfDay) ++ "_skybox"), 
                value=Skybox})
     end,
     mnesia:transaction(F).

%%----------------------------------------------------------------------
%% @spec set_time_of_day(TimeOfDay) -> ok
%% where
%%      TimeOfDay = atom()
%% @doc
%% Creates a time of day.
%% @end
%%----------------------------------------------------------------------
set_time_of_day(TimeOfDay) ->
     F = fun() ->
        mnesia:write(#env_data{key=time_of_day, value=TimeOfDay})
     end,
     mnesia:transaction(F).

%%----------------------------------------------------------------------
%% @spec get_skybox() -> {ok, Skybox}
%% where
%%      Skybox = string()
%% @doc
%% Returns the skybox depending on the current time of day.
%% @end
%%----------------------------------------------------------------------
get_skybox() ->
    case get_time_of_day() of
        {time_of_day, TimeOfDay} -> 
            get_skybox(TimeOfDay);
        {error, no_time_of_day} ->
            {skybox, "Examples/MorningSkyBox"}
    end.

%%----------------------------------------------------------------------
%% @spec get_skybox(TimeOfDay) -> {ok, Skybox}
%% where
%%      TimeOfDay = atom(),
%%      Skybox = string()
%% @doc
%% Returns the skybox for a specific time of day.
%% @end
%%----------------------------------------------------------------------
get_skybox(TimeOfDay) ->
    F = fun() ->
        case mnesia:match_object(#env_data{key=
            list_to_atom(atom_to_list(TimeOfDay) ++  "_skybox"), 
            value='$1'}) of
            [Rec] ->
                {skybox, Rec#env_data.value};
            [] ->
               {skybox, "Examples/MorningSkyBox"}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        {aborted, _Reason} ->
            {skybox, "Examples/MorningSkyBox"}
    end.

%%----------------------------------------------------------------------
%% @spec get_time_of_day() -> {ok, TimeOfDay} | {error, no_time_of_day}
%% where
%%      TimeOfDay = atom()
%% @doc
%% Returns the time of day.
%% @end
%%----------------------------------------------------------------------
get_time_of_day() ->
    F = fun() ->
        case mnesia:match_object(#env_data{key=time_of_day, value='$1'}) of
            [Rec] ->
                {time_of_day, Rec#env_data.value};
            [] ->
               {error, no_time_of_day}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%----------------------------------------------------------------------
%% @spec get_time_of_day_duration(TimeOfDay) -> {time_duration, Minutes}
%% where
%%      TimeOfDay = atom(),
%%      Minutes = int()
%% @doc
%% Returns the duration of a time of day in minutes.
%% @end
%%----------------------------------------------------------------------
get_time_of_day_duration(TimeOfDay) ->
    F = fun() ->
        case mnesia:match_object(#env_data{key=
            list_to_atom(atom_to_list(TimeOfDay) ++ "_time"), 
            value='$1'}) of
            [Rec] ->
                {time_duration, Rec#env_data.value};
            [] ->
               {time_duration, 1}
        end
    end,
    {atomic, Result} = mnesia:transaction(F),
    Result.

%%----------------------------------------------------------------------
%% @spec set_ambient_light(TimeOfDay, Value) -> ok
%% where
%%      TimeOfDay = atom(),
%%      Value = float()
%% @doc
%% Sets the ambient light for a time of day, 0.0 is total darkness and 1.0
%% is full daylight.
%% @end
%%----------------------------------------------------------------------
set_ambient_light(TimeOfDay, Value) ->
    F = fun() ->
        mnesia:write(#env_data{key=
            list_to_atom(atom_to_list(TimeOfDay) ++ "_light"), 
            value=Value})
    end,
    mnesia:transaction(F),
    ok.

%%----------------------------------------------------------------------
%% @spec get_ambient_light() -> {ambient_light, Value}
%% where
%%      Value = float()
%% @doc
%% Returns the ambient light for the current time of day.
%% @end
%%----------------------------------------------------------------------
get_ambient_light() ->
    case get_time_of_day() of
        {time_of_day, TimeOfDay} ->
            get_ambient_light(TimeOfDay);
        {error, no_time_of_day} ->
            {ambient_light, 1.0}
    end.

%%----------------------------------------------------------------------
%% @spec get_ambient_light(TimeOfDay) -> {ambient_light, Value}
%% where
%%      TimeOfDay = atom(),
%%      Value = float()
%% @doc
%% Returns the ambient light for a specific time of day.
%% @end
%%----------------------------------------------------------------------
get_ambient_light(TimeOfDay) ->
    F = fun() ->
        case mnesia:match_object(#env_data{key=
            list_to_atom(atom_to_list(TimeOfDay) ++ "_light"), 
            value='$1'}) of
            [Rec] ->
                {ambient_light, Rec#env_data.value};
            [] ->
               {ambient_light, 1}
        end
    end,
    {atomic, Result} = mnesia:transaction(F),
    Result.
 
%add_day(Day) ->
%    {days, OldList} = get_day_list(),
%    set_day_list(lists:append(OldList, [Day])).

%get_days() ->
%    get_day_list().

%get_day_list() ->
%    F = fun() ->
%        case mnesia:match_object(#env_data{key=days, value='$1'}) of
%            [Rec] ->
%                {days, Rec#env_data.value};
%            [] ->
%                {days, []}
%        end
%    end,
%    {atomic, Result} = mnesia:transaction(F),
%    Result.

%set_day_list(List) ->
%    F = fun() ->
%        mnesia:write(#env_data{key=days, value=List})
%    end,
%    mnesia:transaction(F).

%set_current_day(Day) ->
%    F = fun() ->
%        mnesia:write(#env_data{key=current_day, value=Day})
%    end,
%    mnesia:transaction(F).

%get_current_day() ->
%     F = fun() ->
%        case mnesia:match_object(#env_data{key=current_day, value='$1'}) of
%            [Rec] ->
%                {current_day, Rec#env_data.value};
%            [] ->
%                {current_day, none}
%
%        end
%    end,
%    {atomic, Result} = mnesia:transaction(F),
%    Result.

%set_next_day() ->
%    {current_day, Day} = get_current_day(),
%    case Day of 
%        none ->
%            {error, no_current_day};
%        _Day ->
%            %{next_day, NextDay} = get_next_day(),
%            %set_current_day(NextDay)
%            not_implemented_yet
%    end.


