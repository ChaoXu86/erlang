-module(parking_lot_db).


%% implement parking lot db
-export([init/1,
         db_loop/0]).

-export([query_vacancy/1,
         check_in/1,
         check_out/1]).

-include("logging.hrl").

-define(db_proc, db_proc).

-record(park_slot,
        {id,            %% id
         timestamp,     %% when the parking_slot is used
         state          %% vacancy | occupied
        }).

%% init db process
init([DBName, Capacity]) ->
    %% create db process
    case whereis(?db_proc) of
        undefined ->
            log("create new db~n");
        Pid ->
            log("db process exists, recreating db~n"),
            Pid ! {exit, restart},
            timer:sleep(1)
    end,

    DBPid = spawn(?MODULE,db_loop,[]),
    register(?db_proc, DBPid),

    create_db([DBName, Capacity]).

%% query how much park lot available
query_vacancy([DBName])->
    case ets:lookup(DBName, vacancy) of
        [{vacancy, Vacancy}] ->
            Vacancy;
        _ ->
            %% something wrong with db
            log("vacancy is n/a, something wrong in DB~n"),
            0
    end.

%% check_in, Park slot id is returned
check_in([DBName,_CarType]) ->
    %% TODO sanity check for vacancy
    [{parkslots, ParkSlots}] = ets:lookup(DBName, parkslots),
    
    %% find the first vancancy slot, must exist
    #park_slot{id = VancancyId} 
    = ParkSlot = lists:keyfind(vancancy, #park_slot.state,ParkSlots),

    %% update park slot state and total number of vancancies
    NewParkSlot = ParkSlot#park_slot{timestamp = erlang:system_time(seconds),
                                     state = occupied },
    NewParkSlots = lists:keyreplace(VancancyId, #park_slot.id, ParkSlots, NewParkSlot),

    ets:insert(DBName, {parkslots, NewParkSlots}),
    ets:update_counter(DBName, vacancy, {2,-1}),

    VancancyId.

%% check_out, parking duration is returned
check_out([DBName,ParkSlotId]) ->
    [{parkslots, ParkSlots}] = ets:lookup(DBName, parkslots),

    ParkSlot = lists:keyfind(ParkSlotId, #park_slot.id, ParkSlots),
    Duration = erlang:system_time(seconds) - ParkSlot#park_slot.timestamp,

    %% update park slot state and total number of vancancies
    NewParkSlots = lists:keyreplace(ParkSlotId, #park_slot.id, ParkSlots, #park_slot{id=ParkSlotId,
                                                                                     state=vancancy}),

    ets:insert(DBName, {parkslots, NewParkSlots}),
    ets:update_counter(DBName, vacancy, {2,1}),

    Duration.

%%
%% create db
create_db([DBName, Capacity]) ->
    ?db_proc ! {create, DBName, Capacity}.

%% DB process main logic
db_loop() ->
    receive
        {exit, Reason} ->
            log("db process ~p exit, reason=~p~n", [self(), Reason]);
        {create, DBName, Capacity} ->
            log("create db ~p~n", [DBName]),
            ets:new(DBName, [named_table,set,public]),
            ets:insert(DBName,{capacity, Capacity}),
            ets:insert(DBName,{vacancy, Capacity}),
            %% make all park slots vancancy
            ParkSlots = [#park_slot{id = Id,state=vancancy}|| Id<-lists:seq(1,Capacity)],
            ets:insert(DBName,{parkslots, ParkSlots}),
            db_loop();
        Msg ->
            log("Unsupported received ~p~n",[Msg]),
            db_loop()
    end.



