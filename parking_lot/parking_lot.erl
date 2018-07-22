-module(parking_lot).

-export([start/1]).

-export([check_in/1,
         check_out/1]).

-include("logging.hrl").

-define(parking_db, parking_db).

start([Capacity]) ->
    %% init parking db with name and capacity
    parking_lot_db:init([_DBName=?parking_db,
                         Capacity]).
%% ---------------------------------------------
%% check_in([CarType]) ->
%%          Result
%%
%% Description:
%% check whether CarType is allowed to check in
%% If check in success, parking slot is returned
%%
%% Parmeter:
%% CarType - string()
%% Result  - atom():: {reject, no_slot} | {accept, ParkSlot}
%% ---------------------------------------------
check_in([CarType]) ->
    case parking_lot_db:query_vacancy([?parking_db]) of
        0 ->
            {reject, no_slot};
        _ ->
            ParkSlot = parking_lot_db:check_in([?parking_db ,CarType]),
            {accept, ParkSlot}
    end.

%% --------------------------------------------
%% check_out([CarType, ParkSlot]) ->
%%           Fee
%%
%% Description:
%% Check the car, fee is returned
%% 
%% Parameter:
%% ParkSlot - integer()
%% Result   - integer()
%% --------------------------------------------
check_out([CarType, ParkSlot]) ->
    Duration = parking_lot_db:check_out([?parking_db, ParkSlot]),
    %% TODO
    pricing:calculate(CarType, Duration).

