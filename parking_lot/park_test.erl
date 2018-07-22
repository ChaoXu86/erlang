-module(park_test).

-export([test/2,
         simulate_car/1]).

-include("logging.hrl").

test(NumberOfCar, ParkSlotsCount) ->
    parking_lot:start([ParkSlotsCount]),

    %% Create number of Cars to simulate check out and check in
    [spawn(?MODULE, simulate_car, [CarId])||CarId<-lists:seq(1,NumberOfCar)].


simulate_car(CarId) ->
    %% initial random seed
    timer:sleep(10),
    random:seed(os:timestamp()),

    CarType = case random:uniform(3) of
                  1 ->
                      "BMW";
                  2 ->
                      "VW";
                  3 ->
                      "Other"
              end,

    %% random wait 1~10 seconds before check in
    timer:sleep( rand:uniform(10) * 1000),
    case parking_lot:check_in([CarType]) of
        {reject, _} ->
            log("Car No.~p Type ~p check in failed~n", [CarId, CarType]);
        {accept, SlotId} ->
            log("Car No.~p Type ~p check in park slot ~p~n", [CarId, CarType, SlotId]),
            %% random wait 1~3 seconds and check out
            timer:sleep(random:uniform(5) * 1000),
            Fee = parking_lot:check_out([CarType, SlotId]),
            log("Car No.~p Type ~p check out park slot ~p, Fee ~p~n", [CarId, CarType, SlotId, Fee])
    end.

