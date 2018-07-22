-module(pricing).

-export([calculate/2]).

-include("logging.hrl").

%% calculate price on different type
calculate(CarType, Duration) ->
    case CarType of
        "BMW" ->
            %% 1 yuan per second
            1 * Duration;
        "VW" ->
            2 * Duration;
        _Other ->
            3 * Duration
    end.

