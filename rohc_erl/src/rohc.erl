%% @author ezddxxy
%% @doc @todo Add description to rohc.


-module(rohc).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         compression/1,
         decompression/1,
         create_configuration/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

compression(PlaneData) ->
    rohc_main:compression(PlaneData).

decompression(CompressedData) ->
    rohc_main:decompression(CompressedData).

create_configuration(ConfigData) ->
    rohc_main:create_configuration(ConfigData).