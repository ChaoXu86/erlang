-ifndef(ROHC_HRL).
-define(ROHC_HRL, true).

%% ROHC profile
-define(p104, 16#40).  %% Profile 0x0104 support indicator
-define(p103, 16#20).  %% Profile 0x0103 support indicator
-define(p102, 16#10).  %% Profile 0x0102 support indicator
-define(p6, 16#08).    %% Profile 0x0006 support indicator
-define(p4, 16#04).    %% Profile 0x0004 support indicator
-define(p3, 16#02).    %% Profile 0x0003 support indicator
-define(p2, 16#01).    %% Profile 0x0002 support indicator

%% ROHC CIOT compress/decompress port command
-define(ROHC_PORT_INFO, 0).
-define(ROHC_INIT, 1).
-define(ROHC_COMPRESS, 2).
-define(ROHC_DECOMPRESS, 3).
-define(ROHC_CLEAR, 9).
-define(ROHC_SET_DECOMPRESSOR_RATE_LIMITS, 11).
-define(ROHC_SET_COMPRESSOR_TRACE, 21).
-define(ROHC_SET_DECOMPRESSOR_TRACE, 22).

-define(ROHC_CC_OK, 0).

-record(hcc,
        {
         profile,
         max_cid
         }).

-record(hcc_status,
        {ebi0to7 = 0,
         ebi8to15 = 0}).

-endif.