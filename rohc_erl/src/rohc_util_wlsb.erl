-module(rohc_util_wlsb).

-record(wlsb,
        {         
         window       = [],
         window_width,
         window_mask,
         lsb_shift_p  = 0
         
         
         }
       ).