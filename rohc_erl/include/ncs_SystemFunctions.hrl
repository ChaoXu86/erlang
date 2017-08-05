-ifndef(NCS_SYSTEMFUNCTIONS_HRL).
-define(NCS_SYSTEMFUNCTIONS_HRL,true).

-define(ci_var(Name,Mv), put(Name, Mv)).
-define(co_ci_var(Name,Mv), put(Name, Mv)).
-define(copy_var(Name), get(Name)).
-define(co_var(Name), get(Name)).

-define(replace_pd(Name, Mv), put({pdata,Name},Mv)).
-define(read_pd(Name,_Mv),get({pdata,Name})).

-endif.