For the idea of delta replica, please refer to
https://github.com/dead911/erlang/wiki/erlang-delta_replica

The delta_spec_util.erl contains core functions of the delta replica. Known issues are
* only supports list/tuple with maxium 50 elements. (could be changed in code)
* '$1', '$2' ... are reserved for delta spec. 
