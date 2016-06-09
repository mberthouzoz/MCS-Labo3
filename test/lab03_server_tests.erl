-module(lab03_server_tests).
-author("Michaël").

-include_lib("eunit/include/eunit.hrl").

start_test() ->
  Pid = lab03_server:start_link(),
  lab03_server:stop(Pid),
  ?assert(true).