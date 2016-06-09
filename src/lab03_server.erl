-module(lab03_server).

%% lab03_server: lab03_server library's entry point.

-export([start_link/0, stop/1]).

-include("micromq.hrl").

%% ===================================================================
%% API.
%% ===================================================================

%% @doc Start the echo server.
-spec start_link() -> pid().
start_link() ->
  %spawn_link(fun() -> server(-1) end). % <= Try this with the dialyzer!
  spawn_link(fun() -> server(?PORT) end).

%% Stop the echo server.
-spec stop(Pid) -> stop when
  Pid :: pid().
stop(Pid) ->
  Pid ! stop.


%% ===================================================================
%% Private functions.
%% ===================================================================

-spec server(Port) -> true when
  Port :: inet:port_number().
server(Port) ->
  % Since we don't close the listen socket and this server is sequential,
  % subsequent clients will be allowed to connect until the kernel listen
  % backlog fills up, which is fine.
  {ok, LSocket} = gen_tcp:listen(Port, [binary, {reuseaddr, true},
    {active, true}]), % WARNING No backpressure!
  % Design point: Why spawning another process instead of just calling
  % `accept/1` directly? We spawn another process to allow the main one to
  % be always reactive to Erlang messages, despite the fact that
  % `gen_tcp:accept/1` is blocking in a unnatural way for Erlang (outside a
  % `receive` block).
  Acceptor = spawn_link(fun() -> accept(LSocket) end),
  receive
    stop ->
      ?LOG("Server: stopping~n"),
      % Terminate the acceptor also if it is blocked.
      exit(Acceptor, stop);
    Any ->
      ?LOG("Server: unexpected msg ~p~n", [Any]),
      exit({server_unexpected_msg, Any})
  end.

-spec accept(LSocket) -> no_return() when
  LSocket :: inet:socket().
accept(LSocket) ->
  io:format("accept~n"),
  {ok, Socket} = gen_tcp:accept(LSocket), % WARNING Blocking!
  Pid = spawn_link(fun() -> loop(Socket) end),
  gen_tcp:controlling_process(Socket, Pid),
  accept(LSocket).

-spec loop(Socket) -> ok when
  Socket :: inet:socket().
loop(Socket) ->
  io:format("loop~n"),
  receive
    {tcp, Socket, Bin} ->
      gen_tcp:send(Socket, Bin),
      loop(Socket);
    {tcp_closed, Socket} ->
      ?LOG("Server: peer socket closed~n");
    Any ->
      exit({loop_unexpected_msg, Any})
  end.
% end module