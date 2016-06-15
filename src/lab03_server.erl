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
  % TODO: envoyer message au client (client_id: <client id>) pour le moment client_id == Pid
  gen_tcp:send(Socket, list_to_binary([<<"client_id: ">>] ++ pid_to_list(Pid))),
  gen_tcp:controlling_process(Socket, Pid),
  accept(LSocket).

%parser(Bin) ->
%  case Bin of
%    <<"subscribe:", Rest/binary>> ->
%      io:format("Rest [~s]~n", [Rest]);
%
%    <<"hello">> -> "Alice\n";
%    _ -> "Unexpected msg \n"
%  end.

-spec loop(Socket) -> ok when
  Socket :: inet:socket().
loop(Socket) ->
  io:format("loop~n"),
  receive_line(Socket).
  %receive
  %  {tcp, Socket, Bin} ->
  %    gen_tcp:send(Socket, parser(Bin)),
  %    %gen_tcp:send(Socket, Bin),
  %    loop(Socket);
  %  {tcp_closed, Socket} ->
  %    ?LOG("Server: peer socket closed~n");
  %  Any ->
  %    exit({loop_unexpected_msg, Any})
  %end.

% on dédie la tache de recevoir une ligne
receive_line(Socket) ->
  io:format("start receive_line~n"),
  receive_line(Socket, []).

% une ligne est complète uniquement lorsque le delimiteur est rencontré (\n\n)
receive_line(Socket, SoFar) ->
  io:format("receive_line~n"),
  receive
    {tcp, Socket, Bin} ->
      % TODO: keep this one for the lab
      % case binary:split(Bin, [<<"\n\n">>]) of
      % for testing with telnet...
      case binary:split(Bin, [<<"!!">>]) of
        % délimiteur "\n\n" trouvé
        [Token, Rest] ->
          io:format("found~n"),
          % on gère la ligne trouvée et on continue avec le reste
          parse_line(Socket, Token),
          receive_line(Socket,Rest);
        [Bin] ->
          % si le délimiteur n'est pas trouvé, on continue la reception jusqu'a en trouver un
          io:format("not found~n"),
          receive_line(Socket, [SoFar|Bin]) % y a plus optimisé ?
      end;
    {tcp_closed, Socket} ->
      ?LOG("Server: peer socket closed~n");
    Any ->
      exit({unexpected_msg, Any})
  end.

% Maintenant qu'on a une ligne, on peut la parse
parse_line(Socket, Line) ->
  case Line of
    <<"status">> ->
      io:format("parsing status required~n");
      % TODO: send client's status

    <<"topic: ", Rest/binary>> ->
      io:format("parsing topic [~s]~n", [Rest]),
      %TODO remplacer '!' par le vrai délimiteur '\n' + controle si on trouve bien le delimiter...
      [Topic, Message] = binary:split(Rest, [<<"!body: ">>]),
      io:format("topic : ~s~n", [Topic]),
      io:format("body : ~s~n", [Message]),
      case Topic of
        <<"system">> ->
          gen_tcp:send(Socket, <<"error: system is a reserved topic">>);
        _ ->
          % TODO: créer le topic si besoin + forwarding aux autres clients
          % on envoi la quittance au client
          Response = list_to_binary(<<"accepted ">> ++ Topic),
          gen_tcp:send(Socket, Response)
      end;
    <<"subscribe: ", Rest/binary>> ->
      io:format("parsing subscribe rest [~s]~n", [Rest]),
      Topic_list = binary:split(Rest, <<",">>, [global]);
      % TODO: subscription_handler


    % testing things
    <<"hello">> ->
      gen_tcp:send(Socket, <<"Alice\n">>);
    _ ->
      gen_tcp:send(Socket, <<"Syntax error \n">>)
  end.

% end module