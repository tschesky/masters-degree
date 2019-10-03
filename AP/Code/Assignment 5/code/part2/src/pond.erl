%% Simple web server for flamingos.
-module(pond).
-author('Ken Friis Larsen <kflarsen@diku.dk>').
-export([introduce/1, introduce/2, drain/1]).

%% introduce/1 takes a Flamingo and the web server on port 4242.

%% introduce/2 takes a Flamingo and a port number and starts the web server at that port.

introduce(Flamingo) ->
    introduce(Flamingo, 4242).
introduce(Flamingo, Port) ->
    {ok, ListenSoc} = gen_tcp:listen(Port, [{reuseaddr,true},list]),
    io:format("Your Flamingo has been introduced into the pond~n"),
    io:format("You can see it at http://localhost:~p~n",[Port]),
    spawn(fun() -> acceptor(ListenSoc, Flamingo) end),
    spawn(fun() -> receive
                       stop_and_go_home ->
                           io:format("The pond will now be drained. Take care of your Flamingo~n"),
                           gen_tcp:close(ListenSoc)
                   end
          end).

drain(Pond) ->
    Pond ! stop_and_go_home.

%% The acceptor/2 function accepts a connection, spawns a new acceptor, and
%% then handles its incoming request.
acceptor(ListenSoc, Flamingo) ->
    {ok, Soc} = gen_tcp:accept(ListenSoc),
    ok = inet:setopts(Soc, [{packet,http}]),
    spawn(fun() -> acceptor(ListenSoc, Flamingo) end),
    Req = read_request(Soc, none, none, []),
    handle_request(Soc, Flamingo, Req).


%% The read_request/4 function reads the HTTP request and returns it
%% as a triple consisting of the HTTP method, the URI and the headers
read_request(Soc, Meth, Path, Hdrs) ->
    ok = inet:setopts(Soc, [{active, once}]),
    HttpMsg = receive
                  {http, Soc, Msg} -> Msg;
                  _ -> gen_tcp:close(Soc)
              end,
    case HttpMsg of
        {http_request, NewMeth, {abs_path, NewPath}, _V} ->
            read_request(Soc, NewMeth, NewPath, Hdrs);
        {http_header, _, Hdr, _, Val} ->
            read_request(Soc, Meth, Path, [{Hdr,Val}|Hdrs]);
        http_eoh ->
            {Meth, Path, Hdrs};
        {http_error, Error} ->
            exit(Error);
        ok ->
            exit("Error reading HTTP request")
    end.


%% handle_request/3 handle a GET request by first splitting the URI up
%% into the path and the query parameters, and then forward the
%% request to the given Flamingo server. All other request gives an
%% error.
handle_request(Soc, Flamingo, {'GET', PathQuery, _ }) ->
    {Path, QueryS} =  httpd_util:split_path(PathQuery),
    Query = case QueryS of
                [$?|Q] -> httpd:parse_query(Q);
                _      -> []
            end,
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Flamingo, {Path, Query}, Me, Ref),
    {Status, Mime, Content} = receive
                                  {Ref, Reply} -> Reply
                              end,
    ok = inet:setopts(Soc, [{packet, raw}]),
    ok = gen_tcp:send(Soc, ["HTTP/1.0 ", integer_to_list(Status), "\r\n",
                            "Content-Type: ", Mime, "\r\n",
                            "\r\n", Content]),
    gen_tcp:close(Soc);
handle_request(Soc, _, _) ->
    Status = 500,
    ok = inet:setopts(Soc, [{packet, raw}]),
    ok = gen_tcp:send(Soc, ["HTTP/1.0 ", integer_to_list(Status), "\r\n",
                            "\r\n", "Unsupported HTTP Method"]),
    gen_tcp:close(Soc).
