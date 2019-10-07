-module(test_flamingo).

-include_lib("eunit/include/eunit.hrl").
-export([mytests/0]).

mytests() ->
    eunit:test({module, ?MODULE}, [verbose]).

start_a_flamingo_server_test() ->
    ?assertMatch({ok, _}, flamingo:start("flamingo")).

generator_start_a_flamingo_server_test_() ->
    ?_assertMatch({ok, _}, flamingo:start("flamingo")).

fancy_generator_start_a_flamingo_server_test_() ->
    {"Start a flamingo server an check that it returns a pair with ok as the first component",
     ?_assertMatch({ok, _}, flamingo:start("flamingo"))}.

%% failing_test_() ->
%%     {"We messed up the test",
%%      ?_assertMatch({{ok}, _}, flamingo:start("flamingo"))}.

greetings_test_() ->
    {"Start a greeting server, and send a request",
     fun () ->
             F = greetings:server(),
             Ref = make_ref(),
             flamingo:request(F, {"/hello", [{"name", "Ken"}]},
                              self(), Ref),
             Expected = "Greetings Ken\nYou have reached The Flamingo Server",
             receive
                 X ->
                     ?assertMatch({Ref, {200, _, Expected}}, X)
             end
     end}.

drop_route_test_() ->
    {"Start a server, send a request and then drop route.",
     fun () ->
             {ok, F} = flamingo:start(drop_test),
             Ref = make_ref(),
             {ok, Id} = flamingo:new_route(F, ["/sleep"], fun(_, _) -> timer:sleep(500), {200, "", "sleep"} end),
            flamingo:request(F, {"/sleep", []},
                              self(), Ref),
            flamingo:drop_route(F, Id),
            flamingo:get_routes(F),
            receive
                X1 ->
                    ?assertMatch([], X1)
            end,
            flamingo:request(F, {"/sleep", []},
                              self(), Ref),
            receive
                X2 ->
                    ?assertMatch({Ref, {404, _, _}}, X2)
            end,
            receive
                X3 ->
                    ?assertMatch({Ref, {200, _, _}}, X3)
            end
     end}.


