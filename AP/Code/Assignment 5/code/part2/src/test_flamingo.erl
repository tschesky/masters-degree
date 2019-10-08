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

loop_message(_, _, _, 0) -> ok;
loop_message(F, Path, Expected, Counter) ->
    Ref = make_ref(),
    flamingo:request(F, {Path, []}, self(), Ref),
    receive
        X ->
            ?assertMatch({Ref, {200, _, Expected}}, X),
            loop_message(F, Path, Expected, Counter - 1)
    end.

mood_test_() ->
    {"Start a mood server and see if it works",
     fun() ->
            F = mood:server(),
            loop_message(F, "/mood", "Sad", 5),
            loop_message(F, "/moo", "That's funny", 1),
            loop_message(F, "/mood", "Happy!", 5)
     end}.

latest_path_test_() ->
    {"See if latest path is called",
     fun() ->
            {ok, F} = flamingo:start(latest_path),
            {ok, Id1} = flamingo:new_route(F, ["/test"], fun(_, _) -> {200, "text/html", "test1"} end),
            {ok, Id2} = flamingo:new_route(F, ["/test"], fun(_, _) -> {200, "text/html", "test2"} end),
            {ok, Id3} = flamingo:new_route(F, ["/test"], fun(_, _) -> {200, "text/html", "test3"} end),
            loop_message(F, "/test", "test3", 1),
            flamingo:drop_route(F, Id3),
            loop_message(F, "/test", "test2", 1),
            flamingo:drop_route(F, Id2),
            loop_message(F, "/test", "test1", 1),
            flamingo:drop_route(F, Id1),
            receive
                X ->
                    ?assertMatch({Ref, {404, _, _}}, X)
            end
     end}.

drop_route_test_() ->
    {"Start a server, send a request and then drop route.",
     fun () ->
            {ok, F} = flamingo:start(drop_test),
            Ref = make_ref(),
            {ok, Id} = flamingo:new_route(F, ["/sleep"], fun(_, _) -> timer:sleep(500), _ = (10/0), {200, "", "sleep"} end),
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
                    ?assertMatch({Ref, {500, _, _}}, X3)
            end
     end}.

counter_test_() ->
    {"Start a server, send a request and then drop route.",
     fun () ->
            S = counter:server(),
            Ref = make_ref(),
            flamingo:request(S, {"/inc_with", [{"x", "42"}]}, 
                              self(), Ref),
            receive
                 X ->
                     ?assertMatch({Ref, {200, _, "42"}}, X)
            end,
            flamingo:request(S, {"/dec_with", [{"x", "42"}]}, 
                              self(), Ref),
            receive
                 X1 ->
                     ?assertMatch({Ref, {200, _, "0"}}, X1)
            end,
            flamingo:request(S, {"/inc_with", [{"x", "-42"}]}, 
                              self(), Ref),
            receive
                 X2 ->
                     ?assertMatch({Ref, {200, _, "1"}}, X2)
            end,
            flamingo:request(S, {"/dec_with", [{"x", "towel"}]},
                              self(), Ref), 
            receive
                 X3 ->
                     ?assertMatch({Ref, {200, _, "0"}}, X3)
            end,
            flamingo:request(S, {"/inc_with", [{"y", "towel"}]},
                              self(), Ref), 
            receive
                 X4 ->
                     ?assertMatch({Ref, {200, _, "1"}}, X4)
            end,
            flamingo:request(S, {"/dec_with", [{"xasfasd", "fdhfgsh"},{"x", "1338"}]},
                              self(), Ref), 
            receive
                 X5 ->
                     ?assertMatch({Ref, {200, _, "-1337"}}, X5)
            end,
            flamingo:request(S, {"m a -> (a -> m b) -> m b", [{"xasfasd", "fdhfgsh"},{"x", "1338"}]},
                              self(), Ref), 
            receive
                 X6 ->
                     ?assertMatch({Ref, {404, _, _}}, X6)
            end
     end}.


