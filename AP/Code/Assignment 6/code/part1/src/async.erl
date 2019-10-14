-module(async).

-export([new/2, wait/1, poll/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

new(Fun, Arg) ->
	gen_server:start({local, async}, ?MODULE, [], []),
	gen_server:call(async, {start_action, Fun, Arg}).

wait(Aid) -> 
	case gen_server:call(async, {wait_action, Aid}) of
		{ok, Result} -> Result;
		{exception, E} -> throw(E)
	end.

poll(Aid) -> gen_server:call(async, {poll_action, Aid}).

%% Optional functions, recommended

init(_) -> {ok, #{}}.

handle_call({start_action, Fun, Arg}, _, Map) ->
	BrokerRef = self(),
	ActionId = make_ref(),
	spawn(fun() -> 
			try
				Result = Fun(Arg),
				gen_server:cast(BrokerRef, {completed, ActionId, {ok, Result}})
			catch
				exit: E ->
					gen_server:cast(BrokerRef, {completed, ActionId, {exception, E}});
				error: E ->
					gen_server:cast(BrokerRef, {completed, ActionId, {exception, E}});
				throw: E ->
					gen_server:cast(BrokerRef, {completed, ActionId, {exception, E}})
			end
		  end),
	{reply, ActionId, maps:put(ActionId, {nothing, []}, Map)};

handle_call({wait_action, ActionId}, Pid, Map) ->
	case maps:find(ActionId, Map) of
		{ok, {nothing, Queue}} -> {noreply, maps:update(ActionId, {nothing, [Pid | Queue]}, Map)};
		{ok, {Result, _}} -> {reply, Result, Map};
		error -> fuck_you
	end;

handle_call({poll_action, ActionId}, _, Map) ->
	case maps:find(ActionId, Map) of 
		{ok, {Result, _}} ->
			{reply, Result, Map};
		error -> fuck_you
	end.

handle_cast({completed, ActionId, Result}, Map) ->
	case maps:find(ActionId, Map) of 
		{ok, {_, Queue}} ->
			lists:map(fun(Pid) -> gen_server:reply(Pid, Result) end, Queue),
			{noreply, maps:update(ActionId, {Result, Queue}, Map)};
		error ->
			fuck_you,
			{noreply, Map}
	end.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.
