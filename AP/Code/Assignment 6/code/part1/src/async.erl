-module(async).

-export([new/2, wait/1, poll/1]).

new(Fun, Arg) -> nope.
wait(Aid) -> nope.
poll(Aid) -> nope.

%% Optional functions, recommended

% wait_catch(Aid) -> nope.
% wait_any(Aids) -> nope.
