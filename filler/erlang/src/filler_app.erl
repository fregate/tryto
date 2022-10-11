%%%-------------------------------------------------------------------
%% @doc filler public API
%% @end
%%%-------------------------------------------------------------------

-module(filler_app).

-behaviour(application).

-export([start/2, stop/1, fill/2, assembly_thread/3, filler_thread/2]).

start(_StartType, _StartArgs) ->
    filler_sup:start_link().

stop(_State) ->
    ok.

fill(Threads, Limit) ->
    register(main_thread, spawn(?MODULE, assembly_thread, [[], Threads, erlang:monotonic_time()])),
    Chunk = (Limit - Threads) div Threads,
    run_fillers(Threads, 1, Chunk + 1),
    ok.

assembly_thread(Items, 0, TimePoint) ->
    io:format("duration: ~p, size: ~p, items: ~p~n", [erlang:monotonic_time() - TimePoint, length(Items), Items]),
    ok;
assembly_thread(Items, Threads, TimePoint) ->
    receive
        {entry, Num} ->
            assembly_thread(lists:append(Items, [Num]), Threads, TimePoint);
        {finished, _} ->
            io:format("threads left: ~p~n", [Threads - 1]),
            assembly_thread(Items, Threads - 1, TimePoint);
        _ -> io:format("unknown~n")
    end.

filler_thread(Finish, Finish) ->
    main_thread ! {finished, Finish},
    io:format("filler thread finished~n"),
    ok;
filler_thread(Num, Finish) ->
    case (Num rem 11) of
        0 ->
            main_thread ! {entry, Num},
            filler_thread(Num + 1, Finish);
        _ ->
            filler_thread(Num + 1, Finish)
    end.

%% internal functions

run_fillers(0, _, _) ->
    io:format("fillers are running~n"),
    ok;
run_fillers(Threads, Start, Chunk) ->
    spawn(?MODULE, filler_thread, [Start, Chunk]),
    io:format("run thread for [~p, ~p)~n", [Start, Start + Chunk]),
    run_fillers(Threads - 1, Start + Chunk, Chunk).
