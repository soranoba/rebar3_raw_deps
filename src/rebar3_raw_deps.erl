%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Provider for supporting the raw deps.
-module(rebar3_raw_deps).
-behaviour(provider).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback APIs
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, do/1, format_error/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(DEBUG(Format, Args), rebar_log:log(debug, "[~s:~p] "++Format++"~n", [?MODULE, ?LINE | Args])).
-define(INFO(Format, Args),  rebar_log:log(info,  "[~s:~p] "++Format++"~n", [?MODULE, ?LINE | Args])).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
init(State) ->
    case get(?MODULE) =:= undefined of
        true ->
            InstallDepsProvider = providers:get_provider(install_deps, rebar_state:providers(State)),
            InstallDepsModule   = providers:module(InstallDepsProvider),
            put(?MODULE, InstallDepsProvider),

            Provider = list_to_tuple(lists:map(fun(X) when X =:= InstallDepsModule -> ?MODULE;
                                                  (X)                               -> X
                                               end, tuple_to_list(InstallDepsProvider))),
            {ok, force_override_provider(State, Provider)};
        false ->
            {ok, State}
    end.

%% @private
do(State) ->
    InstallDepsProvider = get(?MODULE),
    InstallDepsModule   = providers:module(InstallDepsProvider),
    case InstallDepsModule:do(State) of
        {ok, State1} ->
            {ok, State1};
        {error, {_, {dep_app_not_found, AppDir, AppName}}} ->
            ?DEBUG("dep_app_not_found : app_dir = ~s, app_name = ~s", [AppDir, AppName]),
            EbinDir = filename:join([AppDir, "ebin"]),
            ok      = filelib:ensure_dir(filename:join([EbinDir, "dummp"])),
            AppFile = filename:join([EbinDir, binary_to_list(AppName) ++ ".app"]),
            AppTemplate = <<"{application, {{app_name}},\n",
                            " [{applications,[]},\n",
                            "  {description,\"\"},\n",
                            "  {registered, []},\n",
                            "  {vsn,\"\"},\n",
                            "  {env,[]},\n",
                            "  {modules,[]}\n",
                            " ]}.">>,
            ok = file:write_file(AppFile, bbmustache:render(AppTemplate, [{"app_name", AppName}])),
            ?DEBUG("auto generate ~s", [AppFile]),
            do(State);
        {error, Other} ->
            {error, Other}
    end.

%% @private
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @private
-spec force_override_provider(rebar_state:t(), providers:t()) -> rebar_state:t().
force_override_provider(State0, Provider) ->
    Old    = rebar_state:allow_provider_overrides(State0),
    State1 = rebar_state:allow_provider_overrides(State0, true),
    State2 = rebar_state:add_provider(State1, Provider),
    rebar_state:allow_provider_overrides(State2, Old).
