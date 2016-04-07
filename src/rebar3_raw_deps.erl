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
    Provider = providers:create([{name, raw},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, [app_discovery]},
                                 {short_desc, "Automatically generate the .app if it is not a OTP application."},
                                 {opts, opts()}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @private
do(State) ->
    case do_impl(State) of
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
            ?INFO("auto generate ~s", [AppFile]),
            do(State);
        {error, Other} ->
            ?DEBUG("other error : ~p", [Other]),
            {error, Other}
    end.

%% @private
format_error(Reason) ->
    rebar_prv_install_deps:format_error(Reason).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions Functions
%%----------------------------------------------------------------------------------------------------------------------

do_impl(State) ->
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Command = case proplists:get_value(upgrade, RawOpts, false) of
                  true  -> upgrade;
                  false -> install_deps
              end,
    Args = proplists:get_value(task, RawOpts, ""),
    Providers = rebar_state:providers(State),
    Namespace = rebar_state:namespace(State),
    CommandProvider = providers:get_provider(Command, Providers, Namespace),
    Opts = providers:opts(CommandProvider),
    Mod  = providers:module(CommandProvider),
    case getopt:parse(Opts, Args) of
        {ok, ParsedArgs} ->
            ?DEBUG("raw internal command = ~s, parsed args = ~p", [Command, ParsedArgs]),
            Mod:do(rebar_state:command_parsed_args(State, ParsedArgs));
        {error, Reason} ->
            {error, lists:flatten(getopt:format_error(Reason))}
    end.

opts() ->
    [
     {upgrade, $u, undefined, undefined, "Upgrade project dependencies when raw dependencies exists"}
    ].
