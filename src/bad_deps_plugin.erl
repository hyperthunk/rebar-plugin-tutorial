%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2012 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(bad_deps_plugin).
-export(['pre_get-deps'/2]).
-include_lib("annotations/include/annotations.hrl").

-base_dir(only).
'pre_get-deps'(Config, _AppFile) ->
    rebar_log:log(debug, "pre_get-deps running in ~s~n",
                    [rebar_utils:get_cwd()]),
    [ pre_load(Dep) || Dep <- rebar_config:get_local(Config, bad_deps, []) ],
    ok.

pre_load({Dep, Url, Config}) ->
    DepsDir = rebar_config:get_global(deps_dir, "deps"),
    Project = atom_to_list(Dep),
    TargetDir = filename:join(DepsDir, Project),
    case filelib:is_dir(TargetDir) of
        true ->
            %% we've already fetched this one
            ok;
        false ->
            rebar_utils:sh("git clone " ++ Url, [{cd, DepsDir}]),
            ProjectDir = filename:join([rebar_utils:get_cwd(),
                                    DepsDir, Project]),
            SrcDir = filename:join(ProjectDir, "src"),
            TestDir = filename:join([rebar_utils:get_cwd(),
                                     DepsDir, Project, "test"]),
            rebar_utils:ensure_dir(filename:join(SrcDir, "foo.txt")),
            rebar_utils:ensure_dir(filename:join(TestDir, "foo.txt")),

            generate_app_file(Project, ProjectDir),
            SrcPattern = proplists:get_value(src_main, Config, "^.*\\.erl\$"),
            TestPattern = proplists:get_value(src_test,
                                              Config, "^.*_tests\\.erl\$"),
            [ mv(Src, SrcDir) || Src <- find(ProjectDir, SrcPattern)],
            [ mv(Src, TestDir) || Src <- find(ProjectDir, TestPattern)]
    end.

generate_app_file(Project, ProjectDir) ->
    Target = filename:join([ProjectDir, "src", Project ++ ".app.src"]),
    file:write_file(Target, app(Project), [write]).

app(Project) ->
    App = {application, list_to_atom(Project),
           [{description, ""},
            {vsn, "1"},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).

find(ProjectDir, SrcPattern) ->
    rebar_utils:find_files(ProjectDir, SrcPattern).

mv(Src, SrcDir) ->
    case rebar_file_utils:mv(Src, filename:join(Src, SrcDir)) of
        ok -> ok;
        {error, Reason} -> rebar_utils:abort(Reason, [])
    end.
