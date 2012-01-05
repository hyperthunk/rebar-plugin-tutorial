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
    rebar_log:log(debug, "pre_get-deps running in ~s~n", [rebar_utils:get_cwd()]),
    [ pre_load(Dep) || Dep <- rebar_config:get_local(Config, bad_deps, []) ],
    ok.

pre_load({Dep, Url}) ->
    DepsDir = rebar_config:get_global(deps_dir, "deps"),
    TargetDir = filename:join(DepsDir, atom_to_list(Dep)),
    case filelib:is_dir(TargetDir) of
        true ->
            %% we've already fetched this one
            ok;
        false ->
            rebar_utils:sh("git clone " ++ Url, [{cd, DepsDir}])
    end.
