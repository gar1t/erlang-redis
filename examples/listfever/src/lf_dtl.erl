-module(lf_dtl).

-export([render/2]).

render(Template, Vars) ->
    M = dtl_module(Template),
    case M:render(Vars) of
        {ok, Rendered} -> Rendered;
        {error, Err} -> error(Err)
    end.

dtl_module(Template) ->
    Module = module_name(Template),
    case dtl_reload_enabled() of
        true -> compile(Template, Module);
        false ->
            case code:is_loaded(Module) of
                {file, _} -> Module;
                false -> compile(Template, Module)
            end
    end.

compile(Template, Module) ->
    erlydtl:compile(template_file(Template), Module),
    Module.

module_name(Template) ->
    list_to_atom("listfever_" ++ Template ++ "_dtl").

template_file(Template) ->
    Root = filename:join(listfever:priv_dir(), "dtl"),
    filename:join(Root, Template ++ ".dtl").

dtl_reload_enabled() ->
    case application:get_env(listfever, dtl_reload) of
        {ok, Enabled} -> Enabled;
        undefined -> false
    end.
