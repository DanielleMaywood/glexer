-module(glexer_ffi).
-export([drop_byte/1]).

drop_byte(String) ->
    case String of
        <<_, Rest/bytes>> -> Rest;
        _ -> String
    end.
