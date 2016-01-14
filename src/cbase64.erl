%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% Copyright (c) 2011 Sergey Urbanovich
%% http://github.com/urbanserj/cbase64-erlang-nif
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

-module(cbase64).
-export([encode/1, decode/1]).

-on_load(on_load/0).

on_load() ->
    BaseDir =
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                EbinDir = filename:dirname(code:which(?MODULE)),
                AppPath = filename:dirname(EbinDir),
                filename:join(AppPath, "priv");
            Dir ->
                Dir
        end,
    SoName = filename:join(BaseDir, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).

-define(ENCODE_CHUNK, (6 * 1024)).
-define(DECODE_CHUNK, (8 * 1024)).

-spec encode(binary()) -> binary().
encode(Data) ->
    encode_iter(Data, <<>>).

encode_iter(<<Data:?ENCODE_CHUNK/binary, Rest/binary>>, Acc) ->
    Encoded =  nif_encode(Data),
    encode_iter(Rest, <<Acc/binary, Encoded/binary>>);
encode_iter(Data, Acc) ->
    <<Acc/binary, (nif_encode(Data))/binary>>.


-spec decode(binary()) -> binary().
decode(Data) ->
    decode_iter(Data, <<>>).

decode_iter(<<Data:?DECODE_CHUNK/binary, Rest/binary>>, Acc) ->
    Decoded = nif_decode(Data),
    decode_iter(Rest, <<Acc/binary, Decoded/binary>>);
decode_iter(Data, Acc) ->
    <<Acc/binary, (nif_decode(Data))/binary>>.

-spec nif_encode(binary()) -> binary().
nif_encode(_Data) ->
    erlang:nif_error(not_loaded, [{module, ?MODULE}, {line, ?LINE}]).

-spec nif_decode(binary()) -> binary().
nif_decode(_Data) ->
    erlang:nif_error(not_loaded, [{module, ?MODULE}, {line, ?LINE}]).
