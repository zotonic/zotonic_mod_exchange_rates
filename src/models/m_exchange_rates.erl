%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2024 Marc Worrell
%% @doc Model to fetch the list of currencies and the "base" currency.
%% @end

%% Copyright 2017-2024 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.-module(filter_exchange_rates).
-module(m_exchange_rates).

-export([
    m_get/3
    ]).

m_get([ <<"rates">> | Rest ], _Msg, Context) ->
    {ok, Rates} = mod_exchange_rates:rates(Context),
    {ok, {Rates, Rest}};
m_get([ <<"currencies">> | Rest ], _Msg, Context) ->
    {ok, Currencies} = mod_exchange_rates:rates(Context),
    {ok, {Currencies, Rest}};
m_get([ <<"convert">>, From, To, Amount | Rest ], _Msg, Context) ->
    case mod_exchange_rates:exchange(z_convert:to_float(Amount), From, To, Context) of
        {ok, ToAmount} -> {ok, {ToAmount, Rest}};
        {error, _} = Error -> Error
    end;
m_get([ <<"base_currency">> | Rest ], _Msg, Context) ->
    Currency = mod_exchange_rates:base_currency(Context),
    {ok, {Currency, Rest}}.
