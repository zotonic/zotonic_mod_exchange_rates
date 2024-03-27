%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2024 Marc Worrell
%% @doc Support for currency exchange rates, automatically fetches rates.
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
%% limitations under the License.

-module(mod_exchange_rates).

-mod_author("Marc Worrell <marc@worrell.nl>").
-mod_title("Exchange Rates").
-mod_description("Provide methods to access exchange rates between currencies.").
-mod_depends([mod_tkvstore]).

-behaviour(gen_server).

-define(BASE_CURRENCY, <<"USD">>).

-define(BTC_JSON_URL, "http://api.bitcoincharts.com/v1/weighted_prices.json").
-define(ECB_XML_URL, "https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml").

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-export([
    observe_tick_12h/2,
    currencies/1,
    rates/1,
    rate/2,
    convert/4,
    update/1,
    base_currency/1,
    fetch/1,
    fetch_ecb/0,
    fetch_btc/1
    ]).

-record(state, {
        site :: atom(),
        rates :: #{ binary() => float() }
    }).

-record(rates, {
        base :: binary(),
        rates :: #{ binary() => float() }
    }).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Update the exchange rates.
-spec observe_tick_12h(tick_12h, Context) -> ok when
    Context :: z:context().
observe_tick_12h(_tick, Context) ->
    update(Context).

%% @doc Force an update of all exchange rates, this is done automatically every
%% 12 hours.
-spec update(Context) -> ok when
    Context :: z:context().
update(Context) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    gen_server:cast(Name, update).

%% @doc Return the base currency.
-spec base_currency(Context) -> Currency when
    Context :: z:context(),
    Currency :: binary().
base_currency(_Context) ->
    ?BASE_CURRENCY.


%% @doc Return all currencies with known exchange rates.
-spec currencies(Context) -> {ok, Currencies} | {error, Reason} when
    Context :: z:context(),
    Currencies :: [ Currency ],
    Currency :: binary(),
    Reason :: term().
currencies(Context) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    case gen_server:call(Name, rates) of
        {ok, Rates} ->
            {ok, lists:sort(maps:keys(Rates))};
        {error, _} = Error ->
            Error
    end.

%% @doc Return all rates relative to the default base currency. If a currency is
%% worth more than the base currency then the rate is smaller than 1.0.
-spec rates(Context) -> {ok, Rates} | {error, Reason} when
    Context :: z:context(),
    Rates :: #{ Currency => Rate },
    Currency :: binary(),
    Rate :: float(),
    Reason :: term().
rates(Context) ->
    Name = z_utils:name_for_site(?MODULE, Context),
    gen_server:call(Name, rates).

%% @doc Return the current rate of a currency relative to the default base currency.
%% If a currency is worth more than the base currency then the rate is smaller than 1.0.
-spec rate1(Currency, Context) -> {ok, Rate} | {error, Reason} when
    Currency :: binary(),
    Context :: z:context(),
    Rate :: float(),
    Reason :: unknown_currency | term().
rate(Currency, Context) ->
    rate1(z_string:to_upper(z_convert:to_binary(Currency)), Context).

rate1(<<"BTC">>, Context) ->
    rate1(<<"XBT">>, Context);
rate1(Currency, Context) ->
    case base_currency(Context) of
        Currency ->
            {ok, 1.0};
        _ ->
            Name = z_utils:name_for_site(?MODULE, Context),
            gen_server:call(Name, {rate, Currency})
    end.

%% @doc Convert an amount in a currency to another currency.
-spec convert(FromAmount, FromCurrency, ToCurrency, Context) -> {ok, ToAmount} | {error, Reason} when
    FromAmount :: float() | integer(),
    FromCurrency :: binary(),
    ToCurrency :: binary(),
    Context :: z:context(),
    ToAmount :: float(),
    Reason :: currency_from | currency_to | term().
convert(_Amount, undefined, _To, _Context) ->
    {error, currency_from};
convert(_Amount, _From, undefined, _Context) ->
    {error, currency_to};
convert(Amount, From, From, _Context) ->
    {ok, z_convert:to_float(Amount)};
convert(Amount, From, To, Context) ->
    Amount1 = z_convert:to_float(Amount),
    FromRate = rate(From, Context),
    ToRate = rate(To, Context),
    case {FromRate, ToRate} of
        {{ok, FR}, {ok, TR}} when FR > 0.0, TR > 0.0 ->
            {ok, round_currency(To, Amount1 / FR * TR)};
        {{error, _}, _} ->
            {error, currency_from};
        {_, {error, _}} ->
            {error, currency_to}
    end.

%% @doc Round an amount to the number of expected digits for that currency.
round_currency(Currency, Amount) ->
    case exponent(z_string:to_upper(z_convert:to_binary(Currency))) of
        0 -> round(Amount);
        N ->
            Exp = exp(N),
            round(Amount * Exp) / Exp
    end.

exp(N) -> exp(N,1).

exp(0, Acc) -> Acc;
exp(N, Acc) -> exp(N-1, 10*Acc).

% See https://en.wikipedia.org/wiki/ISO_4217
exponent(<<"BHD">>) -> 3;
exponent(<<"BIF">>) -> 0;
exponent(<<"BYR">>) -> 0;
exponent(<<"CLF">>) -> 4;
exponent(<<"CLP">>) -> 0;
exponent(<<"CVE">>) -> 0;
exponent(<<"DJF">>) -> 0;
exponent(<<"GNF">>) -> 0;
exponent(<<"IQD">>) -> 3;
exponent(<<"ISK">>) -> 0;
exponent(<<"JOD">>) -> 3;
exponent(<<"JPY">>) -> 0;
exponent(<<"KMF">>) -> 0;
exponent(<<"KRW">>) -> 0;
exponent(<<"KWD">>) -> 3;
exponent(<<"MGA">>) -> 1;
exponent(<<"MRO">>) -> 1;
exponent(<<"OMR">>) -> 3;
exponent(<<"PYG">>) -> 0;
exponent(<<"RWF">>) -> 0;
exponent(<<"TND">>) -> 3;
exponent(<<"UGX">>) -> 0;
exponent(<<"UYI">>) -> 0;
exponent(<<"VND">>) -> 0;
exponent(<<"VUV">>) -> 0;
exponent(<<"XAF">>) -> 0;
exponent(<<"XOF">>) -> 0;
exponent(<<"XPF">>) -> 0;
exponent(<<"XBT">>) -> 8; % Bitcoin new
exponent(<<"BTC">>) -> 8; % Bitcoin old
exponent(_) -> 2.


%% @doc Start the gen_server
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Name = z_utils:name_for_site(?MODULE, Context),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, #state{}}.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    z_context:logger_md(Context),
    gen_server:cast(self(), update),
    {ok, #state{
        site = z_context:site(Context),
        rates = #{}
    }}.

handle_call({rate, Currency}, _From, #state{rates=Rates} = State) ->
    case maps:get(Currency, Rates, undefined) of
        undefined ->
            {reply, {error, unknown_currency}, State};
        Rate ->
            {reply, {ok, Rate}, State}
    end;
handle_call(rates, _From, #state{rates=Rates} = State) ->
    {reply, {ok, Rates}, State};
handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(update, State) ->
    Self = self(),
    erlang:spawn_link(
        fun() ->
            Fetched = fetch(z_context:new(State#state.site)),
            Self ! {new_rates, Fetched}
        end),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({new_rates, FetchedRates}, #state{ rates = StateRates, site = Site } = State) ->
    Context = z_context:new(Site),
    StoredRates = case z_notifier:first(#tkvstore_get{type=?MODULE, key=rates}, Context) of
        #rates{base = ?BASE_CURRENCY, rates = Rs} when is_list(Rs) -> maps:from_list(Rs);
        #rates{base = ?BASE_CURRENCY, rates = Rs} when is_map(Rs) -> Rs;
        _ -> #{}
    end,
    StoredRates1 = maps:merge(StateRates, StoredRates),
    NewRates = maps:merge(StoredRates1, FetchedRates),
    z_notifier:first(
        #tkvstore_put{
            type = ?MODULE,
            key = rates,
            value = #rates{base = ?BASE_CURRENCY, rates = NewRates}
        },
        Context),
    {noreply, State#state{rates = NewRates}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Version, State, _Extra) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

fetch(Context) ->
    BTC = fetch_btc(Context),
    Cs = fetch_ecb(),
    Rs = maps:merge(BTC, Cs),
    Rs#{
        ?BASE_CURRENCY => 1.0
    }.

fetch_ecb() ->
    fetch_ecb_data(z_url_fetch:fetch(?ECB_XML_URL, [])).

fetch_ecb_data({ok, {_FinalUrl, _Hs, Size, <<"<?xml ", _/binary>> = XML}}) when Size > 0 ->
    case z_html_parse:parse(XML, #{ mode => xml }) of
        {ok, {<<"gesmes:Envelope">>, _Args, EnvelopeNodes}} ->
            {value, {<<"Cube">>, _, Cubes}} = lists:keysearch(<<"Cube">>, 1, EnvelopeNodes),
            {value, {<<"Cube">>, _, Cube}} = lists:keysearch(<<"Cube">>, 1, Cubes),
            Rates = lists:foldl(
                fun
                    ({<<"Cube">>, Args, _}, Acc) ->
                        {<<"currency">>, Currency} = proplists:lookup(<<"currency">>, Args),
                        {<<"rate">>, Rate} = proplists:lookup(<<"rate">>, Args),
                        Acc#{
                            Currency => z_convert:to_float(Rate)
                        };
                    (_, Acc) ->
                        Acc
                end,
                #{},
                Cube),
            case maps:get(?BASE_CURRENCY, Rates, undefined) of
                undefined ->
                    #{};
                Rate when is_float(Rate) ->
                    maps:fold(
                        fun(C, R, Acc) ->
                            Acc#{
                                C => R / Rate
                            }
                        end,
                        #{},
                        Rates#{
                            <<"EUR">> => 1.0
                        })
            end;
        _ ->
            ecb_xml_error(XML)
    end;
fetch_ecb_data(Other) ->
    ?LOG_WARNING(#{
        in => zotonic_mod_exchange_rates,
        text => <<"Fetch of ECB data failed">>,
        result => error,
        reason => Other,
        url => ?ECB_XML_URL
    }),
    #{}.

ecb_xml_error(XML) ->
    ?LOG_WARNING(#{
        in => zotonic_mod_exchange_rates,
        text => <<"Unexpected XML structure in ECB data">>,
        result => error,
        reason => xml,
        xml => XML,
        url => ?ECB_XML_URL
    }),
    #{}.


% BTC
fetch_btc(Context) ->
    fetch_btc_data(z_fetch:fetch_json(?BTC_JSON_URL, [], Context)).

fetch_btc_data({ok, JSON}) ->
    case maps:get(?BASE_CURRENCY, JSON) of
        undefined ->
            #{};
        #{ <<"24h">> := Rate } ->
            #{
                <<"XBT">> => 1.0 / z_convert:to_float(Rate)
            };
        _ ->
            ?LOG_INFO(#{
                in => zotonic_mod_exchange_rates,
                text => <<"No 24h key in BTC rates">>,
                result => error,
                reason => missing_24h,
                url => ?BTC_JSON_URL
            }),
            #{}
    end;
fetch_btc_data(Other) ->
    ?LOG_WARNING(#{
        in => zotonic_mod_exchange_rates,
        text => <<"Fetch of BTC (XBT) data failed">>,
        result => error,
        reason => Other,
        url => ?BTC_JSON_URL
    }),
    #{}.
