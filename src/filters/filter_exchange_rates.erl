%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2025 Marc Worrell
%% @doc Filter to fetch exchange rates or list a single rate.
%% @end

%% Copyright 2017-2025 Marc Worrell
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

-module(filter_exchange_rates).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "template_filter", "data_processing_and_formatting",
        "number"
    ]
}).
-moduledoc("
Look up a rate or convert a numeric amount with `mod_exchange_rates`.

```django
{{ \"EUR\"|exchange_rates }}
{{ 100|exchange_rates:\"EUR\" }}
{{ 100|exchange_rates:\"EUR\":\"USD\" }}
```

With no arguments, the input is a currency code and the result is its rate
in currency units per USD. With one argument, the input is an amount in USD
and the argument is the destination currency. With two arguments, the first
is the source currency and the second is the destination currency; the last
example converts 100 EUR to USD.

Currency codes are case-insensitive for rate lookup. Translated currency
values are resolved using the current language fallback. Undefined inputs,
empty currency codes, unsupported tuple values and failed currency lookups
produce `undefined`. Amounts are converted with `z_convert:to_float/1`.
Cross-currency results follow the currency rounding rules of
`mod_exchange_rates`; identical currencies return the amount as a float.
The filter does not format a currency symbol or localized number and does
not perform an ACL check.
").

-export([
    exchange_rates/2,
    exchange_rates/3,
    exchange_rates/4
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

exchange_rates(undefined, _Context) -> undefined;
exchange_rates(Currency, Context) ->
    case to_currency(Currency, Context) of
        undefined -> undefined;
        Cry ->
            case mod_exchange_rates:rate(Cry, Context) of
                {ok, Rate} -> Rate;
                _ -> undefined
            end
    end.

exchange_rates(_, undefined, _Context) -> undefined;
exchange_rates(undefined, _, _Context) -> undefined;
exchange_rates(Value, ToCurrency, Context) ->
    Base = mod_exchange_rates:base_currency(Context),
    exchange_rates(Value, Base, ToCurrency, Context).

exchange_rates(_, _, undefined, _Context) -> undefined;
exchange_rates(_, undefined, _, _Context) -> undefined;
exchange_rates(undefined, _, _, _Context) -> undefined;
exchange_rates(Value, FromCurrency, ToCurrency, Context) ->
    Value1 = z_convert:to_float(Value),
    ToCr = to_currency(ToCurrency, Context),
    FromCr = to_currency(FromCurrency, Context),
    case mod_exchange_rates:convert(Value1, FromCr, ToCr, Context) of
        {ok, V} -> V;
        {error, _} -> undefined
    end.


to_currency(undefined, _Context) -> undefined;
to_currency(<<>>, _Context) -> undefined;
to_currency([], _Context) -> undefined;
to_currency(#trans{} = Tr, Context) ->
    to_currency(z_trans:lookup_fallback(Tr, Context), Context);
to_currency(C, _Context) when is_tuple(C) -> undefined;
to_currency(Currency, _Context) -> z_convert:to_binary(Currency).

