(* :Package: *)

BeginPackage["KirillBelov`WaexLink`REST`"];


WaexExchanges::usage = 
"WaexExchanges[] - returns list of available exchanges."; 


WaexMarkets::usage = 
"WaexMarkets[] - returns list of available markets."; 


WaexTokens::usage = 
"WaexTokens[] - returns list of available tokens.";


WaexOrderBooks::usage = 
"WaexOrderBooks[pair] - order books for a given pair.";


WaexTrades::usage = 
"WaexTrades[pair] - trades for a given pair.";


WaexTickers::usage = 
"WaexTickers[pair] - tickers for a given pair.";


WaexCandleSticks::usage = 
"WaexCandleSticks[pair] - candle sticks for a given pair.";


Begin["`Private`"];


(* :Internal: *)


Options[waexRequest] = {
    "Endpoint" :> "https://access.ccdb.waexservices.com", 
    "APIToken" :> SystemCredential["WAEX_API_TOKEN"], 
    "HTTPMethod" :> "GET"
};


waexRequest[path_, query: _Association: <||>, opts: OptionsPattern[{}]] := 
Module[{
    endpoint = OptionValue[waexRequest, FilterRules[Flatten[{opts}], Options[waexRequest]], "Endpoint"], 
    apiToken = OptionValue[waexRequest, FilterRules[Flatten[{opts}], Options[waexRequest]], "APIToken"], 
    httpMethod = OptionValue[waexRequest, FilterRules[Flatten[{opts}], Options[waexRequest]], "HTTPMethod"]
}, 
    encodedQuery = encode[query]; 

    url = URLBuild[{endpoint, path}, encodedQuery]; 

    request = HTTPRequest[url, <|
        Method -> httpMethod, 
        "Headers" -> {"access_token" -> apiToken}, 
        "ContentType" -> "application/json"
    |>]; 

    response = URLRead[request]; 

    ImportString[response["Body"], "RawJSON"]
];


encode[query_?AssociationQ] := 
KeyValueMap[#1 -> encode[#2]&, DeleteCases[query, Automatic | None | Null | Nothing | {} | ""]]; 


encode[value: _String | _?NumericQ] := 
value; 


encode[date_DateObject] := 
DateString[date, "ISODateTimeMillisecond"] <> "Z"; 

encode[value_List] := 
StringRiffle[Map[encode, value], ","]; 


Options[WaexExchanges] = {
    "search" :> Automatic, 
    "limit" :> Automatic, 
    "offset" :> Automatic
};


SyntaxInformation[WaexExchanges] = {
    "ArgumentsPattern" -> {OptionsPattern[]}, 
    "OptionNames" -> {"\"search\"", "\"limit\"", "\"offset\""}
};


WaexExchanges[opts: OptionsPattern[{waexRequest, WaexExchanges}]] := 
waexRequest["/api/v1/crypto/exchanges", <|
    "search" -> OptionValue["search"], 
    "limit" -> OptionValue["limit"], 
    "offset" -> OptionValue["offset"]
|>, opts]; 


Options[WaexMarkets] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "tokenSymbols" :> Automatic, 
    "active" :> Automatic
};


SyntaxInformation[WaexMarkets] = {
    "ArgumentsPattern" -> {OptionsPattern[]}, 
    "OptionNames" -> {"\"limit\"", "\"offset\"", "\"exchangeNames\"", "\"tokenSymbols\"", "\"active\""}
};


WaexMarkets[opts: OptionsPattern[{waexRequest, WaexMarkets}]] := 
waexRequest["/api/v1/crypto/markets", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "tokenSymbols[]" -> OptionValue["tokenSymbols"], 
    "active" -> OptionValue["active"]
|>, opts]; 


Options[WaexTokens] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "names" :> Automatic, 
    "symbols" :> Automatic, 
    "active" :> Automatic, 
    "itins" :> Automatic
};


SyntaxInformation[WaexTokens] = {
    "ArgumentsPattern" -> {OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"names\"", "\"symbols\"", "\"active\"", "\"itins\""}
};


WaexTokens[opts: OptionsPattern[{waexRequest, WaexTokens}]] := 
waexRequest["/api/v1/crypto/tokens", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "names[]" -> OptionValue["names"], 
    "symbols[]" -> OptionValue["symbols"], 
    "active" -> OptionValue["active"], 
    "itins[]" -> OptionValue["itins"]
|>, opts]; 


Options[WaexOrderBooks] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "rangeDateStart" :> Automatic, 
    "rangeDateEnd" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "symbol" :> Automatic
};


SyntaxInformation[WaexOrderBooks] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\""}
};


WaexOrderBooks[opts: OptionsPattern[{waexRequest, WaexOrderBooks}]] := 
waexRequest["/api/v1/crypto/order-books", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "rangeDateStart" -> OptionValue["rangeDateStart"], 
    "rangeDateEnd" -> OptionValue["rangeDateEnd"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "symbol" -> OptionValue["symbol"]
|>, opts]; 


WaexOrderBooks[symbol_String, opts: OptionsPattern[{waexRequest, WaexOrderBooks}]] := 
WaexOrderBooks["symbol" -> symbol, opts]; 


Options[WaexTrades] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "rangeDateStart" :> Automatic, 
    "rangeDateEnd" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "symbol" :> Automatic
};


SyntaxInformation[WaexTrades] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\""}
};


WaexTrades[opts: OptionsPattern[{waexRequest, WaexTrades}]] := 
waexRequest["/api/v1/crypto/trades", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "rangeDateStart" -> OptionValue["rangeDateStart"], 
    "rangeDateEnd" -> OptionValue["rangeDateEnd"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "symbol" -> OptionValue["symbol"]
|>, opts]; 


WaexTrades[symbol_String, opts: OptionsPattern[{waexRequest, WaexTrades}]] := 
WaexTrades["symbol" -> symbol, opts]; 


Options[WaexTickers] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "rangeDateStart" :> Automatic, 
    "rangeDateEnd" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "symbol" :> Automatic
};


SyntaxInformation[WaexTickers] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\""}
};


WaexTickers[opts: OptionsPattern[{waexRequest, WaexTickers}]] := 
waexRequest["/api/v1/crypto/tickers", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "rangeDateStart" -> OptionValue["rangeDateStart"], 
    "rangeDateEnd" -> OptionValue["rangeDateEnd"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "symbol" -> OptionValue["symbol"]
|>, opts]; 


WaexTickers[symbol_String, opts: OptionsPattern[{waexRequest, WaexTickers}]] := 
WaexTickers["symbol" -> symbol, opts];


Options[WaexCandleSticks] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "rangeDateStart" :> Automatic, 
    "rangeDateEnd" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "symbol" :> Automatic
};


SyntaxInformation[WaexCandleSticks] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\""}
};


WaexCandleSticks[opts: OptionsPattern[{waexRequest, WaexCandleSticks}]] := 
waexRequest["/api/v1/crypto/candle-sticks", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "rangeDateStart" -> OptionValue["rangeDateStart"], 
    "rangeDateEnd" -> OptionValue["rangeDateEnd"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "symbol" -> OptionValue["symbol"]
|>, opts]; 


WaexCandleSticks[symbol_String, opts: OptionsPattern[{waexRequest, WaexCandleSticks}]] := 
WaexCandleSticks["symbol" -> symbol, opts]; 


End[];


EndPackage[];
