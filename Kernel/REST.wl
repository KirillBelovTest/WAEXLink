(* :Package: *)

BeginPackage["KirillBelov`WAEXLink`REST`"];


WAEXExchanges::usage = 
"WAEXExchanges[] - returns list of available exchanges."; 


WAEXMarkets::usage = 
"WAEXMarkets[] - returns list of available markets."; 


WAEXTokens::usage = 
"WAEXTokens[] - returns list of available tokens.";


WAEXOrderBooks::usage = 
"WAEXOrderBooks[pair] - order books for a given pair.";


WAEXTrades::usage = 
"WAEXTrades[pair] - trades for a given pair.";


WAEXTickers::usage = 
"WAEXTickers[pair] - tickers for a given pair.";


WAEXCandleSticks::usage = 
"WAEXCandleSticks[pair] - candle sticks for a given pair.";


Begin["`Private`"];


(* :Internal: *)


Options[WAEXRequest] = {
    "Endpoint" :> "https://access.ccdb.WAEXservices.com", 
    "APIToken" :> SystemCredential["WAEX_API_TOKEN"], 
    "HTTPMethod" :> "GET"
};


WAEXRequest[path_, query: _Association: <||>, opts: OptionsPattern[{}]] := 
Module[{
    endpoint = OptionValue[WAEXRequest, FilterRules[Flatten[{opts}], Options[WAEXRequest]], "Endpoint"], 
    apiToken = OptionValue[WAEXRequest, FilterRules[Flatten[{opts}], Options[WAEXRequest]], "APIToken"], 
    httpMethod = OptionValue[WAEXRequest, FilterRules[Flatten[{opts}], Options[WAEXRequest]], "HTTPMethod"]
}, 
    encodedQuery = encode[query]; 

    url = URLBuild[{endpoint, path}, encodedQuery]; 

    request = HTTPRequest[url, <|
        Method -> httpMethod, 
        "Headers" -> {"access_token" -> apiToken}, 
        "ContentType" -> "application/json"
    |>]; 

    Global`$response = response = URLRead[request]; 

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


Options[WAEXExchanges] = {
    "search" :> Automatic, 
    "limit" :> Automatic, 
    "offset" :> Automatic
};


SyntaxInformation[WAEXExchanges] = {
    "ArgumentsPattern" -> {OptionsPattern[]}, 
    "OptionNames" -> {"\"search\"", "\"limit\"", "\"offset\""}
};


WAEXExchanges[opts: OptionsPattern[{WAEXRequest, WAEXExchanges}]] := 
WAEXRequest["/api/v1/crypto/exchanges", <|
    "search" -> OptionValue["search"], 
    "limit" -> OptionValue["limit"], 
    "offset" -> OptionValue["offset"]
|>, opts]; 


Options[WAEXMarkets] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "tokenSymbols" :> Automatic, 
    "active" :> Automatic
};


SyntaxInformation[WAEXMarkets] = {
    "ArgumentsPattern" -> {OptionsPattern[]}, 
    "OptionNames" -> {"\"limit\"", "\"offset\"", "\"exchangeNames\"", "\"tokenSymbols\"", "\"active\""}
};


WAEXMarkets[opts: OptionsPattern[{WAEXRequest, WAEXMarkets}]] := 
WAEXRequest["/api/v1/crypto/markets", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "tokenSymbols[]" -> OptionValue["tokenSymbols"], 
    "active" -> OptionValue["active"]
|>, opts]; 


Options[WAEXTokens] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "names" :> Automatic, 
    "symbols" :> Automatic, 
    "active" :> Automatic, 
    "itins" :> Automatic
};


SyntaxInformation[WAEXTokens] = {
    "ArgumentsPattern" -> {OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"names\"", "\"symbols\"", "\"active\"", "\"itins\""}
};


WAEXTokens[opts: OptionsPattern[{WAEXRequest, WAEXTokens}]] := 
WAEXRequest["/api/v1/crypto/tokens", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "names[]" -> OptionValue["names"], 
    "symbols[]" -> OptionValue["symbols"], 
    "active" -> OptionValue["active"], 
    "itins[]" -> OptionValue["itins"]
|>, opts]; 


Options[WAEXOrderBooks] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "rangeDateStart" :> Automatic, 
    "rangeDateEnd" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "symbol" :> Automatic
};


SyntaxInformation[WAEXOrderBooks] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\""}
};


WAEXOrderBooks[opts: OptionsPattern[{WAEXRequest, WAEXOrderBooks}]] := 
WAEXRequest["/api/v1/crypto/order-books", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "rangeDateStart" -> OptionValue["rangeDateStart"], 
    "rangeDateEnd" -> OptionValue["rangeDateEnd"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "symbol" -> OptionValue["symbol"]
|>, opts]; 


WAEXOrderBooks[symbol_String, opts: OptionsPattern[{WAEXRequest, WAEXOrderBooks}]] := 
WAEXOrderBooks["symbol" -> symbol, opts]; 


Options[WAEXTrades] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "rangeDateStart" :> Automatic, 
    "rangeDateEnd" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "symbol" :> Automatic
};


SyntaxInformation[WAEXTrades] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\""}
};


WAEXTrades[opts: OptionsPattern[{WAEXRequest, WAEXTrades}]] := 
WAEXRequest["/api/v1/crypto/trades", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "rangeDateStart" -> OptionValue["rangeDateStart"], 
    "rangeDateEnd" -> OptionValue["rangeDateEnd"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "symbol" -> OptionValue["symbol"]
|>, opts]; 


WAEXTrades[symbol_String, opts: OptionsPattern[{WAEXRequest, WAEXTrades}]] := 
WAEXTrades["symbol" -> symbol, opts]; 


Options[WAEXTickers] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "rangeDateStart" :> Automatic, 
    "rangeDateEnd" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "symbol" :> Automatic
};


SyntaxInformation[WAEXTickers] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\""}
};


WAEXTickers[opts: OptionsPattern[{WAEXRequest, WAEXTickers}]] := 
WAEXRequest["/api/v1/crypto/tickers", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "rangeDateStart" -> OptionValue["rangeDateStart"], 
    "rangeDateEnd" -> OptionValue["rangeDateEnd"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "symbol" -> OptionValue["symbol"]
|>, opts]; 


WAEXTickers[symbol_String, opts: OptionsPattern[{WAEXRequest, WAEXTickers}]] := 
WAEXTickers["symbol" -> symbol, opts];


Options[WAEXCandleSticks] = {
    "offset" :> Automatic, 
    "limit" :> Automatic, 
    "rangeDateStart" :> Automatic, 
    "rangeDateEnd" :> Automatic, 
    "exchangeNames" :> Automatic, 
    "symbol" :> Automatic
};


SyntaxInformation[WAEXCandleSticks] = {
    "ArgumentsPattern" -> {_., OptionsPattern[]}, 
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\""}
};


WAEXCandleSticks[opts: OptionsPattern[{WAEXRequest, WAEXCandleSticks}]] := 
WAEXRequest["/api/v1/crypto/candle-sticks", <|
    "offset" -> OptionValue["offset"], 
    "limit" -> OptionValue["limit"], 
    "rangeDateStart" -> OptionValue["rangeDateStart"], 
    "rangeDateEnd" -> OptionValue["rangeDateEnd"], 
    "exchangeNames[]" -> OptionValue["exchangeNames"], 
    "symbol" -> OptionValue["symbol"]
|>, opts]; 


WAEXCandleSticks[symbol_String, opts: OptionsPattern[{WAEXRequest, WAEXCandleSticks}]] := 
WAEXCandleSticks["symbol" -> symbol, opts]; 


End[];


EndPackage[];
