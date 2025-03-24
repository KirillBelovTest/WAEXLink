(* ::Package:: *)

BeginPackage["KirillBelov`WAEXLink`REST`"];


WAEXLogin::usage = 
"WAEXLogin[login, password] login to WAEX."; 


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


$WAEXCredentials::usage = 
"User cookie and csrf token or access_token."; 


Begin["`Private`"];


(* :Internal: *)


$WAEXCredentials := Which[
    StringQ[SystemCredential["WAEX_ACCESS_TOKEN"]], 
        <|"access_token" -> SystemCredential["WAEX_ACCESS_TOKEN"]|>, 
    StringQ[SystemCredential["WAEX_LOGIN"]] && StringQ[SystemCredential["WAEX_PASSWORD"]], 
        $WAEXCredentials = Block[{$creds, $WAEXCredentials}, 
            WAEXLogin[SystemCredential["WAEX_LOGIN"], SystemCredential["WAEX_PASSWORD"], 
                "CreadentialHandler" -> Function[cred, $creds = cred], 
                "Credentials" -> <||>
            ];
            $creds
        ], 
    True,
        <||>
];


Options[WAEXRequest] = {
    "Endpoint" :> "https://access.ccdb.WAEXservices.com", 
    "Credentials" :> $WAEXCredentials, 
    "HTTPMethod" :> "GET", 
    "ResponseHandler" :> Identity, 
    "Logger" :> Identity
};


WAEXRequest[path_, query: _Association: <||>, body: _Association: <||>, opts: OptionsPattern[]] := 
Module[{
    endpoint = OptionValue[WAEXRequest, FilterRules[Flatten[{opts}], Options[WAEXRequest]], "Endpoint"], 
    credentials = OptionValue[WAEXRequest, FilterRules[Flatten[{opts}], Options[WAEXRequest]], "Credentials"], 
    httpMethod = OptionValue[WAEXRequest, FilterRules[Flatten[{opts}], Options[WAEXRequest]], "HTTPMethod"], 
    responseHandler = OptionValue[WAEXRequest, FilterRules[Flatten[{opts}], Options[WAEXRequest]], "ResponseHandler"], 
    request, response, 
    metadata, url, encodedQuery, 
    headers, 
    logger = OptionValue[WAEXRequest, FilterRules[Flatten[{opts}], Options[WAEXRequest]], "Logger"]
}, 
    encodedQuery = encode[query]; 

    url = URLBuild[{endpoint, path}, encodedQuery]; 

    logger[{"URL", url}];

    headers = Which[
        KeyExistsQ[credentials, "Cookie"] && KeyExistsQ[credentials, "csrf_token"],
            <|
                "Cookie" -> credentials["Cookie"], 
                "csrf_token" -> credentials["csrf_token"]
            |>,
        KeyExistsQ[credentials, "access_token"] && StringQ[credentials["access_token"]],
            <|"access_token" -> credentials["access_token"]|>, 
        True,
            <||>
    ];

    metadata = <|
        Method -> httpMethod, 
        "Headers" -> headers, 
        "ContentType" -> "application/json"
    |>; 

    If[Length[body] =!= 0, 
        metadata["Body"] = ExportString[body, "RawJSON"]; 
    ]; 

    request = HTTPRequest[url, metadata]; 

    response = URLRead[request]; 

    responseHandler[response]; 

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


encode[value: True | False] := 
ToLowerCase[ToString[value]]; 


getWAEXCredentials[response_HTTPResponse] := 
Module[{
    headers = response["Headers"], 
    body = response["Body"], 
    json
}, 
    cookie = Association[headers]["set-cookie"]; 
    json = ImportString[body, "RawJSON"];
    csrfToken = json["meta", "csrfToken"]; 

    <|"Cookie" -> cookie, "csrf_token" -> csrfToken|>
]; 


Options[WAEXLogin] = {
    "CreadentialHandler" -> Function[cred, $WAEXCredentials = cred]
}; 


WAEXLogin[login_String, password_String, opts: OptionsPattern[{WAEXLogin, WAEXRequest}]] := 
With[{creadentialHandler = OptionValue["CreadentialHandler"]}, 
    WAEXRequest["/api/v1/auth/local", <||>, <|"login" -> login, "password" -> password|>, 
        "HTTPMethod" -> "POST", 
        "ResponseHandler" -> creadentialHandler @* getWAEXCredentials, 
        opts
    ]
]; 


WAEXLogin[opts: OptionsPattern[{WAEXLogin, WAEXRequest}]] := 
WAEXLogin[SystemCredential["WAEX_LOGIN"], SystemCredential["WAEX_PASSWORD"], opts];


Options[WAEXExchanges] = {
    "search" :> Automatic, 
    "limit" :> Automatic, 
    "offset" :> Automatic
};


SyntaxInformation[WAEXExchanges] = {
    "ArgumentsPattern" -> {OptionsPattern[]}, 
    "OptionNames" -> {"\"search\"", "\"limit\"", "\"offset\"", "\"Credentials\"", "\"Endpoint\""}
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
    "OptionNames" -> {"\"limit\"", "\"offset\"", "\"exchangeNames\"", "\"tokenSymbols\"", "\"active\"", "\"Credentials\"", "\"Endpoint\""}
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
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"names\"", "\"symbols\"", "\"active\"", "\"itins\"", "\"Credentials\"", "\"Endpoint\""}
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
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\"", "\"Credentials\"", "\"Endpoint\""}
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
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\"", "\"Credentials\"", "\"Endpoint\""}
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
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\"", "\"Credentials\"", "\"Endpoint\""}
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
    "OptionNames" -> {"\"offset\"", "\"limit\"", "\"rangeDateStart\"", "\"rangeDateEnd\"", "\"exchangeNames\"", "\"symbol\"", "\"Credentials\"", "\"Endpoint\""}
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
