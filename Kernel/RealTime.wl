(* ::Package:: *)

BeginPackage["KirillBelov`WAEXLink`RealTime`", {
    "KirillBelov`WAEXLink`REST`", 
    "KirillBelov`SocketIOLink`"
}]; 


WAEXConnect::usage = 
"WAEXConnect[login, password] connect to WAEX Services and return socket io connection."; 


WAEXConnections::usage = 
"WAEXConnections[] returns list of all created connections."


WAEXSubscribe::usage =
"WAEXSubscribe[connection, emitter, payload] subscribes to the emitter.";


WAEXUnsubscribe::usage =
"WAEXUnsubscribe[connection, emitter, payload] unsubscribes from the emitter.";


WAEXSubscribeToAggregateOHLCV::usage =
"WAEXSubscribeToAggregateOHLCV[connection] subscribes to the AggregateOHLCV emitter.";


WAEXSubscribeToPricing::usage =
"WAEXSubscribeToPricing[connection, type, marketId] subscribes to the Pricing emitter.";


WAEXUnsubscribeFromAggregateOHLCV::usage =
"WAEXUnsubscribeFromAggregateOHLCV[connection] unsubscribes from the AggregateOHLCV emitter.";


WAEXUnsubscribeFromPricing::usage =
"WAEXUnsubscribeFromPricing[connection, type, marketId] unsubscribes from the Pricing emitter.";


Begin["`Private`"]; 


Options[WAEXConnect] = {
    "MessageHandler" -> Identity
};


WAEXConnect[login_String, password_String, OptionsPattern[]] := 
Module[{credentials, messageHandler = OptionValue["MessageHandler"]}, 
    WAEXLogin[login, password, "CreadentialHandler" -> Function[credentials = #]]; 
    
    With[{connection = getConnection[credentials]},  
        connection["MessageHandler"] = messageHandler; 
        connection["Subscribtions"] = {}; 

        (*Return*)
        connection
    ]
]; 


WAEXConnections[] := $connections; 


WAEXSubscribe[connection_SocketIOConnection, emitterName_String] :=
Module[{message = <|"emitterName" -> emitterName|>}, 
    connection["Subscribtions"] = Append[connection["Subscribtions"], message]; 
    SocketIOEmit[connection, "subscribe-to-emitter", message, Identity]
]; 


WAEXSubscribe[connection_SocketIOConnection, emitterName_String, payload_Association] :=
Module[{message = <|"emitterName" -> emitterName, "payload" -> payload|>}, 
    connection["Subscribtions"] = Append[connection["Subscribtions"], message]; 
    SocketIOEmit[connection, "subscribe-to-emitter", message, Identity]
]; 


WAEXUnsubscribe[connection_SocketIOConnection, emitterName_String] := 
Module[{message = <|"emitterName" -> emitterName|>}, 
    connection["Subscribtions"] = DeleteCases[connection["Subscribtions"], message]; 
    SocketIOEmit[connection, "unsubscribe-from-emitter", message, Identity]
]; 


WAEXUnsubscribe[connection_SocketIOConnection, emitterName_String, payload_Association] := 
Module[{message = <|"emitterName" -> emitterName, "payload" -> payload|>}, 
    connection["Subscribtions"] = DeleteCases[connection["Subscribtions"], message]; 
    SocketIOEmit[connection, "unsubscribe-from-emitter", message, Identity]
]; 


WAEXSubscribeToAggregateOHLCV[connection_SocketIOConnection] := 
WAEXSubscribe[connection, "AggregateOHLCVEmitter"]; 


WAEXUnsubscribeFromAggregateOHLCV[connection_SocketIOConnection] := 
WAEXUnsubscribe[connection, "AggregateOHLCVEmitter"]; 


WAEXSubscribeToPricing[connection_SocketIOConnection, type: "CandleStick" | "Trade" | "Ticker" | "OrderBook", marketId_Integer] := 
WAEXSubscribe[connection, 
    "PricingEmitter", 
    <|
        "type" -> type, 
        "marketId" -> marketId
    |>
]; 


WAEXUnsubscribeFromPricing[connection_SocketIOConnection, type: "CandleStick" | "Trade" | "Ticker" | "OrderBook", marketId_Integer] := 
WAEXUnsubscribe[connection, 
    "PricingEmitter", 
    <|
        "type" -> type, 
        "marketId" -> marketId
    |>
]; 


$waexEndpoint = "https://access.ccdb.waexservices.com"; 


If[!ListQ[$connections], $connections = {}]; 


getConnection[credentials_Association] := 
Block[{connected},  
    With[{
        data = CreateDataStructure["DynamicArray"], 
        connection = SocketIOConnect[$waexEndpoint, 
            "HTTPHeaders" -> credentials
        ]
    }, 
        connection["Data"] := Normal[data]; 
        connection["DataDynamicArray"] := data; 
        
        SocketIOListenAll[connection, 
            Function[
                data["Append", KeyDrop[#, "connection"]];
                connection["MessageHandler"][#];
            ]
        ];

        TimeConstrained[While[Not[connection @ connected[]], Pause[0.01]], 5]; 

        AppendTo[$connections, connection]; 

        (*Return*)
        connection 
    ]
]; 


End[]; 


EndPackage[]; 
