(* ::Package:: *)

BeginPackage["KirillBelov`WAEXLink`RealTime`", {
    "KirillBelov`WAEXLink`REST`", 
    "KirillBelov`SocketIOLink`"
}]; 


$WAEXSocketIO::usage = 
"Socket.IO connection singleton.";  


WAEXEmitterSubscribe::usage =
"WAEXEmitterSubscribe[emitter, payload] subscribes to the WAEX emitter.";


WAEXEmitterUnsubscribe::usage =
"WAEXEmitterUnsubscribe[emitter, payload] unsubscribes from the WAEX emitter.";


WAEXSubscribeToAggregateOHLCV::usage = 
"WAEXSubscribeToAggregateOHLCV[] collecting OHLCV events."; 


Begin["`Private`"]; 


$WAEXSocketIO := 
getSocketIOObject[]; 


If[!AssociationQ[$WAEXSubscriptions], 
    $WAEXSubscriptions = <||>
]; 


WAEXSubscribeToAggregateOHLCV[] := 
With[{socketObj = getSocketIOObject[]}, 
    SocketIOListen[socketObj, "AggregateOHLCV"]; 
    SocketIOEmit[socketObj, "subscribe-to-emitter", 
	    <|"emitterName" -> "AggregateOHLCVEmitter"|>
    ];
    socketObj
]; 


WAEXEmitterSubscribe[emitterName_String, payload: _Association: None] :=
With[{socketObj = getSocketIOObject[]}, 
    SocketIOListen[socketObj, emitterName]; 
    SocketIOEmit[socketObj, "subscribe-to-emitter", 
        If[AssociationQ[payload], 
            <|"emitterName" -> emitterName, "payload" -> payload|>, 
        (*Else*)
            <|"emitterName" -> emitterName|>
        ]
    ];
    socketObj
];


WAEXEmitterUnsubscribe[emitterName_String, payload_Association] := 
With[{socketObj = getSocketIOObject[]}, 
    SocketIOEmit[socketObj, "unsubscribe-to-emitter", 
        <|"emitterName" -> emitterName, "payload" -> payload|>
    ];
    socketObj
];


$socketIOObject = 
Null; 


$waexEndpoint = 
"https://access.ccdb.waexservices.com"; 


getSocketIOObject[] := 
Block[{connected}, 
    If[
        $socketIOObject === Null || 
        And[
            Heed[$socketIOObject] === SocketIOObject, 
            Not[$socketIOObject["JavaIOSocket"] @ connected[]]
        ], 

        $socketIOObject = SocketIOConnect[$waexEndpoint, 
            "HTTPHeaders" -> $WAEXCredentials
        ]; 

        With[{
            data = CreateDataStructure["DynamicArray"]
        }, 
            $socketIOObject["Data"] := Normal[data]; 
            
            $socketIOObject["Handler"] = Function[
                data["Append", ImportString[#, "RawJSON"]]
            ]; 
        ]; 
    ]; 

    TimeConstrained[While[Not[$socketIOObject @ connected[]], Pause[0.01]], 5]; 

    $socketIOObject
]; 


End[]; 


EndPackage[]; 
