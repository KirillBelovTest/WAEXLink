(* ::Package:: *)

BeginPackage["KirillBelov`WAEXLink`RealTime`", {
    "KirillBelov`WAEXLink`REST`", 
    "KirillBelov`SocketIOLink`"
}]; 


$WAEXConnection::usage = 
"Socket.IO connection singleton to waexservices.";  


WAEXSubscribe::usage =
"WAEXSubscribe[emitter, payload] subscribes to the emitter.";


WAEXUnsubscribe::usage =
"WAEXUnsubscribe[emitter, payload] unsubscribes from the emitter.";


Begin["`Private`"]; 


$WAEXConnection := getConnection[]; 


WAEXSubscribe[emitterName_String, payload: _Association: None] :=
SocketIOEmit[$WAEXConnection, "subscribe-to-emitter", 
    <|"emitterName" -> emitterName, "payload" -> payload|>
];    


WAEXEmitterUnsubscribe[emitterName_String, payload_Association] := 
SocketIOEmit[$WAEXConnection, "unsubscribe-from-emitter", 
    <|"emitterName" -> emitterName, "payload" -> payload|>
];


$connection = Null; 


$waexEndpoint = "https://access.ccdb.waexservices.com"; 


getConnection[] := 
Block[{connected}, 
    If[
        $connection === Null || 
        And[
            Heed[$connection] === SocketIOObject, 
            Not[$connection["JavaIOSocket"] @ connected[]]
        ], 

        $connection = SocketIOConnect[$waexEndpoint, 
            "HTTPHeaders" -> $WAEXCredentials
        ]; 

        With[{data = CreateDataStructure["DynamicArray"]}, 
            $connection["Data"] := Normal[data]; 
            $connection["DataDynamicArray"] := data; 
            SocketIOListenAll[$connection, data["Append", KeyDrop[#, "connection"]]&]; 
        ]; 
    ]; 

    TimeConstrained[While[Not[$connection @ connected[]], Pause[0.01]], 5]; 

    $connection
]; 


End[]; 


EndPackage[]; 
