(* ::Package:: *)

BeginPackage["KirillBelov`WAEXLink`RealTime`", {
    "KirillBelov`WAEXLink`REST`", 
    "KirillBelov`SocketIOLink`"
}]; 


$WAEXSocketIO::usage = 
"Socket.IO connection singleton.";  


WAEXStreamAggregateOHLCV::usage = 
"WAEXStreamAggregateOHLCV[] subsribe to OHLCV events."; 


Begin["`Private`"]; 


$WAEXSocketIO := 
getSocketIOObject[]; 


WAEXStreamAggregateOHLCV[] := 
With[{socketObj = getSocketIOObject[]}, 
    SocketIOListen[socketObj, "AggregateOHLCV"]; 
    SocketIOEmit[socketObj, "subscribe-to-emitter", 
	    <|"emitterName" -> "AggregateOHLCVEmitter"|>
    ];
    socketObj
]; 


$socketIOObject = Null; 


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
