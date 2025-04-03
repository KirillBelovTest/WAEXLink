(* ::Package:: *)

Once[
    Map[
        If[PacletFind[#] === {}, 
            PacletInstall[#]
        ]&, 
        {
            "KirillBelov/CSockets", 
            "KirillBelov/Objects", 
            "KirillBelov/LTP", 
            "KirillBelov/SocketIOLink"
        }
    ]
];


BeginPackage["KirillBelov`WAEXLink`"];


EndPackage[];


Get["KirillBelov`WAEXLink`REST`"];


Get["KirillBelov`WAEXLink`RealTime`"];
