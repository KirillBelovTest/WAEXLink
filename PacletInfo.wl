(* ::Package:: *)

PacletObject[
  <|
    "Name" -> "KirillBelov/WAEXLink",
    "Description" -> "Wolfram Language client for WAEX API",
    "Creator" -> "Kirill Belov",
    "License" -> "MIT",
    "PublisherID" -> "KirillBelov",
    "Version" -> "1.0.5",
    "WolframVersion" -> "14+",
    "PrimaryContext" -> "KirillBelov`WAEXLink`",
    "DocumentationURL" -> "https://resources.wolframcloud.com/PacletRepository/resources",
    "Extensions" -> {
      {
        "Kernel",
        "Root" -> "Kernel",
        "Context" -> {
          {"KirillBelov`WAEXLink`", "WAEXLink.wl"},
          {"KirillBelov`WAEXLink`REST`", "REST.wl"},
          {
            "KirillBelov`WAEXLink`RealTime`",
            "RealTime.wl"
          }
        }
      },
      {
        "Documentation",
        "Root" -> "Documentation",
        "Language" -> "English"
      },
      {"Asset", "Assets" -> {{"ReadMe", "README.md"}}}
    }
  |>
]
