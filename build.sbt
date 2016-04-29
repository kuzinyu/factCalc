name := "factCalc"

version := "1.0"

scalaVersion := "2.10.2"

val factCalcMain = "com.yk.factcalc.main.FactCalc"

mainClass in (Compile,run) := Some(factCalcMain)