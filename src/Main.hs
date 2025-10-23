{-|
Module      : Main
Description : Energy Engineering Calculator - Web Interface
Copyright   : (c) 2025
License     : MIT

This module provides both command-line examples and a web interface
for energy engineering calculations.
-}

module Main where

import System.Environment (getArgs)
import qualified Web.Server as Web

-- Import our modules for command-line examples
import Energy.Conversions

-- | Main entry point - choose between web server or examples
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["web"] -> Web.startWebServer
        ["examples"] -> runExamples
        [] -> do
            putStrLn "Energy Engineering Calculator"
            putStrLn ""
            putStrLn "Choose mode:"
            putStrLn "  stack run energy-engineering web       - Start web interface"
            putStrLn "  stack run energy-engineering examples  - Run command-line examples"
            putStrLn ""
            putStrLn "Starting web interface by default..."
            Web.startWebServer
        _ -> do
            putStrLn "Usage: energy-engineering [web|examples]"

-- | Run command-line examples (original functionality)
runExamples :: IO ()
runExamples = do
    putStrLn "=== Energy Engineering Calculator ==="
    putStrLn ""
    
    -- Example 1: Unit Conversions
    putStrLn "--- Example 1: Unit Conversions ---"
    let energyKwh = 100
    putStrLn $ "Energy: " ++ show energyKwh ++ " kWh"
    putStrLn $ "  = " ++ show (kwhToJoules energyKwh) ++ " Joules"
    putStrLn $ "  = " ++ show (kwhToMJ energyKwh) ++ " MJ"
    putStrLn $ "  = " ++ show (kwhToBtu energyKwh) ++ " BTU"
    putStrLn ""
    
    -- Example 2: Temperature Conversions
    putStrLn "--- Example 2: Temperature Conversions ---"
    let tempC = 25.0
    putStrLn $ "Temperature: " ++ show tempC ++ "°C"
    putStrLn $ "  = " ++ show (celsiusToFahrenheit tempC) ++ "°F"
    putStrLn $ "  = " ++ show (celsiusToKelvin tempC) ++ " K"
    putStrLn ""
    
    putStrLn "=== All examples completed! ==="
    putStrLn ""
    putStrLn "Try loading this in GHCi:"
    putStrLn "  ghci src/Main.hs"
    putStrLn "Then run: main"
