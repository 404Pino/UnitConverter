#!/usr/bin/env stack
-- stack script --resolver lts-21.25

{-# LANGUAGE OverloadedStrings #-}

import Energy.Conversions
import Control.Monad (when)
import Data.Time
import Text.Printf

main :: IO ()
main = do
    startTime <- getCurrentTime
    putStrLn "=== Testing All Unit Conversions ==="
    putStrLn $ "Started at: " ++ show startTime
    putStrLn ""
    
    let allCategories = [minBound..maxBound] :: [UnitCategory]
    
    putStrLn $ "Found " ++ show (length allCategories) ++ " unit categories to test"
    putStrLn "Progress: [Category Details]"
    putStrLn ""
    
    -- Test each category with progress indicators
    mapM_ (testCategoryWithProgress (length allCategories)) (zip [1..] allCategories)
    
    putStrLn ""
    putStrLn "=== Comprehensive Testing Phase ==="
    results <- testAllConversionsWithProgress allCategories
    
    putStrLn ""
    putStrLn "=== Final Summary ==="
    mapM_ printCategorySummary results
    
    endTime <- getCurrentTime
    let duration = diffUTCTime endTime startTime
    putStrLn ""
    putStrLn $ "Testing completed at: " ++ show endTime
    putStrLn $ "Total duration: " ++ printf "%.2f" (realToFrac duration :: Double) ++ " seconds"

testCategoryWithProgress :: Int -> (Int, UnitCategory) -> IO ()
testCategoryWithProgress totalCategories (currentIndex, category) = do
    let progress = printf "[%d/%d]" currentIndex totalCategories
    putStrLn $ progress ++ " Testing " ++ show category ++ "..."
    
    let units = getUnitsByCategory category
    putStrLn $ "  " ++ show (length units) ++ " units available:"
    mapM_ (\u -> putStrLn $ "    " ++ unitSymbol u ++ " - " ++ unitName u) units
    
    -- Test sample conversions with progress
    case units of
        (u1:u2:_) -> do
            let testValue = 100.0
            putStr $ "  Testing sample conversion... "
            case convert testValue (unitSymbol u1) (unitSymbol u2) of
                Just result -> do
                    putStrLn "OK"
                    putStrLn $ "    " ++ show testValue ++ " " ++ unitSymbol u1 ++ " = " ++ 
                              printf "%.6f" (convertedValue result) ++ " " ++ unitSymbol u2
                Nothing -> do
                    putStrLn "FAILED"
                    putStrLn $ "    Error: Cannot convert " ++ unitSymbol u1 ++ " to " ++ unitSymbol u2
        _ -> putStrLn "  (!) Not enough units for sample testing"
    
    putStrLn ""

testCategory :: UnitCategory -> IO ()
testCategory category = do
    putStrLn $ "--- " ++ show category ++ " ---"
    let units = getUnitsByCategory category
    putStrLn $ "Available units: " ++ show (length units)
    mapM_ (\u -> putStrLn $ "  " ++ unitSymbol u ++ " - " ++ unitName u) units
    
    -- Test a few sample conversions
    case units of
        (u1:u2:_) -> do
            let testValue = 100.0
            case convert testValue (unitSymbol u1) (unitSymbol u2) of
                Just result -> putStrLn $ "  Sample: " ++ show testValue ++ " " ++ unitSymbol u1 ++ " = " ++ 
                              show (convertedValue result) ++ " " ++ unitSymbol u2
                Nothing -> putStrLn $ "  Error: Cannot convert " ++ unitSymbol u1 ++ " to " ++ unitSymbol u2
        _ -> putStrLn "  Not enough units for testing"
    putStrLn ""

testAllConversionsWithProgress :: [UnitCategory] -> IO [(UnitCategory, [(String, String, Bool)])]
testAllConversionsWithProgress categories = do
    putStrLn "Running comprehensive conversion tests for all unit combinations..."
    results <- mapM (testCategoryConversionsWithProgress (length categories)) (zip [1..] categories)
    return results

testCategoryConversionsWithProgress :: Int -> (Int, UnitCategory) -> IO (UnitCategory, [(String, String, Bool)])
testCategoryConversionsWithProgress totalCategories (currentIndex, category) = do
    let progress = printf "[%d/%d]" currentIndex totalCategories
    putStr $ progress ++ " Testing all " ++ show category ++ " conversions... "
    
    let units = getUnitsByCategory category
        conversions = [(u1, u2) | u1 <- units, u2 <- units, u1 /= u2]
        testValue = 100.0
        totalConversions = length conversions
    
    putStrLn $ "(" ++ show totalConversions ++ " combinations)"
    
    results <- mapM (\(u1, u2) -> do
        let from = unitSymbol u1
            to = unitSymbol u2
            result = convert testValue from to
        case result of
            Just _ -> return (from, to, True)
            Nothing -> return (from, to, False)
        ) conversions
    
    let successful = length $ filter (\(_, _, success) -> success) results
    putStrLn $ "    [OK] " ++ show successful ++ "/" ++ show totalConversions ++ " conversions successful"
    
    return (category, results)

testAllConversions :: [UnitCategory] -> IO [(UnitCategory, [(String, String, Bool)])]
testAllConversions categories = do
    results <- mapM testCategoryConversions categories
    return results

testCategoryConversions :: UnitCategory -> IO (UnitCategory, [(String, String, Bool)])
testCategoryConversions category = do
    let units = getUnitsByCategory category
        conversions = [(u1, u2) | u1 <- units, u2 <- units, u1 /= u2]
        testValue = 100.0
    
    results <- mapM (\(u1, u2) -> do
        let from = unitSymbol u1
            to = unitSymbol u2
            result = convert testValue from to
        case result of
            Just _ -> return (from, to, True)
            Nothing -> return (from, to, False)
        ) conversions
    
    return (category, results)

printCategorySummary :: (UnitCategory, [(String, String, Bool)]) -> IO ()
printCategorySummary (category, results) = do
    let total = length results
        successful = length $ filter (\(_, _, success) -> success) results
        failed = total - successful
        successRate = if total > 0 then (fromIntegral successful / fromIntegral total) * 100 else 0
        
    putStrLn $ printf "%-12s: %3d/%-3d conversions (%5.1f%% success)" 
               (show category) successful total (successRate :: Double)
    
    when (failed > 0) $ do
        putStrLn $ "  Failed conversions (showing first " ++ show (min 5 failed) ++ "):"
        mapM_ (\(from, to, _) -> putStrLn $ "    [X] " ++ from ++ " -> " ++ to) 
              $ take 5 $ filter (\(_, _, success) -> not success) results