{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.API
Description : REST API endpoints for energy engineering calculations
Copyright   : (c) 2025
License     : MIT

This module defines the REST API endpoints for the web application.
-}

module Web.API
    ( apiRoutes
    ) where

import Web.Scotty
import Data.Aeson (object, (.=), Value(..), (.:))
import Data.Aeson.Types (parseMaybe, Parser)
import qualified Data.Aeson as JSON
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status (status400, status404, status500)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T

-- Import our calculation modules
import qualified Energy.Conversions as Conv

-- | Define all API routes
apiRoutes :: ScottyM ()
apiRoutes = do
    
    -- API documentation endpoint
    get "/api" $ do
        json $ object
            [ "name" .= ("Energy Engineering Calculator API" :: String)
            , "version" .= ("1.0.0" :: String)
            , "description" .= ("REST API for energy engineering calculations" :: String)
            , "endpoints" .= object
                [ "conversions" .= object
                    [ "universal-convert" .= ("POST /api/convert" :: String)
                    , "multiple-convert" .= ("POST /api/convert-multiple" :: String)
                    , "list-units" .= ("/api/units/{category}" :: String)
                    , "search-units" .= ("/api/units/search?q={query}" :: String)
                    , "categories" .= ("/api/categories" :: String)
                    ]
                ]
            ]
    
    -- Universal Conversion Endpoints
    
    -- Convert from one unit to another
    post "/api/convert" $ do
        requestData <- jsonData :: ActionM Value
        case parseMaybe parseConvertRequest requestData of
            Nothing -> do
                status status400
                json $ object ["error" .= ("Invalid request format" :: String)]
            Just (value, fromUnit, toUnit) -> do
                case Conv.convert value fromUnit toUnit of
                    Nothing -> do
                        status status400
                        json $ object ["error" .= ("Invalid conversion or units not found" :: String)]
                    Just result -> do
                        json $ object 
                            [ "input" .= object 
                                [ "value" .= Conv.originalValue result
                                , "unit" .= Conv.unitSymbol (Conv.originalUnit result)
                                , "unit_name" .= Conv.unitName (Conv.originalUnit result)
                                ]
                            , "output" .= object
                                [ "value" .= Conv.convertedValue result
                                , "unit" .= Conv.unitSymbol (Conv.convertedUnit result)
                                , "unit_name" .= Conv.unitName (Conv.convertedUnit result)
                                ]
                            , "conversion_factor" .= Conv.conversionFactor result
                            , "formula" .= (fromUnit ++ " × " ++ show (Conv.conversionFactor result) ++ " = " ++ toUnit)
                            ]
    
    -- Convert to multiple units
    post "/api/convert-multiple" $ do
        requestData <- jsonData :: ActionM Value
        case parseMaybe parseConvertMultipleRequest requestData of
            Nothing -> do
                status status400
                json $ object ["error" .= ("Invalid request format" :: String)]
            Just (value, fromUnit, toUnits) -> do
                let results = Conv.convertMultiple value fromUnit toUnits
                if null results
                    then do
                        status status400
                        json $ object ["error" .= ("No valid conversions found" :: String)]
                    else do
                        json $ object 
                            [ "input" .= object 
                                [ "value" .= value
                                , "unit" .= fromUnit
                                ]
                            , "conversions" .= map (\r -> object
                                [ "value" .= Conv.convertedValue r
                                , "unit" .= Conv.unitSymbol (Conv.convertedUnit r)
                                , "unit_name" .= Conv.unitName (Conv.convertedUnit r)
                                , "factor" .= Conv.conversionFactor r
                                ]) results
                            ]
    
    -- Get units by category
    get "/api/units/:category" $ do
        category <- param "category"
        case reads category of
            [(cat, "")] -> do
                let units = Conv.getUnitsByCategory cat
                json $ object 
                    [ "category" .= show cat
                    , "units" .= map (\u -> object
                        [ "symbol" .= Conv.unitSymbol u
                        , "name" .= Conv.unitName u
                        , "description" .= Conv.unitDescription u
                        ]) units
                    ]
            _ -> do
                status status400
                json $ object ["error" .= ("Invalid category" :: String)]
    
    -- Search units
    get "/api/units/search" $ do
        query <- param "q"
        let matchingUnits = filter (\u -> 
                query `isInfixOf` Conv.unitName u || 
                query `isInfixOf` Conv.unitSymbol u ||
                query `isInfixOf` Conv.unitDescription u) Conv.allUnits
        json $ object 
            [ "query" .= query
            , "results" .= map (\u -> object
                [ "symbol" .= Conv.unitSymbol u
                , "name" .= Conv.unitName u
                , "category" .= show (Conv.unitCategory u)
                , "description" .= Conv.unitDescription u
                ]) matchingUnits
            ]
    
    -- Get all available categories
    get "/api/categories" $ do
        json $ object 
            [ "categories" .= map (\cat -> object
                [ "name" .= show cat
                , "unit_count" .= length (Conv.getUnitsByCategory cat)
                ]) [minBound..maxBound :: Conv.UnitCategory]
            ]
    
    -- Error handling for invalid routes
    notFound $ do
        status status404
        json $ object 
            [ "error" .= ("Endpoint not found" :: String)
            , "message" .= ("Please check the API documentation at /api" :: String)
            ]

-- JSON parsing helper functions
parseConvertRequest :: Value -> Parser (Double, String, String)
parseConvertRequest = JSON.withObject "ConvertRequest" $ \o -> do
    value <- o .: "value"
    fromUnit <- o .: "from_unit"
    toUnit <- o .: "to_unit"
    return (value, T.unpack fromUnit, T.unpack toUnit)

parseConvertMultipleRequest :: Value -> Parser (Double, String, [String])
parseConvertMultipleRequest = JSON.withObject "ConvertMultipleRequest" $ \o -> do
    value <- o .: "value"
    fromUnit <- o .: "from_unit"
    toUnits <- o .: "to_units"
    return (value, T.unpack fromUnit, map T.unpack toUnits)