{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Server
Description : Main web server for energy engineering calculator
Copyright   : (c) 2025
License     : MIT

This module provides the main web server functionality, bringing together
the API routes and HTML views in a clean, organized structure.
-}

module Web.Server 
    ( startWebServer
    , app
    ) where

import Web.Scotty
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Network.Wai.Middleware.Static
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Aeson (object, (.=))
import Network.HTTP.Types (status404, status500)

-- Import our modules
import qualified Web.API as API
import qualified Web.Views as Views

-- | Start the web server on port 3000
startWebServer :: IO ()
startWebServer = do
    putStrLn ""
    putStrLn "Starting Energy Engineering Web Calculator..."
    putStrLn "Visit: http://localhost:3000"
    putStrLn "API docs: http://localhost:3000/api"
    putStrLn "Press Ctrl+C to stop"
    putStrLn ""
    scotty 3000 app

-- | Main application configuration
app :: ScottyM ()
app = do
    -- Add request logging for development
    middleware logStdoutDev
    
    -- Serve static files (CSS, JS, images)
    middleware $ staticPolicy (noDots >-> addBase "static")
    
    -- Main HTML page
    get "/" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
        html $ renderHtml Views.mainPage
    
    -- Health check endpoint
    get "/health" $ do
        json $ object 
            [ "status" .= ("healthy" :: String)
            , "service" .= ("energy-engineering-calculator" :: String)
            , "version" .= ("1.0.0" :: String)
            ]
    
    -- Include all API routes
    API.apiRoutes
    
    -- 404 handler for non-API routes
    notFound $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        status status404
        html $ renderHtml $ Views.errorPage "Page Not Found" "The requested page could not be found."
    
    -- Global error handler
    defaultHandler $ \e -> do
        liftIO $ putStrLn $ "Error: " ++ show e
        setHeader "Content-Type" "text/html; charset=utf-8"
        status status500
        html $ renderHtml $ Views.errorPage "Internal Server Error" "An unexpected error occurred. Please try again later."