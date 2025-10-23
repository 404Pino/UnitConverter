{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Web.Views
Description : HTML views for the energy engineering calculator
Copyright   : (c) 2025
License     : MIT

This module contains the HTML templates and view functions for the web interface.
-}

module Web.Views
    ( mainPage
    , errorPage
    ) where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

-- | Main application page
mainPage :: Html
mainPage = docTypeHtml $ do
    H.head $ do
        H.title "Energy Engineering Calculator"
        meta ! A.charset "utf-8"
        meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
        meta ! A.name "description" ! A.content "Professional energy engineering calculations with Haskell"
        meta ! A.name "keywords" ! A.content "energy, engineering, calculator, haskell, solar, efficiency"
        
        -- External CSS/JS
        link ! A.rel "stylesheet" ! A.href "/style.css"
        script ! A.src "https://cdn.jsdelivr.net/npm/chart.js" $ ""
        
        -- Favicon
        link ! A.rel "icon" ! A.type_ "image/x-icon" ! A.href "/favicon.ico"
    
    body $ do
        -- Header Section
        H.header ! A.class_ "header" $ do
            h1 "🔋 Energy Engineering Calculator"
            p "Professional energy calculations with Haskell"
        
        -- Main Content
        H.main ! A.class_ "container" $ do
            
            -- Unit Conversions Section  
            section ! A.class_ "card slide-up" $ do
                h2 "⚡ Universal Unit Converter"
                p ! A.class_ "text-muted" $ "Convert between any engineering units"
                
                H.div ! A.class_ "converter-interface" $ do
                    -- Input Section
                    H.div ! A.class_ "converter-input" $ do
                        h3 "From"
                        H.div ! A.class_ "input-group" $ do
                            input ! A.type_ "number" 
                                  ! A.id "input-value" 
                                  ! A.placeholder "Enter value" 
                                  ! A.step "any"
                                  ! A.value "1"
                            select ! A.id "from-category" ! A.class_ "unit-category" $ do
                                option ! A.value "" $ "Select Category"
                                option ! A.value "Energy" $ "Energy"
                                option ! A.value "Power" $ "Power"
                                option ! A.value "Temperature" $ "Temperature"
                                option ! A.value "Pressure" $ "Pressure"
                                option ! A.value "Length" $ "Length"
                                option ! A.value "Area" $ "Area"
                                option ! A.value "Volume" $ "Volume"
                                option ! A.value "Mass" $ "Mass"
                                option ! A.value "Force" $ "Force"
                                option ! A.value "Velocity" $ "Velocity"
                                option ! A.value "Flow" $ "Flow Rate"
                                option ! A.value "Density" $ "Density"
                                option ! A.value "Frequency" $ "Frequency"
                                option ! A.value "Angle" $ "Angle"
                            select ! A.id "from-unit" ! A.class_ "unit-selector" ! A.disabled "" $ do
                                option ! A.value "" $ "Select unit first"
                    
                    -- Output Section
                    H.div ! A.class_ "converter-output" $ do
                        h3 "To"
                        H.div ! A.class_ "input-group" $ do
                            select ! A.id "to-category" ! A.class_ "unit-category" ! A.disabled "" $ do
                                option ! A.value "" $ "Select From Category First"
                            select ! A.id "to-unit" ! A.class_ "unit-selector" ! A.disabled "" $ do
                                option ! A.value "" $ "Select unit first"
                
                -- Convert Button
                H.div ! A.class_ "converter-button" $ do
                    button ! A.id "convert-button" ! A.class_ "convert-btn" ! A.onclick "performConversion()" $ "Convert"
                
                -- Results Section
                H.div ! A.id "conversion-results" ! A.class_ "conversion-results hidden" $ do
                    h3 "Conversion Result"
                    H.div ! A.class_ "result-display" $ do
                        H.div ! A.id "main-result" ! A.class_ "main-result" $ ""
                        H.div ! A.id "conversion-details" ! A.class_ "conversion-details" $ do
                            p ! A.class_ "conversion-formula" $ ""
                            p ! A.class_ "conversion-factor" $ ""
        
        -- Footer
        footer $ do
            p $ do
                "Built with Haskell 💚 | "
                a ! A.href "https://github.com/yourusername/energy-engineering-haskell" $ "Source Code"
                " | "
                a ! A.href "/api" $ "API Documentation"
        
        -- JavaScript
        script ! A.src "/script.js" $ ""

-- | Error page template
errorPage :: String -> String -> Html
errorPage title message = docTypeHtml $ do
    H.head $ do
        H.title $ toHtml $ "Error - " ++ title
        meta ! A.charset "utf-8"
        meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
        link ! A.rel "stylesheet" ! A.href "/style.css"
    
    body $ do
        H.header ! A.class_ "header" $ do
            h1 "⚠️ Error"
            p $ toHtml title
        
        H.main ! A.class_ "container" $ do
            section ! A.class_ "card error" $ do
                h2 "Something went wrong"
                p $ toHtml message
                H.div ! A.class_ "mt-3" $ do
                    a ! A.href "/" ! A.class_ "button" $ "Return Home"
        
        footer $ do
            p "Energy Engineering Calculator"