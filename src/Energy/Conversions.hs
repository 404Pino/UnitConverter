{-|
Module      : Energy.Conversions
Description : Comprehensive unit conversion system for engineering applications
Copyright   : (c) 2025
License     : MIT

This module provides a comprehensive unit conversion system covering all major
engineering disciplines. It supports energy, power, temperature, pressure,
length, area, volume, mass, force, velocity, flow, density, frequency, and angle units.
-}

module Energy.Conversions
    ( -- * Data Types
      UnitCategory(..)
    , Unit(..)
    , ConversionResult(..)
      -- * Unit Categories
    , energyUnits
    , powerUnits
    , temperatureUnits
    , pressureUnits
    , lengthUnits
    , areaUnits
    , volumeUnits
    , massUnits
    , forceUnits
    , velocityUnits
    , flowUnits
    , densityUnits
    , frequencyUnits
    , angleUnits
      -- * All Units
    , allUnits
    , getUnitsByCategory
    , findUnit
      -- * Conversion Functions
    , convert
    , convertMultiple
    , isValidConversion
      -- * Legacy Functions (for backward compatibility)
    , kwhToJoules
    , joulesToKwh
    , kwhToMJ
    , mjToKwh
    , btuToKwh
    , kwhToBtu
    , celsiusToFahrenheit
    , fahrenheitToCelsius
    , celsiusToKelvin
    , kelvinToCelsius
    , wattsToKW
    , kwToWatts
    , hpToKW
    , kwToHp
    , barToPsi
    , psiToBar
    , paToBar
    , barToPa
    ) where

import Data.List (find)
import Data.Maybe (fromMaybe)

-- | Engineering unit categories
data UnitCategory = 
    Energy | Power | Temperature | Pressure | Length | Area | Volume | 
    Mass | Force | Velocity | Flow | Density | Frequency | Angle
    deriving (Show, Eq, Enum, Bounded, Read)

-- | Unit definition with conversion factors
data Unit = Unit
    { unitName :: String            -- Full name (e.g., "Kilowatt-hour")
    , unitSymbol :: String          -- Symbol (e.g., "kWh")
    , unitCategory :: UnitCategory  -- Category
    , toBaseUnit :: Double -> Double    -- Function to convert to SI base unit
    , fromBaseUnit :: Double -> Double  -- Function to convert from SI base unit
    , unitDescription :: String     -- Description and common usage
    }

-- Custom Show instance that excludes the function fields
instance Show Unit where
    show u = unitSymbol u ++ " (" ++ unitName u ++ ")"

-- Make Unit comparable by symbol
instance Eq Unit where
    u1 == u2 = unitSymbol u1 == unitSymbol u2

-- | Result of a unit conversion
data ConversionResult = ConversionResult
    { originalValue :: Double
    , originalUnit :: Unit
    , convertedValue :: Double
    , convertedUnit :: Unit
    , conversionFactor :: Double
    } deriving (Show)

-- | Energy units (Base: Joules)
energyUnits :: [Unit]
energyUnits = 
    [ Unit "Joule" "J" Energy id id "SI base unit of energy"
    , Unit "Kilowatt-hour" "kWh" Energy (*3.6e6) (/3.6e6) "Common electrical energy unit"
    , Unit "Megajoule" "MJ" Energy (*1e6) (/1e6) "Large energy quantities"
    , Unit "British Thermal Unit" "BTU" Energy (*1055.06) (/1055.06) "Traditional heat energy unit"
    , Unit "Calorie" "cal" Energy (*4.184) (/4.184) "Food and chemical energy"
    , Unit "Kilocalorie" "kcal" Energy (*4184) (/4184) "Nutrition energy (food calories)"
    , Unit "Therm" "thm" Energy (*1.055e8) (/1.055e8) "Natural gas energy unit"
    , Unit "Watt-hour" "Wh" Energy (*3600) (/3600) "Small electrical energy unit"
    , Unit "Foot-pound" "ft.lbf" Energy (*1.35582) (/1.35582) "Mechanical work unit"
    ]

-- | Power units (Base: Watts)
powerUnits :: [Unit]
powerUnits = 
    [ Unit "Watt" "W" Power id id "SI base unit of power"
    , Unit "Kilowatt" "kW" Power (*1000) (/1000) "Common electrical power unit"
    , Unit "Megawatt" "MW" Power (*1e6) (/1e6) "Large power generation"
    , Unit "Horsepower" "HP" Power (*745.7) (/745.7) "Mechanical power (US)"
    , Unit "Metric Horsepower" "PS" Power (*735.5) (/735.5) "Mechanical power (metric)"
    , Unit "BTU per hour" "BTU/h" Power (*0.293071) (/0.293071) "HVAC power rating"
    , Unit "Ton of refrigeration" "TR" Power (*3516.85) (/3516.85) "Cooling capacity"
    , Unit "Foot-pound per second" "ft.lbf/s" Power (*1.35582) (/1.35582) "Mechanical power rate"
    ]

-- | Temperature units (Base: Kelvin)
temperatureUnits :: [Unit]
temperatureUnits = 
    [ Unit "Kelvin" "K" Temperature id id "SI base unit of thermodynamic temperature"
    , Unit "Celsius" "°C" Temperature (+273.15) (subtract 273.15) "Common temperature scale"
    , Unit "Fahrenheit" "°F" Temperature (\f -> (f - 32) * 5/9 + 273.15) (\k -> (k - 273.15) * 9/5 + 32) "US temperature scale"
    , Unit "Rankine" "°R" Temperature (\r -> r * 5/9) (\k -> k * 9/5) "Absolute Fahrenheit scale"
    ]

-- | Pressure units (Base: Pascals)
pressureUnits :: [Unit]
pressureUnits = 
    [ Unit "Pascal" "Pa" Pressure id id "SI base unit of pressure"
    , Unit "Kilopascal" "kPa" Pressure (*1000) (/1000) "Common pressure unit"
    , Unit "Megapascal" "MPa" Pressure (*1e6) (/1e6) "High pressure applications"
    , Unit "Bar" "bar" Pressure (*1e5) (/1e5) "Atmospheric pressure unit"
    , Unit "Pounds per square inch" "psi" Pressure (*6894.76) (/6894.76) "US pressure unit"
    , Unit "Atmosphere" "atm" Pressure (*101325) (/101325) "Standard atmospheric pressure"
    , Unit "Millimeters of mercury" "mmHg" Pressure (*133.322) (/133.322) "Medical and vacuum pressure"
    , Unit "Inches of mercury" "inHg" Pressure (*3386.39) (/3386.39) "Weather and vacuum pressure"
    , Unit "Torr" "Torr" Pressure (*133.322) (/133.322) "Vacuum pressure (≈ mmHg)"
    ]

-- | Length units (Base: Meters)
lengthUnits :: [Unit]
lengthUnits = 
    [ Unit "Meter" "m" Length id id "SI base unit of length"
    , Unit "Kilometer" "km" Length (*1000) (/1000) "Long distances"
    , Unit "Centimeter" "cm" Length (/100) (*100) "Small measurements"
    , Unit "Millimeter" "mm" Length (/1000) (*1000) "Precise measurements"
    , Unit "Foot" "ft" Length (*0.3048) (/0.3048) "US length unit"
    , Unit "Inch" "in" Length (*0.0254) (/0.0254) "Small US measurements"
    , Unit "Yard" "yd" Length (*0.9144) (/0.9144) "US distance unit"
    , Unit "Mile" "mi" Length (*1609.34) (/1609.34) "US long distance"
    , Unit "Nautical mile" "nmi" Length (*1852) (/1852) "Marine and aviation distance"
    ]

-- | Area units (Base: Square meters)
areaUnits :: [Unit]
areaUnits = 
    [ Unit "Square meter" "m²" Area id id "SI base unit of area"
    , Unit "Square kilometer" "km²" Area (*1e6) (/1e6) "Large areas"
    , Unit "Square centimeter" "cm²" Area (/1e4) (*1e4) "Small areas"
    , Unit "Square foot" "ft²" Area (*0.092903) (/0.092903) "US area unit"
    , Unit "Square inch" "in²" Area (*0.00064516) (/0.00064516) "Small US areas"
    , Unit "Acre" "ac" Area (*4046.86) (/4046.86) "Land area unit"
    , Unit "Hectare" "ha" Area (*10000) (/10000) "Metric land area"
    ]

-- | Volume units (Base: Cubic meters)
volumeUnits :: [Unit]
volumeUnits = 
    [ Unit "Cubic meter" "m³" Volume id id "SI base unit of volume"
    , Unit "Liter" "L" Volume (/1000) (*1000) "Common liquid volume"
    , Unit "Milliliter" "mL" Volume (/1e6) (*1e6) "Small liquid volume"
    , Unit "Cubic foot" "ft³" Volume (*0.0283168) (/0.0283168) "US volume unit"
    , Unit "Gallon (US)" "gal" Volume (*0.00378541) (/0.00378541) "US liquid volume"
    , Unit "Gallon (Imperial)" "gal (UK)" Volume (*0.00454609) (/0.00454609) "UK liquid volume"
    , Unit "Barrel (oil)" "bbl" Volume (*0.158987) (/0.158987) "Oil industry volume"
    , Unit "Cubic inch" "in³" Volume (*1.6387e-5) (/1.6387e-5) "Small US volume"
    ]

-- | Mass units (Base: Kilograms)
massUnits :: [Unit]
massUnits = 
    [ Unit "Kilogram" "kg" Mass id id "SI base unit of mass"
    , Unit "Gram" "g" Mass (/1000) (*1000) "Small mass unit"
    , Unit "Pound" "lb" Mass (*0.453592) (/0.453592) "US mass unit"
    , Unit "Ounce" "oz" Mass (*0.0283495) (/0.0283495) "Small US mass unit"
    , Unit "Ton (metric)" "t" Mass (*1000) (/1000) "Large metric mass"
    , Unit "Ton (US)" "ton" Mass (*907.185) (/907.185) "Large US mass"
    , Unit "Stone" "st" Mass (*6.35029) (/6.35029) "UK mass unit"
    ]

-- | Force units (Base: Newtons)
forceUnits :: [Unit]
forceUnits = 
    [ Unit "Newton" "N" Force id id "SI base unit of force"
    , Unit "Kilonewton" "kN" Force (*1000) (/1000) "Large force unit"
    , Unit "Pound-force" "lbf" Force (*4.44822) (/4.44822) "US force unit"
    , Unit "Kilogram-force" "kgf" Force (*9.80665) (/9.80665) "Gravitational force unit"
    , Unit "Dyne" "dyn" Force (/1e5) (*1e5) "CGS force unit"
    ]

-- | Velocity units (Base: Meters per second)
velocityUnits :: [Unit]
velocityUnits = 
    [ Unit "Meter per second" "m/s" Velocity id id "SI base unit of velocity"
    , Unit "Kilometer per hour" "km/h" Velocity (/3.6) (*3.6) "Common speed unit"
    , Unit "Mile per hour" "mph" Velocity (*0.44704) (/0.44704) "US speed unit"
    , Unit "Foot per second" "ft/s" Velocity (*0.3048) (/0.3048) "US velocity unit"
    , Unit "Knot" "kn" Velocity (*0.514444) (/0.514444) "Marine and aviation speed"
    ]

-- | Flow units (Base: Cubic meters per second)
flowUnits :: [Unit]
flowUnits = 
    [ Unit "Cubic meter per second" "m³/s" Flow id id "SI base unit of volumetric flow"
    , Unit "Liter per minute" "L/min" Flow (/60000) (*60000) "Common liquid flow rate"
    , Unit "Gallon per minute" "gpm" Flow (*6.309e-5) (/6.309e-5) "US liquid flow rate"
    , Unit "Cubic foot per minute" "cfm" Flow (*4.719e-4) (/4.719e-4) "US air flow rate"
    , Unit "Liter per second" "L/s" Flow (/1000) (*1000) "Industrial flow rate"
    ]

-- | Density units (Base: Kilograms per cubic meter)
densityUnits :: [Unit]
densityUnits = 
    [ Unit "Kilogram per cubic meter" "kg/m³" Density id id "SI base unit of density"
    , Unit "Gram per cubic centimeter" "g/cm³" Density (*1000) (/1000) "Common density unit"
    , Unit "Pound per cubic foot" "lb/ft³" Density (*16.0185) (/16.0185) "US density unit"
    ]

-- | Frequency units (Base: Hertz)
frequencyUnits :: [Unit]
frequencyUnits = 
    [ Unit "Hertz" "Hz" Frequency id id "SI base unit of frequency"
    , Unit "Kilohertz" "kHz" Frequency (*1000) (/1000) "Audio frequency"
    , Unit "Megahertz" "MHz" Frequency (*1e6) (/1e6) "Radio frequency"
    , Unit "Gigahertz" "GHz" Frequency (*1e9) (/1e9) "Microwave frequency"
    , Unit "Revolutions per minute" "rpm" Frequency (/60) (*60) "Rotational frequency"
    ]

-- | Angle units (Base: Radians)
angleUnits :: [Unit]
angleUnits = 
    [ Unit "Radian" "rad" Angle id id "SI base unit of angle"
    , Unit "Degree" "°" Angle (\d -> d * pi/180) (\r -> r * 180/pi) "Common angle unit"
    , Unit "Gradian" "grad" Angle (\g -> g * pi/200) (\r -> r * 200/pi) "Metric angle unit"
    , Unit "Revolution" "rev" Angle (\rev -> rev * 2*pi) (\r -> r / (2*pi)) "Full rotation"
    ]

-- | All available units
allUnits :: [Unit]
allUnits = concat 
    [ energyUnits, powerUnits, temperatureUnits, pressureUnits
    , lengthUnits, areaUnits, volumeUnits, massUnits, forceUnits
    , velocityUnits, flowUnits, densityUnits, frequencyUnits, angleUnits
    ]

-- | Get units by category
getUnitsByCategory :: UnitCategory -> [Unit]
getUnitsByCategory cat = filter (\u -> unitCategory u == cat) allUnits

-- | Find a unit by symbol
findUnit :: String -> Maybe Unit
findUnit symbol = find (\u -> unitSymbol u == symbol) allUnits

-- | Check if conversion between two unit categories is valid
isValidConversion :: UnitCategory -> UnitCategory -> Bool
isValidConversion cat1 cat2 = cat1 == cat2

-- | Convert a value from one unit to another
convert :: Double -> String -> String -> Maybe ConversionResult
convert value fromSymbol toSymbol = do
    fromUnit <- findUnit fromSymbol
    toUnit <- findUnit toSymbol
    
    -- Check if both units are in the same category
    if unitCategory fromUnit /= unitCategory toUnit
        then Nothing
        else Just $ convertUnits value fromUnit toUnit
  where
    convertUnits val from to =
        let baseValue = toBaseUnit from val
            convertedValue = fromBaseUnit to baseValue
            factor = if val == 0 then 0 else convertedValue / val
        in ConversionResult val from convertedValue to factor

-- | Convert a value to multiple target units
convertMultiple :: Double -> String -> [String] -> [ConversionResult]
convertMultiple value fromSymbol toSymbols =
    let validResults = [convert value fromSymbol toSym | toSym <- toSymbols]
    in [result | Just result <- validResults]

-- | Test all conversions within a category
testCategoryConversions :: UnitCategory -> [(String, String, Bool)]
testCategoryConversions category = 
    let units = getUnitsByCategory category
        unitSymbols = map unitSymbol units
        testValue = 1.0
        combinations = [(from, to) | from <- unitSymbols, to <- unitSymbols, from /= to]
    in [(from, to, case convert testValue from to of
                    Just _ -> True
                    Nothing -> False) | (from, to) <- combinations]

-- | Test all conversions in all categories
testAllConversions :: [UnitCategory] -> [(UnitCategory, [(String, String, Bool)])]
testAllConversions categories = 
    [(cat, testCategoryConversions cat) | cat <- categories]

-- Legacy functions for backward compatibility
kwhToJoules :: Double -> Double
kwhToJoules kwh = kwh * 3.6e6

joulesToKwh :: Double -> Double  
joulesToKwh joules = joules / 3.6e6

kwhToMJ :: Double -> Double
kwhToMJ kwh = kwh * 3.6

mjToKwh :: Double -> Double
mjToKwh mj = mj / 3.6

btuToKwh :: Double -> Double
btuToKwh btu = btu * 0.000293071

kwhToBtu :: Double -> Double
kwhToBtu kwh = kwh / 0.000293071

celsiusToFahrenheit :: Double -> Double
celsiusToFahrenheit c = c * 1.8 + 32

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f = (f - 32) / 1.8

celsiusToKelvin :: Double -> Double
celsiusToKelvin c = c + 273.15

kelvinToCelsius :: Double -> Double
kelvinToCelsius k = k - 273.15

wattsToKW :: Double -> Double
wattsToKW w = w / 1000

kwToWatts :: Double -> Double
kwToWatts kw = kw * 1000

hpToKW :: Double -> Double
hpToKW hp = hp * 0.7457

kwToHp :: Double -> Double
kwToHp kw = kw / 0.7457

barToPsi :: Double -> Double
barToPsi bar = bar * 14.5038

psiToBar :: Double -> Double
psiToBar psi = psi / 14.5038

paToBar :: Double -> Double
paToBar pa = pa / 1e5

barToPa :: Double -> Double
barToPa bar = bar * 1e5
