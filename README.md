# Universal Unit Converter - Haskell Web Application

A practical Haskell web application providing universal unit conversions across 14 categories. Built with Scotty web framework and featuring a responsive web interface.

## 🎯 Features

This project provides:
- **Universal Unit Converter**: 522+ conversions across 14 categories
- **Web Interface**: Clean, responsive HTML interface
- **REST API**: JSON endpoints for programmatic access
- **Real-time Conversion**: Instant results with live category syncing
- **Multiple Results**: Convert to all units in a category simultaneously

## 📦 Project Structure

```
Haskell Project One/
├── energy-engineering.cabal   # Project configuration
├── stack.yaml                  # Stack build tool configuration
├── src/
│   ├── Main.hs                 # Application entry point
│   ├── Energy/
│   │   └── Conversions.hs      # Universal unit conversion functions
│   └── Web/
│       ├── Server.hs           # Scotty web server
│       ├── API.hs              # REST API endpoints
│       └── Views.hs            # HTML template generation
└── static/
    ├── style.css               # Responsive CSS styling
    └── script.js               # Frontend JavaScript
```

## 🚀 Getting Started

### Prerequisites

1. **Install Stack** (Haskell build tool):
   - Windows: Download from [haskellstack.org](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
   - Or use: `winget install commercialhaskell.stack`

2. **Verify Installation**:
   ```powershell
   stack --version
   ```

### Building and Running

```powershell
# Navigate to project directory
cd "c:\Users\pino_\Desktop\Privat\AI\Haskell Project One"

# Setup Haskell toolchain (first time only)
stack setup

# Build and run the web application
stack run

# Or specify web mode explicitly
stack run energy-engineering web

# Run command-line examples
stack run energy-engineering examples
```

The web interface will be available at: http://localhost:3000

## 🌐 Usage

### Web Interface
1. Open http://localhost:3000 in your browser
2. Select a unit category (Energy, Power, Temperature, etc.)
3. Choose your input unit and enter a value
4. Click "Convert" to see results for all units in that category

### API Endpoints
- `GET /api/units/<category>` - List all units in a category
- `POST /api/convert` - Convert single value
- `POST /api/convert-multiple` - Convert to multiple target units

### Supported Categories
- **Energy**: J, kJ, MJ, kWh, BTU, cal, kcal, thm
- **Power**: W, kW, MW, HP, PS, BTU/h, TR
- **Temperature**: °C, °F, K, °R
- **Pressure**: Pa, kPa, MPa, bar, atm, psi, mmHg
- **Length**: m, km, cm, mm, in, ft, yd
- **Mass**: kg, g, lb, oz, t
- **Volume**: L, mL, gal, qt, pt, cup, fl oz
- **Area**: m², km², cm², in², ft², ac
- **Velocity**: m/s, km/h, mph, ft/s, knot
- **Force**: N, kN, lbf, dyn
- **Torque**: Nm, lbf⋅ft, kgf⋅m
- **Frequency**: Hz, kHz, MHz, GHz, rpm
- **Angle**: °, rad, grad, rev
- **Flow**: m³/s, L/s, L/min, gpm, cfm

## 🧪 Interactive Development with GHCi

GHCi is Haskell's REPL for testing functions:

```powershell
# Start GHCi with the project loaded
stack ghci

# Try some conversion functions:
ghci> kwhToJoules 10
36000000.0

ghci> celsiusToFahrenheit 20
68.0

ghci> universalConvert "Power" "kW" "W" 5.0
Just 5000.0

# Load and run the main program
ghci> :load src/Main.hs
ghci> main

# Reload after changes
ghci> :reload
```

## 📚 Module Overview

### Energy.Conversions
Universal unit conversion system covering 14 categories with 522+ conversion functions:
- Complete coverage of engineering units
- Type-safe conversion functions
- Support for both single and multiple target conversions

### Web.Server
Scotty-based web server with static file serving and API routing.

### Web.API  
REST API endpoints providing JSON responses for unit conversions.

### Web.Views
Blaze-HTML template generation for the responsive web interface.

## 🎓 Technology Stack

### Backend
- **Haskell**: Functional programming language with strong type safety
- **Scotty**: Lightweight web framework for REST APIs
- **Aeson**: JSON parsing and generation
- **Blaze-HTML**: Type-safe HTML generation

### Frontend  
- **HTML5**: Semantic markup with responsive design
- **CSS3**: Grid-based layout with modern styling
- **JavaScript**: Fetch API for backend communication

## 🔨 Example Conversions

```bash
# Energy conversions
100 kWh = 360,000,000 J = 341,214 BTU = 85,985 kcal

# Temperature conversions  
25°C = 77°F = 298.15 K = 536.67°R

# Power conversions
500 HP = 372.85 kW = 372,850 W = 1,272,122 BTU/h

# Pressure conversions
100 psi = 6.895 bar = 689.5 kPa = 703.1 kg/cm²
```

## 🐛 Common Issues

### "Stack not found"
Install Stack from [haskellstack.org](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### "Port already in use"
Another application is using port 3000. Stop it or change the port in `Web.Server.hs`.

### API returns 500 errors
Check that JSON request format matches the expected structure:
```json
{"value": 100, "from_unit": "kW", "to_unit": "W"}
```

## 📖 Learning Resources

- [Scotty Documentation](https://hackage.haskell.org/package/scotty) - Web framework guide
- [Aeson Tutorial](https://artyom.me/aeson) - JSON handling in Haskell
- [Haskell Wiki](https://wiki.haskell.org/) - Reference documentation
- [Stack Documentation](https://docs.haskellstack.org/) - Build tool guide

## 🎯 Next Steps

1. **Run the web app**: `stack run`
2. **Test the API**: Use browser or curl to test endpoints
3. **Explore the code**: Understand the conversion system
4. **Add new units**: Extend the conversion categories
5. **Customize the UI**: Modify the HTML/CSS/JS frontend

## 💡 Development Tips

- **Hot reload**: Restart with `stack run` after Haskell changes
- **Debug API**: Check browser Network tab for request/response details
- **Test conversions**: Use GHCi to verify conversion accuracy
- **Monitor logs**: Server logs show all HTTP requests and responses

## 📝 License

MIT License - Feel free to use and modify for your projects.

---

**Universal Unit Converter - Powered by Haskell! 🚀**
