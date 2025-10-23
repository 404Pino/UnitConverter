// Energy Engineering Calculator - Enhanced JavaScript Functions

// Global variables for unit management
let unitsByCategory = {};
let allUnits = [];

// API call helper
async function apiCall(url, method = 'GET', data = null) {
    try {
        const options = {
            method: method,
            headers: {
                'Content-Type': 'application/json',
            }
        };
        
        if (data && method === 'POST') {
            options.body = JSON.stringify(data);
        } else if (data && method === 'GET') {
            // For GET requests, append data as query params
            const params = new URLSearchParams(data);
            url += '?' + params.toString();
        }
        
        const response = await fetch(url, options);
        if (!response.ok) {
            throw new Error(`HTTP ${response.status}: ${response.statusText}`);
        }
        
        return await response.json();
        
    } catch (error) {
        console.error('API call failed:', error);
        throw error;
    }
}

// Load units for a specific category
async function loadUnitsForCategory(category, selectElementId) {
    try {
        const data = await apiCall(`/api/units/${category}`);
        const selectElement = document.getElementById(selectElementId);
        
        if (selectElement && data.units) {
            // Clear existing options except the first one
            selectElement.innerHTML = '<option value="">Select unit</option>';
            
            // Add units for this category
            data.units.forEach(unit => {
                const option = document.createElement('option');
                option.value = unit.symbol;
                option.textContent = unit.symbol;
                option.title = `${unit.name} - ${unit.description}`;
                selectElement.appendChild(option);
            });
            
            selectElement.disabled = false;
        }
    } catch (error) {
        console.error('Failed to load units:', error);
    }
}

// Universal conversion function
async function performConversion() {
    const inputValue = document.getElementById('input-value').value;
    const fromUnit = document.getElementById('from-unit').value;
    const toUnit = document.getElementById('to-unit').value;
    const resultsSection = document.getElementById('conversion-results');
    const mainResult = document.getElementById('main-result');
    const detailsElement = document.getElementById('conversion-details');
    
    // Validation
    if (!inputValue || !fromUnit || !toUnit) {
        if (resultsSection) resultsSection.className = 'conversion-results hidden';
        return;
    }
    
    const value = parseFloat(inputValue);
    if (isNaN(value)) {
        if (mainResult) {
            mainResult.innerHTML = '<div class="error">Invalid input value</div>';
            resultsSection.className = 'conversion-results';
        }
        return;
    }
    
    try {
        // Show loading state
        if (mainResult) {
            mainResult.innerHTML = '<div class="loading">Converting...</div>';
            resultsSection.className = 'conversion-results';
        }
        
        // Perform conversion
        const result = await apiCall('/api/convert', 'POST', {
            value: value,
            from_unit: fromUnit,
            to_unit: toUnit
        });
        
        // Display main result
        if (mainResult) {
            const formattedResult = result.output.value.toFixed(6).replace(/\.?0+$/, '');
            mainResult.innerHTML = `
                <div class="result-value">
                    <span class="input-display">${value} ${fromUnit}</span>
                    <span class="equals">=</span>
                    <span class="output-display">${formattedResult} ${toUnit}</span>
                </div>
            `;
        }
        
        // Show conversion details
        if (detailsElement) {
            detailsElement.innerHTML = `
                <p class="conversion-formula">${result.formula || ''}</p>
                <p class="conversion-factor">Conversion factor: ${result.conversion_factor.toFixed(6)}</p>
            `;
        }
        
        // Show results section
        if (resultsSection) {
            resultsSection.className = 'conversion-results';
        }
        
    } catch (error) {
        console.error('Conversion failed:', error);
        if (mainResult) {
            mainResult.innerHTML = '<div class="error">Conversion failed. Please check your units.</div>';
        }
        if (detailsElement) {
            detailsElement.innerHTML = '<p class="error">Unable to perform conversion.</p>';
        }
        if (resultsSection) {
            resultsSection.className = 'conversion-results';
        }
    }
}

// Initialize the application
document.addEventListener('DOMContentLoaded', function() {
    console.log('🔋 Energy Engineering Calculator - Enhanced Version');
    
    // Set up event listeners for universal converter
    const fromCategory = document.getElementById('from-category');
    const toCategory = document.getElementById('to-category');
    const fromUnit = document.getElementById('from-unit');
    const toUnit = document.getElementById('to-unit');
    const inputValue = document.getElementById('input-value');
    const convertButton = document.getElementById('convert-button');
    
    // Category change handlers
    if (fromCategory) {
        fromCategory.addEventListener('change', function() {
            if (this.value) {
                // Sync to-category with from-category
                if (toCategory) {
                    toCategory.value = this.value;
                    toCategory.innerHTML = '<option value="' + this.value + '">' + this.value + '</option>';
                }
                
                loadUnitsForCategory(this.value, 'from-unit');
                loadUnitsForCategory(this.value, 'to-unit');
            } else {
                if (fromUnit) {
                    fromUnit.innerHTML = '<option value="">Select category first</option>';
                    fromUnit.disabled = true;
                }
                if (toUnit) {
                    toUnit.innerHTML = '<option value="">Select category first</option>';
                    toUnit.disabled = true;
                }
                if (toCategory) {
                    toCategory.innerHTML = '<option value="">Select From Category First</option>';
                    toCategory.value = '';
                }
            }
            // Hide results when category changes
            const resultsSection = document.getElementById('conversion-results');
            if (resultsSection) resultsSection.className = 'conversion-results hidden';
        });
    }
    
    // Unit change handlers - hide results but don't auto-convert
    if (fromUnit) {
        fromUnit.addEventListener('change', function() {
            const resultsSection = document.getElementById('conversion-results');
            if (resultsSection) resultsSection.className = 'conversion-results hidden';
        });
    }
    
    if (toUnit) {
        toUnit.addEventListener('change', function() {
            const resultsSection = document.getElementById('conversion-results');
            if (resultsSection) resultsSection.className = 'conversion-results hidden';
        });
    }
    
    // Input change handler - hide results but don't auto-convert
    if (inputValue) {
        inputValue.addEventListener('input', function() {
            const resultsSection = document.getElementById('conversion-results');
            if (resultsSection) resultsSection.className = 'conversion-results hidden';
        });
    }
    
    // Convert button handler
    if (convertButton) {
        convertButton.addEventListener('click', performConversion);
    }
    
    // Auto-format number inputs
    const numberInputs = document.querySelectorAll('input[type="number"]');
    numberInputs.forEach(input => {
        input.addEventListener('focus', function() {
            this.style.borderColor = '#667eea';
        });
        
        input.addEventListener('blur', function() {
            this.style.borderColor = '#e1e1e1';
        });
    });
    
    // Keyboard shortcuts
    document.addEventListener('keydown', function(event) {
        if (event.key === 'Enter') {
            const focused = document.activeElement;
            if (focused && focused.id === 'input-value') {
                event.preventDefault();
                performConversion();
            }
        }
        
        // Space bar for convert (when not in input field)
        if (event.key === ' ') {
            const focused = document.activeElement;
            if (focused && focused.tagName !== 'INPUT' && focused.tagName !== 'SELECT') {
                event.preventDefault();
                performConversion();
            }
        }
    });
    
    console.log('✅ Universal unit converter initialized');
});

// Error handling
window.addEventListener('error', function(event) {
    console.error('JavaScript error:', event.error);
});

// Service worker registration for offline functionality
if ('serviceWorker' in navigator) {
    window.addEventListener('load', function() {
        navigator.serviceWorker.register('/sw.js').then(function(registration) {
            console.log('ServiceWorker registration successful');
        }, function(err) {
            console.log('ServiceWorker registration failed');
        });
    });
}