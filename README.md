# Freight GHG Calculator

A Shiny web application to calculate greenhouse gas (GHG) emissions for freight transportation. The app allows users to input freight legs, vehicle types, distances, and weights, and it computes CO₂, CH₄, N₂O emissions, and total CO₂e.

---

## Features

- Add or remove freight legs dynamically.
- Select freight type (FTL or LTL) and vehicle type.
- Input distance and weight (for LTL).
- Real-time calculation of emissions using EPA simplified GHG emissions methodology.
- Summary row with total emissions.
- Download results as a CSV file.
- Mobile-friendly warning: use landscape mode for best experience.
- Banner with title, subtitle, and logo.
- Footer with author and links.

---

## Installation

1. Clone the repository:

```bash
git clone https://github.com/trevor-lyman/freight-GHG-calculator.git
cd freight-GHG-calculator
```

2. Make sure you have the required R packages installed:

```R
install.packages(c("shiny", "dplyr", "reactable", "shinyWidgets", "htmltools"))
```

3. Source the calculator logic:
The app uses `r Calculator_Logic.R` for emission calculations. Ensure this file is in the same directory as app.R.

## Running the App
From R or RStudio:

```R
library(shiny)
runApp("Shiny_App")
```

This will launch the app in your default web browser.

## Usage

1. **Add Freight Legs:** Click "Add Freight Leg" to include a new leg in your shipment.
2. **Remove Last Leg:** Click "Remove Last Leg" to delete the most recently added leg.
3. **Input Freight Data:** For each leg, select freight type, vehicle type, and input distance. If using LTL, also input the weight.
4. **View Results:** The table displays calculated emissions for each leg, with CO₂, CH₄, N₂O, and total CO₂e.
5. **Download:** Click "Download Table" to export results as a CSV.

> **Mobile Tip:** For best experience on mobile, use landscape mode.

---

## Citation

Source: [EPA Simplified GHG Emissions Calculator](https://www.epa.gov/climateleadership/simplified-ghg-emissions-calculator)

---

Trevor Pettit, 2026  
[About This Project](https://github.com/trevor-lyman/freight-GHG-calculator) | [GitHub](https://github.com/trevor-lyman) | [LinkedIn](https://www.linkedin.com/in/trevor-pettit-2b115a161/)
