# Calculator_Logic.R
# Trevor Pettit
# 7 March 2026
# Optimized version

library(dplyr)

### ---------------------------------------------------------
### Unit Conversion Tables
### ---------------------------------------------------------

# distance → miles
distance_to_mile <- c(
  "mile" = 1,
  "nautical mile" = 1.15078,
  "km" = 0.621371
)

# weight → short tons
weight_to_ton <- c(
  "g" = 1.10231e-6,
  "kg" = 0.00110231,
  "metric ton" = 1.10231,
  "lb" = 0.0005,
  "short ton" = 1
)

### ---------------------------------------------------------
### Global Warming Potentials
### ---------------------------------------------------------

GWP_CO2 <- 1
GWP_CH4 <- 28
GWP_N2O <- 265

g_to_kg <- 0.001
kg_to_tonne <- 0.001

### ---------------------------------------------------------
### User Input Options
### ---------------------------------------------------------

distance_unit <- c(
  "mile",
  "nautical mile",
  "km"
)

weight_unit <- c(
  "g",
  "kg",
  "metric ton",
  "lb",
  "short ton"
)

vehicle_type <- c(
  "Medium- and Heavy-Duty Truck",
  "Passenger Car",
  "Light-Duty Truck",
  "Rail",
  "Waterborne Craft",
  "Aircraft"
)

transportation_type <- c(
  "FTL",
  "LTL"
)

### ---------------------------------------------------------
### Emissions Factor Lookup Table
### ---------------------------------------------------------

emission_factors <- tibble::tibble(
  
  transportation_type = c(
    "FTL","FTL","FTL",
    "LTL","LTL","LTL","LTL"
  ),
  
  vehicle_type = c(
    "Medium- and Heavy-Duty Truck",
    "Passenger Car",
    "Light-Duty Truck",
    "Medium- and Heavy-Duty Truck",
    "Rail",
    "Waterborne Craft",
    "Aircraft"
  ),
  
  CO2 = c(
    1.35978659553184,
    0.306131007296821,
    0.40545567265964,
    0.168042601834139,
    0.0218932118681075,
    0.0821016445284028,
    0.904815113272083
  ),
  
  CH4 = c(
    0.0119087314819225,
    0.00898623309090473,
    0.011070764325569,
    0.00147168256353026,
    0.00171076475414864,
    0.0325800176700011,
    0
  ),
  
  N2O = c(
    0.0377484318672262,
    0.00601342893630355,
    0.00982582554480694,
    0.00466495605043554,
    0.000547444721327564,
    0.00206545017681516,
    0.027861370252691
  )
  
)

### ---------------------------------------------------------
### Vehicle Logic
### ---------------------------------------------------------

get_vehicle_types <- function(transportation_type) {
  
  if (transportation_type == "FTL") {
    c(
      "Medium- and Heavy-Duty Truck",
      "Passenger Car",
      "Light-Duty Truck"
    )
  } else if (transportation_type == "LTL") {
    c(
      "Medium- and Heavy-Duty Truck",
      "Rail",
      "Waterborne Craft",
      "Aircraft"
    )
  } else {
    NA
  }
  
}

get_carbon_unit <- function(transportation_type) {
  
  if (transportation_type == "FTL") {
    "Vehicle-Miles"
  } else if (transportation_type == "LTL") {
    "Short Ton-Miles"
  } else {
    NA
  }
  
}

### ---------------------------------------------------------
### Unit Standardization
### ---------------------------------------------------------

get_miles <- function(vehicle_type, distance_value, distance_unit) {
  
  if(!distance_unit %in% names(distance_to_mile))
    stop("Invalid distance unit")
  
  distance_value * distance_to_mile[[distance_unit]]
  
}

get_tons <- function(weight_value, weight_unit) {
  
  weight_value * weight_to_ton[[weight_unit]]
  
}

### ---------------------------------------------------------
### Emission Factor Retrieval
### ---------------------------------------------------------

get_factors <- function(transportation_type, vehicle_type) {
  
  emission_factors[
    emission_factors$transportation_type == transportation_type &
      emission_factors$vehicle_type == vehicle_type,
  ]
  
}

### ---------------------------------------------------------
### Gas Calculations
### ---------------------------------------------------------

calculate_CO2 <- function(
    transportation_type,
    vehicle_type,
    distance_value,
    distance_unit,
    weight_value,
    weight_unit
){
  
  factors <- get_factors(transportation_type, vehicle_type)
  
  miles <- get_miles(vehicle_type, distance_value, distance_unit)
  
  if(get_carbon_unit(transportation_type) == "Vehicle-Miles"){
    
    miles * factors$CO2
    
  } else {
    
    tons <- get_tons(weight_value, weight_unit)
    
    miles * tons * factors$CO2
    
  }
  
}

calculate_CH4 <- function(
    transportation_type,
    vehicle_type,
    distance_value,
    distance_unit,
    weight_value,
    weight_unit
){
  
  factors <- get_factors(transportation_type, vehicle_type)
  
  miles <- get_miles(vehicle_type, distance_value, distance_unit)
  
  if(get_carbon_unit(transportation_type) == "Vehicle-Miles"){
    
    miles * factors$CH4
    
  } else {
    
    tons <- get_tons(weight_value, weight_unit)
    
    miles * tons * factors$CH4
    
  }
  
}

calculate_N2O <- function(
    transportation_type,
    vehicle_type,
    distance_value,
    distance_unit,
    weight_value,
    weight_unit
){
  
  factors <- get_factors(transportation_type, vehicle_type)
  
  miles <- get_miles(vehicle_type, distance_value, distance_unit)
  
  if(get_carbon_unit(transportation_type) == "Vehicle-Miles"){
    
    miles * factors$N2O
    
  } else {
    
    tons <- get_tons(weight_value, weight_unit)
    
    miles * tons * factors$N2O
    
  }
  
}

### ---------------------------------------------------------
### CO2e Calculation
### ---------------------------------------------------------

calculate_CO2e <- function(
    transportation_type,
    vehicle_type,
    distance_value,
    distance_unit,
    weight_value,
    weight_unit
){
  
  CO2 <- calculate_CO2(
    transportation_type,
    vehicle_type,
    distance_value,
    distance_unit,
    weight_value,
    weight_unit
  )
  
  CH4 <- calculate_CH4(
    transportation_type,
    vehicle_type,
    distance_value,
    distance_unit,
    weight_value,
    weight_unit
  )
  
  N2O <- calculate_N2O(
    transportation_type,
    vehicle_type,
    distance_value,
    distance_unit,
    weight_value,
    weight_unit
  )
  
  (
    CO2 * GWP_CO2 +
      CH4 * GWP_CH4 * g_to_kg +
      N2O * GWP_N2O * g_to_kg
  ) * kg_to_tonne
  
}

if (interactive()) {
  
  transportation_type <- "LTL"
  vehicle_type <- "Medium- and Heavy-Duty Truck"
  distance_value <- 1
  distance_unit <- "miles"
  weight_value <- 100
  weight_unit <- "short tons"
  
  df <- data.frame(
    transportation_type, vehicle_type,
    distance_value, distance_unit,
    weight_value, weight_unit
  )
  
}