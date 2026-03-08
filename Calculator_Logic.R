# Calculator_Logic.R
# Trevor Pettit
# 7 March 2026
# Optimized version with safe factor lookup

library(dplyr)
library(tibble)

### ---------------------------------------------------------
### Unit Conversion Tables
### ---------------------------------------------------------
distance_to_mile <- c(
  "mile" = 1,
  "nautical mile" = 1.15078,
  "km" = 0.621371
)

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
distance_unit <- c("mile","nautical mile","km")
weight_unit <- c("g","kg","metric ton","lb","short ton")
vehicle_type_list <- c(
  "Medium- and Heavy-Duty Truck",
  "Passenger Car",
  "Light-Duty Truck",
  "Rail",
  "Waterborne Craft",
  "Aircraft"
)
freight_type <- c("FTL","LTL")

### ---------------------------------------------------------
### Emission Factor Table
### ---------------------------------------------------------
emission_factors <- tibble(
  freight_type = c(
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
get_vehicle_types <- function(t_type) {
  if(t_type == "FTL") c("Medium- and Heavy-Duty Truck",
                        "Passenger Car",
                        "Light-Duty Truck")
  else if(t_type == "LTL") c("Medium- and Heavy-Duty Truck",
                             "Rail",
                             "Waterborne Craft",
                             "Aircraft")
  else character(0)
}

get_carbon_unit <- function(t_type) {
  if(t_type == "FTL") "Vehicle-Miles" 
  else if(t_type == "LTL") "Short Ton-Miles" else NA
}

### ---------------------------------------------------------
### Unit Standardization
### ---------------------------------------------------------
get_miles <- function(distance_value, distance_unit){
  if(!distance_unit %in% names(distance_to_mile)) 
    stop("Distance unit not recognized")
  distance_value * distance_to_mile[[distance_unit]]
}

get_tons <- function(weight_value, weight_unit){
  if(!weight_unit %in% names(weight_to_ton)) 
    stop("Weight unit not recognized")
  weight_value * weight_to_ton[[weight_unit]]
}

### ---------------------------------------------------------
### Emission Factor Retrieval
### ---------------------------------------------------------
get_factors <- function(t_type, v_type){
  f <- emission_factors %>%
    filter(freight_type == t_type, vehicle_type == v_type)
  if(nrow(f)==0) 
    stop("Emission factor not found for selected combination")
  f
}

### ---------------------------------------------------------
### Gas Calculations
### ---------------------------------------------------------
calculate_CO2 <- function(t_type, v_type, d_val, d_unit, w_val, w_unit){
  f <- get_factors(t_type, v_type)
  miles <- get_miles(d_val, d_unit)
  if(get_carbon_unit(t_type) == "Vehicle-Miles") 
    miles * f$CO2 else miles * get_tons(w_val,w_unit) * f$CO2
}

calculate_CH4 <- function(t_type, v_type, d_val, d_unit, w_val, w_unit){
  f <- get_factors(t_type, v_type)
  miles <- get_miles(d_val, d_unit)
  if(get_carbon_unit(t_type) == "Vehicle-Miles") 
    miles * f$CH4 else miles * get_tons(w_val,w_unit) * f$CH4
}

calculate_N2O <- function(t_type, v_type, d_val, d_unit, w_val, w_unit){
  f <- get_factors(t_type, v_type)
  miles <- get_miles(d_val, d_unit)
  if(get_carbon_unit(t_type) == "Vehicle-Miles") 
    miles * f$N2O else miles * get_tons(w_val,w_unit) * f$N2O
}

### ---------------------------------------------------------
### CO2e Calculation
### ---------------------------------------------------------
calculate_CO2e <- function(t_type, v_type, d_val, d_unit, w_val, w_unit){
  CO2 <- calculate_CO2(t_type, v_type, d_val, d_unit, w_val, w_unit)
  CH4 <- calculate_CH4(t_type, v_type, d_val, d_unit, w_val, w_unit)
  N2O <- calculate_N2O(t_type, v_type, d_val, d_unit, w_val, w_unit)
  (CO2*GWP_CO2 + CH4*GWP_CH4*g_to_kg + N2O*GWP_N2O*g_to_kg)*kg_to_tonne
}