# Calculator_Logic.R
# Trevor Pettit
# 6 March 2026

### Libraries ####
library(dplyr)

### Unit Conversions ####
# Distance
mile_to_km <- 1.609 # km / mile
nm_to_mile <- 1.15 # mile / nautical mile
km_to_mile <- 0.622 # mile / km
mile_to_mile <- 1.00 # mile / mile
nm_to_nm <- 1.00 # nautical mile / nautical mile
km_to_nm <- km_to_mile * (1/nm_to_mile) # nautical mile / km
mile_to_nm <- 1/nm_to_mile # nautical mile / mile

# Weight
lb_to_g <- 453.6 # g / lb
kg_to_g <- 1000 # g / kg
lb_to_kg <- 0.4536 # kg / lb
lbs_to_tonnes <- 0.0004536 # metric ton / lb
kg_to_lb <- 2.205 # lb / kg
g_to_ton <- 0.000001102 # short ton / g
kg_to_ton <- 0.001102000 # short ton / kg
tonnes_to_tons <- 1.102 # short ton / metric ton
lbs_to_tons <- 0.0005 # short ton / lb
tons_to_tons <- 1.00 # short ton / short ton
tonnes_per_lb <- 2205 # lb / metric ton
tonnes_to_kg <- 1000 # kg / metric ton
g_to_kg <- 1/kg_to_g # kg / g
kg_to_tonnes <- 1/tonnes_to_kg # metric ton / kg

### GWP ####
GWP_CO2 <- 1
GWP_CH4 <- 28
GWP_N2O <- 265

### Emissions factors ####
factor_CO2_MHtruck_FTL <- 1.35978659553184 # kg CO2 / vehicle-mile
factor_CH4_MHtruck_FTL <- 0.0119087314819225 # g CH4 / vehicle-mile
factor_N2O_MHtruck_FTL <- 0.0377484318672262 # g N2O / vehicle-mile
MHtruck_FTL <- cbind(factor_CO2_MHtruck_FTL, factor_CH4_MHtruck_FTL, 
                     factor_N2O_MHtruck_FTL)

factor_CO2_car_FTL <- 0.306131007296821 # kg CO2 / vehicle-mile
factor_CH4_car_FTL <- 0.00898623309090473 # g CH4 / vehicle-mile
factor_N2O_car_FTL <- 0.00601342893630355 # g N2O / vehicle-mile
car_FTL <- cbind(factor_CO2_car_FTL, factor_CH4_car_FTL, 
                 factor_N2O_car_FTL)

factor_CO2_Ltruck_FTL <- 0.40545567265964 # kg CO2 / vehicle-mile
factor_CH4_Ltruck_FTL <- 0.011070764325569 # g CH4 / vehicle-mile
factor_N2O_Ltruck_FTL <- 0.00982582554480694 # g N2O / vehicle-mile
Ltruck_FTL <- cbind(factor_CO2_Ltruck_FTL, factor_CH4_Ltruck_FTL, 
                     factor_N2O_Ltruck_FTL)

factor_CO2_truck_LTL <- 0.168042601834139 # kg CO2 / ton-mile
factor_CH4_truck_LTL <- 0.00147168256353026 # g CH4 / ton-mile
factor_N2O_truck_LTL <- 0.00466495605043554 # g N2O / ton-mile
truck_LTL <- cbind(factor_CO2_truck_LTL, factor_CH4_truck_LTL, 
                   factor_N2O_truck_LTL)


factor_CO2_rail_LTL <- 0.0218932118681075 # kg CO2 / ton-mile
factor_CH4_rail_LTL <- 0.00171076475414864 # g CH4 / ton-mile
factor_N2O_rail_LTL <- 0.000547444721327564 # g N2O / ton-mile
rail_LTL <- cbind(factor_CO2_rail_LTL, factor_CH4_rail_LTL, 
                   factor_N2O_rail_LTL)

factor_CO2_sea_LTL <- 0.0821016445284028 # kg CO2 / ton-mile
factor_CH4_sea_LTL <- 0.0325800176700011 # g CH4 / ton-mile
factor_N2O_sea_LTL <- 0.00206545017681516 # g N2O / ton-mile
sea_LTL <- cbind(factor_CO2_sea_LTL, factor_CH4_sea_LTL, 
                  factor_N2O_sea_LTL)

factor_CO2_air_LTL <- 0.904815113272083 # kg CO2 / ton-mile
factor_CH4_air_LTL <- 0 # g CH4 / ton-mile
factor_N2O_air_LTL <- 0.027861370252691 # g N2O / ton-mile
air_LTL <- cbind(factor_CO2_air_LTL, factor_CH4_air_LTL, 
                 factor_N2O_air_LTL)

### Define Parameters ####
distance_unit <- c("miles", "nautical miles", "km")

weight_unit <- c("g", "kg", "metric tons", "lb", "short tons")

vehicle_type <- c("Medium- and Heavy-Duty Truck", "Passenger Car", 
                  "Light-Duty Truck", "Rail", "Waterborne Craft",
                  "Aircraft")

transportation_type <- c("FTL", "LTL")

carbon_unit <- c("Vehicle-Miles", "Short Ton-Miles")

### Vehicle Types Logic ####
get_vehicle_types <- function(transportation_type) {
  if (transportation_type == "FTL") {
    c("Medium- and Heavy-Duty Truck", "Passenger Car", 
      "Light-Duty Truck")
  } else if (transportation_type == "LTL") {
    c("Medium- and Heavy-Duty Truck", "Rail", "Waterborne Craft",
      "Aircraft")
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

### Distance Standardization Logic ####
get_miles <- function(vehicle_type, distance_value, distance_unit) {
  if ((vehicle_type == "Waterborne Craft" | 
      vehicle_type == "Aircraft") && distance_unit == "miles") {
    distance_value * mile_to_nm
  } else if ((vehicle_type == "Waterborne Craft" | 
             vehicle_type == "Aircraft") && distance_unit == "nautical miles") {
    distance_value * nm_to_nm
  } else if ((vehicle_type == "Waterborne Craft" | 
             vehicle_type == "Aircraft") && distance_unit == "km") {
    distance_value * km_to_nm
  } else if (distance_unit == "miles") {
    distance_value * mile_to_mile 
  } else if (distance_unit == "nautical miles") {
    distance_value * nm_to_mile
  } else if (distance_unit == "km") {
    distance_value * km_to_mile
  } else {
    NA
  }
}

### Weight Standardization Logic #### 
# convert to short-tons
get_tons <- function(weight_value, weight_unit) {
  if (weight_unit == "g") {
    weight_value * g_to_tons
  } else if (weight_unit == "kg") {
    weight_value * kg_to_tons
  } else if (weight_unit == "metric tons") {
    weight_value * tonnes_to_tons 
  } else if (weight_unit == "lb") {
    weight_value * lbs_to_tons 
  } else if (weight_unit == "short tons") {
    weight_value * tons_to_tons
  } else {
    NA
  }
}

### CO2 Factor Logic ####
get_factor_CO2 <- function(transportation_type, vehicle_type) {
  if (transportation_type == "FTL" && 
      vehicle_type == "Medium- and Heavy-Duty Truck") {
    factor_CO2_MHtruck_FTL
  } else if (transportation_type == "FTL" &&
             vehicle_type == "Passenger Car") {
    factor_CO2_car_FTL
  } else if (transportation_type == "FTL" && 
             vehicle_type == "Light-Duty Truck") {
    factor_CO2_Ltruck_FTL
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Medium- and Heavy-Duty Truck") {
    factor_CO2_truck_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Rail") {
    factor_CO2_rail_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Waterborne Craft") {
    factor_CO2_sea_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Aircraft") {
    factor_CO2_air_LTL 
  } else {
    NA
  }
}

### CH4 Factor Logic ####
get_factor_CH4 <- function(transportation_type, vehicle_type) {
  if (transportation_type == "FTL" && 
      vehicle_type == "Medium- and Heavy-Duty Truck") {
    factor_CH4_MHtruck_FTL
  } else if (transportation_type == "FTL" &&
             vehicle_type == "Passenger Car") {
    factor_CH4_car_FTL
  } else if (transportation_type == "FTL" && 
             vehicle_type == "Light-Duty Truck") {
    factor_CH4_Ltruck_FTL
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Medium- and Heavy-Duty Truck") {
    factor_CH4_truck_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Rail") {
    factor_CH4_rail_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Waterborne Craft") {
    factor_CH4_sea_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Aircraft") {
    factor_CH4_air_LTL 
  } else {
    NA
  }
}

### N2O Factor Logic ####
get_factor_N2O <- function(transportation_type, vehicle_type) {
  if (transportation_type == "FTL" && 
      vehicle_type == "Medium- and Heavy-Duty Truck") {
    factor_N2O_MHtruck_FTL
  } else if (transportation_type == "FTL" &&
             vehicle_type == "Passenger Car") {
    factor_N2O_car_FTL
  } else if (transportation_type == "FTL" && 
             vehicle_type == "Light-Duty Truck") {
    factor_N2O_Ltruck_FTL
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Medium- and Heavy-Duty Truck") {
    factor_N2O_truck_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Rail") {
    factor_N2O_rail_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Waterborne Craft") {
    factor_N2O_sea_LTL 
  } else if (transportation_type == "LTL" && 
             vehicle_type == "Aircraft") {
    factor_N2O_air_LTL 
  } else {
    NA
  }
}

### CO2 Calculation Logic ####
# units: kg
calculate_CO2 <- function(transportation_type, vehicle_type,
                          distance_value, distance_unit,
                          weight_value, weight_unit) {
  
  if (get_carbon_unit(transportation_type) == "Vehicle-Miles") {
    
    get_miles(vehicle_type = vehicle_type,
              distance_value = distance_value, 
              distance_unit = distance_unit) *
      get_factor_CO2(transportation_type = transportation_type, 
                     vehicle_type = vehicle_type)
    
  } else if (get_carbon_unit(transportation_type) == "Short Ton-Miles") {
    
    get_miles(vehicle_type = vehicle_type,
              distance_value = distance_value, 
              distance_unit = distance_unit) *
      get_tons(weight_value = weight_value, weight_unit = weight_unit) *
      get_factor_CO2(transportation_type = transportation_type, 
                     vehicle_type = vehicle_type) 
    } else {
    NA
  }
}

### CH4 Calculation Logic ####
# units: g
calculate_CH4 <- function(transportation_type, vehicle_type,
                          distance_value, distance_unit,
                          weight_value, weight_unit) {
  
  if (get_carbon_unit(transportation_type) == "Vehicle-Miles") {
    
    get_miles(vehicle_type = vehicle_type,
              distance_value = distance_value, 
              distance_unit = distance_unit) *
      get_factor_CH4(transportation_type = transportation_type, 
                     vehicle_type = vehicle_type)
    
  } else if (get_carbon_unit(transportation_type) == "Short Ton-Miles") {
    
    get_miles(vehicle_type = vehicle_type,
              distance_value = distance_value, 
              distance_unit = distance_unit) *
      get_tons(weight_value = weight_value, weight_unit = weight_unit) *
      get_factor_CH4(transportation_type = transportation_type, 
                     vehicle_type = vehicle_type) 
  } else {
    NA
  }
}

### N2O Calculation Logic ####
# units: g
calculate_N2O <- function(transportation_type, vehicle_type,
                          distance_value, distance_unit,
                          weight_value, weight_unit) {
  
  if (get_carbon_unit(transportation_type) == "Vehicle-Miles") {
    
    get_miles(vehicle_type = vehicle_type,
              distance_value = distance_value, 
              distance_unit = distance_unit) *
      get_factor_N2O(transportation_type = transportation_type, 
                     vehicle_type = vehicle_type)
    
  } else if (get_carbon_unit(transportation_type) == "Short Ton-Miles") {
    
    get_miles(vehicle_type = vehicle_type,
              distance_value = distance_value, 
              distance_unit = distance_unit) *
      get_tons(weight_value = weight_value, weight_unit = weight_unit) *
      get_factor_N2O(transportation_type = transportation_type, 
                     vehicle_type = vehicle_type) 
  } else {
    NA
  }
}

### CO2 Calculation Logic ####
# units: tonnes
calculate_CO2e <- function(transportation_type, vehicle_type,
                           distance_value, distance_unit,
                           weight_value, weight_unit) {
  (calculate_CO2(transportation_type = transportation_type, 
                vehicle_type = vehicle_type, distance_value = distance_value,
                distance_unit = distance_unit, weight_value = weight_value,
                weight_unit = weight_unit) * GWP_CO2 + 
     calculate_CH4(transportation_type = transportation_type, 
                   vehicle_type = vehicle_type, distance_value = distance_value,
                   distance_unit = distance_unit, weight_value = weight_value,
                   weight_unit = weight_unit) * GWP_CH4*g_to_kg +
     calculate_N2O(transportation_type = transportation_type, 
                   vehicle_type = vehicle_type, distance_value = distance_value,
                   distance_unit = distance_unit, weight_value = weight_value,
                   weight_unit = weight_unit) * GWP_N2O*g_to_kg
  )*kg_to_tonnes
}

### Demo ####
transportation_type <- "LTL"
vehicle_type <- "Medium- and Heavy-Duty Truck"
distance_value <- 1
distance_unit <- "miles"
weight_value <- 100
weight_unit <- "short tons"

df <- data.frame(transportation_type, vehicle_type, distance_value,
                 distance_unit, weight_value, weight_unit) %>%
  mutate(CO2 = calculate_CO2(transportation_type = transportation_type,
                             vehicle_type = vehicle_type, 
                             distance_value = distance_value,
                             distance_unit = distance_unit, 
                             weight_value = weight_value,
                             weight_unit = weight_unit)) %>%
  mutate(CH4 = calculate_CH4(transportation_type = transportation_type,
                             vehicle_type = vehicle_type, 
                             distance_value = distance_value,
                             distance_unit = distance_unit, 
                             weight_value = weight_value,
                             weight_unit = weight_unit)) %>%
  mutate(N2O = calculate_N2O(transportation_type = transportation_type,
                             vehicle_type = vehicle_type, 
                             distance_value = distance_value,
                             distance_unit = distance_unit, 
                             weight_value = weight_value,
                             weight_unit = weight_unit)) %>%
  mutate(CO2e = calculate_CO2e(transportation_type = transportation_type,
                               vehicle_type = vehicle_type, 
                               distance_value = distance_value,
                               distance_unit = distance_unit,
                               weight_value = weight_value,
                               weight_unit = weight_unit))


