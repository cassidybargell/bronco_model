#### Bronco #### 
# 08/06/2024 -- 

library(nloptr)
library(tidyverse)

## resting heart rate and vo2 max estimates 


vo2max <- function(RHR, MHR){ 
VO2max <-  15.3*( MHR / RHR)
return(VO2max)
}

cass_vo2 <- vo2max(48, 194)

nick_vo2 <- vo2max(48, 194)

nick_kg <- 77

cass_kg <- 69

# energy expenditure, assume 1 L of O2 = 5kcal

kcal_min <- function(vo2_max, kg) { 
  # convert ml/kg/min vo2 max to L/kg/min
  vo2_max_L <- vo2_max/100 
  
  # liters/min
  L_min <- vo2_max_L * kg
  
  # energy
  energy <- L_min * 5 # constant
  
  return(energy)
  
}

nick_kcal_min <- kcal_min(nick_vo2, nick_kg)
cass_kcal_min <- kcal_min(cass_vo2, cass_kg)

### need to test our 1.2k for more accurate km/h

# Define bronco
bronco <- function(params) {
  # optimal velocity for distances (v1 = 20m, v2 = 40m, v3 = 60m)
  # in final function upper and lower bounds for the parameters have to be in m/s ? 
  v1 <- params[1]
  v2 <- params[2]
  v3 <- params[3]
  
##  standard bronco reps and parameters
  # distances for bronco 
  d1 <- 20*10 
  d2 <- 40*10 
  d3 <- 60*10
  
  # energetic cost factors super inaccurate - don't know how to do this better 
  c1 <- 1.0 # 20m
  # assume 5 m to accelerate and 5 m to decelerate (-10m total for next two)
  c2 <- 30/20 # 40m
  c3 <- 50/20 # 60m
  
  ## would rather this be meters/second to accelerate than seconds but can only figure
  ## out with seconds
  # change to 5m/s 
  t_acc <- 5
  t_dec <- 5
  # Total number of turns
  n <- 29 
  
  ## ignore
  # time - inaccurate bc using distances without subtracting acceleration time
  # T_acc_dec <- n * (t_acc + t_dec)
  # T_run <- (d1 / v1) + (d2 / v2) + (d3 / v3)
  
  ## time 2 - assume always accelerate for 1 second. not taking into account
  ## additional cost of accelerating to higher velocity
  
  # total time accelerating and decelerating (always accelerate/decelerate for 1
  # second into turns)
  T_acc_dec <- n * 1 
  
  # find actual distances at speed given acceleration being constant 
  d1_2 <- 20*10 - 5*10
  d2_2 <- 40*10 - 5*10
  # no last decel 
  d3_2 <- 60*10 - 5*9
  
  T_run <- (d1_2/v1) + (d2_2/v2) + (d3_2/v3)
  
  T_total <- T_acc_dec + T_run
  return(T_total)
}

# Energetic constraints of bronco - seperate per person based on energy capacity 
constraint_function_nick <- function(params) {
  # optimal velocities for distances (v1 = 20m, v2 = 40m, v3 = 60m)
  # in final function upper and lower bounds for the parameters have to be in m/s ? 
  v1 <- params[1]
  v2 <- params[2]
  v3 <- params[3]
  
  ##  standard bronco reps and parameters
  # distances for bronco 
  d1 <- 20*10 
  d2 <- 40*10 
  d3 <- 60*10

  # energetic cost factors super inaccurate - don't know how to do this better 
  c1 <- 1.0 # 20m
  # assume 5 m to accelerate and 5 m to decelerate (-10m total for next two)
  c2 <- 30/20 # 40m
  c3 <- 50/20 # 60m
  
  # total energy capacity - assume using our vo2 max -- massive problem with this
  # model is energy capacity not constant, changes w time. using 5 min.
  
  E <- nick_kcal_min * 5
  # chk 1 second for accel/decel times kcal_min
  E_acc <- nick_kcal_min * (1/60 * 15)
  E_dec <- nick_kcal_min * (1/60 * 14)
  # n turns 
  n <- 29 
  
  # energy totals 
  # find actual distances at speed given acceleration being constant 
  d1_2 <- 20*10 - 5*10
  d2_2 <- 40*10 - 5*10
  # no last decel 
  d3_2 <- 60*10 - 5*9
  
  E_acc_dec <- (E_acc + E_dec)
  E_run <- (c1 * v1 * d1_2) + (c2 * v2 * d2_2) + (c3 * v3 * d3_2)
  
  # Calculate total energy
  E_total <- E_acc_dec + E_run
  return(E - E_total)
}

# Energetic constraints of bronco - seperate per person based on energy capacity 
constraint_function_cass <- function(params) {
  # optimal velocities for distances (v1 = 20m, v2 = 40m, v3 = 60m)
  # in final function upper and lower bounds for the parameters have to be in m/s ? 
  v1 <- params[1]
  v2 <- params[2]
  v3 <- params[3]
  
  ##  standard bronco reps and parameters
  # distances for bronco 
  d1 <- 20*10 
  d2 <- 40*10 
  d3 <- 60*10
  
  # energetic cost factors super inaccurate - don't know how to do this better 
  c1 <- 1.0 # 20m
  # assume 5 m to accelerate and 5 m to decelerate (-10m total for next two)
  c2 <- 30/20 # 40m
  c3 <- 50/20 # 60m
  
  # total energy capacity - assume using our vo2 max -- massive problem with this
  # model is energy capacity not constant, changes w time. using 5 min.
  
  E <- cass_kcal_min * 5
  # chk 1 second for accel/decel times kcal_min
  E_acc <- cass_kcal_min * (1/60 * 15)
  E_dec <- cass_kcal_min * (1/60 * 14)
  # n turns 
  n <- 29 
  
  # energy totals 
  # find actual distances at speed given acceleration being constant 
  d1_2 <- 20*10 - 5*10
  d2_2 <- 40*10 - 5*10
  # no last decel 
  d3_2 <- 60*10 - 5*9
  
  E_acc_dec <- (E_acc + E_dec)
  E_run <- (c1 * v1 * d1_2) + (c2 * v2 * d2_2) + (c3 * v3 * d3_2)
  
  # Calculate total energy
  E_total <- E_acc_dec + E_run
  return(E - E_total)
}

# Set initial values (m/s guesses for bronco )
initial_values <- c(3, 3.2, 3.5)

# Optimize !! 
result_nick <- nloptr(
  x0 = initial_values,
  eval_f = bronco,
  eval_g_ineq = constraint_function_nick,
  lb = c(0.1, 0.1, 0.1), # Lower bounds
  # guess nick upper bound m/s
  ub = c(9, 9, 9), # Upper bounds
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

result_cass <- nloptr(
  x0 = initial_values,
  eval_f = bronco,
  eval_g_ineq = constraint_function_nick,
  lb = c(0.1, 0.1, 0.1), # Lower bounds
  # cass max velocity m/s 
  ub = c(8.64, 8.64, 8.64), # Upper bounds
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

optimal_values_nick <- result_nick$solution
optimal_values_cass <- result_cass$solution

print(optimal_values_nick)
print(optimal_values_cass)
