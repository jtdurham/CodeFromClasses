setwd("C:\\Users\\Jordan\\Documents\\GSE 544")

library(SuppDists)
library(QRM)

### Problem 1

sp_hist = read.csv("F-F_Research_Data_Factors.csv")
sp_hist$Date = as.Date(sp_hist$Date)

month_after_birth = as.Date("1994-02-02")

sp_hist$Mkt.RF = sp_hist$Mkt.RF / 100
sp_hist$RF = sp_hist$RF / 100
sp_hist$Mkt = sp_hist$Mkt.RF + sp_hist$RF

# Calculate the discounted gross rate of return, converting the given 
# percentage point values to decimals

sp_hist$disc_ret = (1 + sp_hist$Mkt) / (1 + sp_hist$RF)

before_birth = sp_hist[which(sp_hist$Date < month_after_birth),]
params = JohnsonFit(before_birth$disc_ret, moment = "quant")

pits = pJohnson(before_birth$disc_ret, params)

t_cop = fit.tcopula(as.matrix(pits), method = "Kendall")
trials = 20000
uniforms = rcopula.t(trials, t_cop$nu, t_cop$P)

market_preds = params$xi + (params$lambda * sinh((qnorm(uniforms, 0, 1) - params$gamma) / params$delta)) 

write.csv(market_preds, "Homework 4 Sim Vals Jordan Durham.csv")

to_plot = read.csv("Monthly Values.csv")
to_plot$Date = as.Date(to_plot$Date)
to_plot = to_plot[1:255,]

plot(to_plot$Date, to_plot$Growth.Optimal, type = "l", col = "blue", xlab = "Date", ylab = "Portfolio Value", main= "Monthly Porfolio Value")
lines(to_plot$Date, to_plot$If.No.Leverage)
legend(x = "topleft", legend = c("No Leverage", "1.05 Leverage Ratio"), fill = c("black", "blue"))

### Problem 2

# Prepare functions for making NPV calculations

discount = function(cashflows, discount_rate, year)
{
  return(cashflows / (1 + discount_rate)^year)
}

facility_npv = function(years, demand, facilities_planned, facility_capacity, 
                        facility_cost, variable_rev, variable_cost, discount_rate, 
                        salvage_value, flexibility = F)
{
  # Instantiate tracking variable storage
  
  fac_built = list()
  cap = list()
  prod_revs = list()
  prod_costs = list()
  cap_inv = list()
  salv = list()
  cashf = list()
  disc_cashf = list()
  
  npv = numeric(nrow(demand))
  
  # For each set of demand values to consider
  
  for(d in 1:nrow(demand))
  {
    # Instantiate tracking variables
    
    facilities_built = numeric(length(years))
    capacity = numeric(length(years))
    product_revenue = numeric(length(years))
    product_costs = numeric(length(years))
    capital_investment = numeric(length(years))
    salvage = numeric(length(years))
    cashflow = numeric(length(years))
    discounted_cashflow = numeric(length(years))
    
    fac_plan = facilities_planned
    
    # For each year
    
    for(y in years)
    {
      # If we are just beginning
      
      if(y == 0)
      {
        # Build a plant for the start of year 1
        
        facilities_built[y+1] = 1
        fac_plan = fac_plan - 1
        
        # Do not allocate production outputs
        
        capacity[y+1] = 0
        product_revenue[y+1] = 0
        product_costs[y+1] = 0
        
        # Allocate the capital investment and do not record salvage
        
        capital_investment[y+1] = facility_cost * facilities_built[y+1]
        salvage[y+1] = 0
        
        # Calculated and discount cashflow
        
        cashflow[y+1] = product_revenue[y+1] - product_costs[y+1] - capital_investment[y+1] + salvage[y+1]
        discounted_cashflow[y+1] = discount(cashflow[y+1], discount_rate, y)
      }
      
      # Otherwise (we are beyond the beginnning)
      
      else
      {
        # Allocate capacity from a facility built at the beginning of the year
        # and carry over previous capacity
        
        capacity[y+1] = capacity[y] + facilities_built[y] * facility_capacity
        
        # If we use a flexible plan
        
        if(flexibility)
        {
          # If demand that occurred this year was less than capacity
          
          if(demand[d, y+1] < capacity[y+1]) #in terms of building a facility for next year, this year is the past year
          {
            # Do not build a plant for next year
            
            build_plant = F
          }
          
          # Otherwise (if demand was greater than or equal to capacity)
          
          else
          {
            # Build a plant if we have more plants planned
            
            build_plant = T
          }
        }
        
        # Otherwise (if our plan is inflexible)
        
        else
        {
          # Build a plant if we have more plants planned
          
          build_plant = T
        }
        
        # If we have more plants planned, our strategy says we can build,
        # and we are not at the end of the last year
        
        if(fac_plan > 0 & build_plant & (y+1) < length(years))
        {
          # Build a plant for next year and adjust the planned facilities remaining
          
          facilities_built[y+1] = 1
          fac_plan = fac_plan - 1
        }
        
        # Otherwise
        
        else
        {
          # Do not build a plant for next year
          
          facilities_built[y+1] = 0
        }
        
        # Caculate variable revenue and cost for satisfied demand
        
        product_revenue[y+1] = min(demand[d, y+1], capacity[y+1]) * variable_rev
        product_costs[y+1] = min(demand[d, y+1], capacity[y+1]) * variable_cost
        
        # Allocate capital investment if we built a facility
        
        capital_investment[y+1] = facility_cost * facilities_built[y+1]
        
        # Obtain salvage value if we are in the last year and
        # a facility was built at the end of the previous year
        
        salvage[y+1] = ifelse(y+1 == length(years) & facilities_built[length(years)-1] >= 1, salvage_value, 0)
        
        # Calculate and discount cashflows
        
        cashflow[y+1] = product_revenue[y+1] - product_costs[y+1] - capital_investment[y+1] + salvage[y+1]
        discounted_cashflow[y+1] = discount(cashflow[y+1], discount_rate, y)
      }
    }
    
    # Store tracking varibles in storage and calculave NPV
    
    fac_built[[d]] = facilities_built
    cap[[d]] = capacity
    prod_revs[[d]] = product_revenue
    prod_costs[[d]] = product_costs
    cap_inv[[d]] = capital_investment
    salv[[d]] = salvage
    cashf[[d]] = cashflow
    disc_cashf[[d]] = discounted_cashflow
    
    npv[d] = sum(discounted_cashflow)
  }
  
  # Output tracking storage and NPV
  
  return(list(fac_built = fac_built, cap = cap, prod_revs = prod_revs,
              prod_costs = prod_costs, cap_inv = cap_inv, salv = salv,
              cashf = cashf, disc_cashf = disc_cashf, npv = npv))
}

## Step 1

# Instantiate demand and set our years

expected_demand = c(0, 300000, 600000, 900000)
demand = matrix(expected_demand, nrow = 1)
years = c(0, 1, 2, 3)

# Plan A

# Instantiate situational variables for this specific plan

facility_capacity = 900000
facility_cost = 900000000
variable_rev = 2000
variable_cost = 1280
discount_rate = 0.09
salvage_value = 0

facilities_planned = 1

# Calculate NPV and get tracking numbers

calculation_a = facility_npv(years, demand, facilities_planned, facility_capacity, 
                                 facility_cost, variable_rev, variable_cost, discount_rate, 
                                 salvage_value)

# Plan B

# Instantiate situational variables for this specific plan

facility_capacity = 300000
facility_cost = 300000000
variable_rev = 2000
variable_cost = 1500
discount_rate = 0.08
salvage_value = 300000000

facilities_planned = 3

# Calculate NPV and get tracking numbers

calculation_b = facility_npv(years, demand, facilities_planned, facility_capacity, 
                           facility_cost, variable_rev, variable_cost, discount_rate, 
                           salvage_value)

## Step 2

# Instantiate the range of possible demand

years = c(0, 1, 2, 3)
expected_demand = c(0, 300000, 600000, 900000)
demand_low = expected_demand - (expected_demand * 0.5)
demand_high = expected_demand + (expected_demand * 0.5)
range = demand_high - demand_low

# Set the number of trials

trials = 10000

# For each year

for(y in years)
{
  # If we are at the beginning
  
  if(y == 0)
  {
    # Simulate demand for the beginning (will all be zero)
    
    sim_demand = runif(trials, 0, 1) * range[y+1] + demand_low[y+1]
  }
  
  # Otherwise (we are beyond the beginning)
  
  else
  {
    # Simulate demand for this respective year
    
    sim_demand = cbind(sim_demand, runif(trials, 0, 1) * range[y+1] + demand_low[y+1])
  }
}

colnames(sim_demand) = c("Year0", "Year1", "Year2", "Year3")

# Plan A

# Instantiate situational variables for this specific plan

facility_capacity = 900000
facility_cost = 900000000
variable_rev = 2000
variable_cost = 1280
discount_rate = 0.09
salvage_value = 0

facilities_planned = 1

# Calculate NPV and get tracking numbers for simulated demand

simulation_a = facility_npv(years, sim_demand, facilities_planned, facility_capacity, 
                             facility_cost, variable_rev, variable_cost, discount_rate, 
                             salvage_value)

# Calculate expected, minimum and maximum NPV from our simulation

mean_npv_a = mean(simulation_a$npv)
min_npv_a = min(simulation_a$npv)
max_npv_a = max(simulation_a$npv)

# Calculate specified NPV percentiles from our simulation

percentiles_a = c(.01, .05, .1, .25, .5, .75, .90, .95, .99)
percentiles_a = quantile(simulation_a$npv, probs = percentiles_a)

# Calculate the probability that we lose on our investment 

prob_neg_a = sum(simulation_a$npv < 0) / length(simulation_a$npv)

# Plot the histogram and CDF of NPV from our simulation

hist(simulation_a$npv, main = "Histogram of Plan A NPVs", xlab = "Plan A NPVs")
plot(ecdf(simulation_a$npv), main = "CDF of Plan A NPVs", xlab = "Plan A NPVs")

# Plan B

# Instantiate situational variables for this specific plan

facility_capacity = 300000
facility_cost = 300000000
variable_rev = 2000
variable_cost = 1500
discount_rate = 0.08
salvage_value = 300000000

facilities_planned = 3

# Calculate NPV and get tracking numbers for simulated demand

simulation_b = facility_npv(years, sim_demand, facilities_planned, facility_capacity, 
                            facility_cost, variable_rev, variable_cost, discount_rate, 
                            salvage_value)

# Calculate expected, minimum and maximum NPV from our simulation

mean_npv_b = mean(simulation_b$npv)
min_npv_b = min(simulation_b$npv)
max_npv_b = max(simulation_b$npv)

# Calculate specified NPV percentiles from our simulation

percentiles_b = c(.01, .05, .1, .25, .5, .75, .90, .95, .99)
percentiles_b = quantile(simulation_b$npv, probs = percentiles_b)

# Calculate the probability that we lose on our investment 

prob_neg_b = sum(simulation_b$npv < 0) / length(simulation_b$npv)

# Plot the histogram and CDF of NPV from our simulation

hist(simulation_b$npv, main = "Histogram of Plan B NPVs", xlab = "Plan B NPVs")
plot(ecdf(simulation_b$npv), main = "CDF of Plan B NPVs", xlab = "Plan B NPVs")

## Step 3

# Plan C

# Instantiate situational variables for this specific plan

facility_capacity = 300000
facility_cost = 300000000
variable_rev = 2000
variable_cost = 1500
discount_rate = 0.08
salvage_value = 300000000

facilities_planned = 3

# Calculate NPV and get tracking numbers for simulated demand

simulation_c = facility_npv(years, sim_demand, facilities_planned, facility_capacity, 
                            facility_cost, variable_rev, variable_cost, discount_rate, 
                            salvage_value, flexibility = T)

# Calculate expected, minimum and maximum NPV from our simulation

mean_npv_c = mean(simulation_c$npv)
min_npv_c = min(simulation_c$npv)
max_npv_c = max(simulation_c$npv)

# Calculate specified NPV percentiles from our simulation

percentiles_c = c(.01, .05, .1, .25, .5, .75, .90, .95, .99)
percentiles_c = quantile(simulation_c$npv, probs = percentiles_c)

# Calculate the probability that we lose on our investment 

prob_neg_c = sum(simulation_c$npv < 0) / length(simulation_c$npv)

# Plot the histogram and CDF of NPV from our simulation

hist(simulation_c$npv, main = "Histogram of Plan C NPVs", xlab = "Plan C NPVs")
plot(ecdf(simulation_c$npv), main = "CDF of Plan C NPVs", xlab = "Plan C NPVs")

## Step 4

# Certain demand

# With certain demand, the correlation matrix does not apply.
# Therefore, Step 1 is complete as is.

# Uncertain demand

# Instantiate the Kendall correlation matrix for our copula

kendall_cor = matrix(c(1, .5, .25, .5, 1, .4, .25, .4, 1), nrow = 3, ncol = 3)

# Instantiate the range of possible demand

years = c(0, 1, 2, 3)
expected_demand = c(0, 300000, 600000, 900000)
demand_low = expected_demand - (expected_demand * 0.5)
demand_high = expected_demand + (expected_demand * 0.5)
range = demand_high - demand_low

# Set the number of trials for our simulation

trials = 10000

# Simulate correlated uniform values and append year 0 to the beginning

sim_demand = rcopula.t(trials, 2, kendall_cor)
sim_demand = cbind(rep(0, nrow(sim_demand)), sim_demand)

# For every year

for(y in years)
{
  # If we are at the beginning
  
  if(y == 0)
  {
    # Do nothing and skip to the next year
    
    next
  }
  
  # Otherwise (if we are beyond the beginning)
  
  else
  {
    # Calculate simulated demand from correlated uniform values
    
    sim_demand[,y+1] = sim_demand[,y+1] * range[y+1] + demand_low[y+1]
  }
}

colnames(sim_demand) = c("Year0", "Year1", "Year2", "Year3")

# Plan A

# Instantiate situational variables for this specific plan

facility_capacity = 900000
facility_cost = 900000000
variable_rev = 2000
variable_cost = 1280
discount_rate = 0.09
salvage_value = 0

facilities_planned = 1

# Calculate NPV and get tracking numbers for simulated correlated demand

correlated_a = facility_npv(years, sim_demand, facilities_planned, facility_capacity, 
                            facility_cost, variable_rev, variable_cost, discount_rate, 
                            salvage_value)

# Calculate expected, minimum and maximum NPV from our simulation

mean_npv_a = mean(correlated_a$npv)
min_npv_a = min(correlated_a$npv)
max_npv_a = max(correlated_a$npv)

# Calculate specified NPV percentiles from our simulation

percentiles_a = c(.01, .05, .1, .25, .5, .75, .90, .95, .99)
percentiles_a = quantile(correlated_a$npv, probs = percentiles_a)

# Calculate the probability that we lose on our investment 

prob_neg_a = sum(correlated_a$npv < 0) / length(correlated_a$npv)

# Plot the histogram and CDF of NPV from our simulation

hist(correlated_a$npv, main = "Histogram of Plan A NPVs", xlab = "Plan A NPVs")
plot(ecdf(correlated_a$npv), main = "CDF of Plan A NPVs", xlab = "Plan A NPVs")

# Plan B

# Instantiate situational variables for this specific plan

facility_capacity = 300000
facility_cost = 300000000
variable_rev = 2000
variable_cost = 1500
discount_rate = 0.08
salvage_value = 300000000

facilities_planned = 3

# Calculate NPV and get tracking numbers for simulated correlated demand

correlated_b = facility_npv(years, sim_demand, facilities_planned, facility_capacity, 
                            facility_cost, variable_rev, variable_cost, discount_rate, 
                            salvage_value)

# Calculate expected, minimum and maximum NPV from our simulation

mean_npv_b = mean(correlated_b$npv)
min_npv_b = min(correlated_b$npv)
max_npv_b = max(correlated_b$npv)

# Calculate specified NPV percentiles from our simulation

percentiles_b = c(.01, .05, .1, .25, .5, .75, .90, .95, .99)
percentiles_b = quantile(correlated_b$npv, probs = percentiles_b)

# Calculate the probability that we lose on our investment 

prob_neg_b = sum(correlated_b$npv < 0) / length(correlated_b$npv)

# Plot the histogram and CDF of NPV from our simulation

hist(correlated_b$npv, main = "Histogram of Plan B NPVs", xlab = "Plan B NPVs")
plot(ecdf(correlated_b$npv), main = "CDF of Plan B NPVs", xlab = "Plan B NPVs")

# Plan C

# Instantiate situational variables for this specific plan

facility_capacity = 300000
facility_cost = 300000000
variable_rev = 2000
variable_cost = 1500
discount_rate = 0.08
salvage_value = 300000000

facilities_planned = 3

# Calculate NPV and get tracking numbers for simulated correlated demand

correlated_c = facility_npv(years, sim_demand, facilities_planned, facility_capacity, 
                            facility_cost, variable_rev, variable_cost, discount_rate, 
                            salvage_value, flexibility = T)

# Calculate expected, minimum and maximum NPV from our simulation

mean_npv_c = mean(correlated_c$npv)
min_npv_c = min(correlated_c$npv)
max_npv_c = max(correlated_c$npv)

# Calculate specified NPV percentiles from our simulation

percentiles_c = c(.01, .05, .1, .25, .5, .75, .90, .95, .99)
percentiles_c = quantile(correlated_c$npv, probs = percentiles_c)

# Calculate the probability that we lose on our investment 

prob_neg_c = sum(correlated_c$npv < 0) / length(correlated_c$npv)

# Plot the histogram and CDF of NPV from our simulation

hist(correlated_c$npv, main = "Histogram of Plan C NPVs", xlab = "Plan C NPVs")
plot(ecdf(correlated_c$npv), main = "CDF of Plan C NPVs", xlab = "Plan C NPVs")