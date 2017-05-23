trials = 20000

## 1

# Set the parameters of the log-normal distribution of arsenic in water

log_mean_theta = -4.98
log_stdev_theta = 0.95

# Calculate the parameters of the normal distributiosn from the log-normal

mean_theta = exp(log_mean_theta + (log_stdev_theta^2)/2)
stdev_theta = (exp(log_stdev_theta^2)-1)^0.5*mean_theta

# Set the parameters for random variable x

x_mean = mean_theta
x_stdev = 0.14

# Set the parameters for random variable x_bar

n = 140

x_bar_mean = mean_theta
x_bar_stdev = x_stdev / (n^0.5)

# Set up the experiment

safe_level = 0.01 #amount of arsenic in the water that's still safe
# Ho: theta is <= the safe level
# H1: theta is > the safe level

# Determine the precision of our measurement

obs_mean = 0.047266666666
epsilon = 0.1
little_interval_low = obs_mean * (1 - epsilon)
little_interval_high = obs_mean * (1 + epsilon)

# Simulate values of theta and x_bar

theta_sim = numeric(trials)
x_bar_sim = numeric(trials)
mean_in_interval = numeric(trials)
mean_in_r = numeric(trials)

sig_thresh = 0.01
x_bar_rej = qnorm(1-sig_thresh, safe_level, x_bar_stdev)

for(i in 1:trials)
{
  theta_sim[i] = qlnorm(runif(1, 0, 1), log_mean_theta, log_stdev_theta) # q gives the inverse calc
  x_bar_sim[i] = qnorm(runif(1, 0, 1), theta_sim[i], x_bar_stdev)
  mean_in_interval[i] = ifelse(x_bar_sim[i] > little_interval_low & x_bar_sim[i] < little_interval_high, theta_sim[i], NA)
  mean_in_r[i] = ifelse(theta_sim[i] > safe_level & x_bar_sim[i] > x_bar_rej, 1, 0)
}

# Determine the simulated numbers of theta

num_t_given_d = length(which(!is.na(mean_in_interval)))
num_t = length(theta_sim)

# Determine the simulated number of values satisfying the hypotheses

num_sat_ho = sum(ifelse(theta_sim <= safe_level, 1, 0))
num_sat_h1 = num_t - num_sat_ho

# Deterimne the simulated number of values satisfying the hypotheses given the data

num_ho_data = sum(ifelse(mean_in_interval[-which(is.na(mean_in_interval))] <= safe_level, 1, 0))
num_h1_data = num_t_given_d - num_ho_data

# Obtain various priors

p_ho = num_sat_ho / num_t
p_h1 = 1 - p_ho
p_data_given_ho = num_ho_data / num_sat_ho
p_data_given_h1 = num_h1_data / num_sat_h1

# Calculate the odds and Bayes factor

prior_odds = p_h1 / p_ho
bayes_factor = p_data_given_h1 / p_data_given_ho
posterior_odds = prior_odds * bayes_factor
p_h1_given_data = posterior_odds / (1 + posterior_odds)

log_pri = log(prior_odds)
log_bf = log(bayes_factor)
log_pos = log(posterior_odds)

# Final experimental judgments

x_bar_rej_h1 = sum(mean_in_r)
avg_power = x_bar_rej_h1 / num_sat_h1
pre_exp_rej = avg_power / sig_thresh
pre_odds_corr = pre_exp_rej * prior_odds

## 2

trials = 20000

# Set the parameters of the log-normal distribution of mercury in fish

log_mean_theta = -2.12
log_stdev_theta = 1.34

# Calculate the parameters of the normal distribution from the log-normal

mean_theta = exp(log_mean_theta + (log_stdev_theta^2)/2)
stdev_theta = (exp(log_stdev_theta^2)-1)^0.5*mean_theta

# Set the parameters for random variable x

x_mean = mean_theta
x_stdev = 0.028

# Set the parameters for random variable x_bar

n = 30

x_bar_mean = mean_theta
x_bar_stdev = x_stdev / (n^0.5)

# Set up the experiment

safe_level = 0.09 # safe amount of mercury in the tuna
# Ho: theta is <= the safe level
# H1: theta is > the safe level

# Determine the precision of our measurement

obs_mean = 0.052
epsilon = .1
little_interval_low = obs_mean * (1 - epsilon)
little_interval_high = obs_mean * (1 + epsilon)

# Simulate values of theta and x_bar

theta_sim = numeric(trials)
x_bar_sim = numeric(trials)
mean_in_interval = numeric(trials)
mean_in_r = numeric(trials)

sig_thresh = 0.01
x_bar_rej = qnorm(1-sig_thresh, safe_level, x_bar_stdev)

for(i in 1:trials)
{
  theta_sim[i] = qlnorm(runif(1, 0, 1), log_mean_theta, log_stdev_theta) # q gives the inverse calc
  x_bar_sim[i] = qnorm(runif(1, 0, 1), theta_sim[i], x_bar_stdev)
  mean_in_interval[i] = ifelse(x_bar_sim[i] > little_interval_low & x_bar_sim[i] < little_interval_high, theta_sim[i], NA)
  mean_in_r[i] = ifelse(theta_sim[i] > safe_level & x_bar_sim[i] > x_bar_rej, 1, 0)
}

# Determine the simulated numbers of theta

num_t_given_d = length(which(!is.na(mean_in_interval)))
num_t = length(theta_sim)

# Determine the simulated number of values satisfying the hypotheses

num_sat_ho = sum(ifelse(theta_sim <= safe_level, 1, 0))
num_sat_h1 = num_t - num_sat_ho

# Deterimne the simulated number of values satisfying the hypotheses given the data

num_ho_data = sum(ifelse(mean_in_interval[-which(is.na(mean_in_interval))] <= safe_level, 1, 0))
num_h1_data = num_t_given_d - num_ho_data

# Obtain various priors

p_ho = num_sat_ho / num_t
p_h1 = 1 - p_ho
p_data_given_ho = num_ho_data / num_sat_ho
p_data_given_h1 = num_h1_data / num_sat_h1

# Calculate the odds and Bayes factor

prior_odds = p_h1 / p_ho
bayes_factor = p_data_given_h1 / p_data_given_ho
posterior_odds = prior_odds * bayes_factor
p_h1_given_data = posterior_odds / (1 + posterior_odds)

# 3

## a

trials = 1000000

# Ho: theta = 0
# H1: theta > 0 and is U[0, 1]

# Calculate the standard deviation for x_bar

prob_neg = 0.5 #this is probability that theta = 0
x_bar_std = 5 / (100^.5)

# Prepare variables for tracking simulation outputs

theta_sim = numeric(trials)
x_bar_sim = numeric(trials)
z_scores = numeric(trials)

# For each trial

for(i in 1:trials)
{
  
  # Simulate if there will be a negligible effect
  
  if(runif(1) < prob_neg)
  {
    theta_sim[i] = 0
  }
  
  # If the effect is non-negligible, simulate the effect
  
  else
  {
    theta_sim[i] = runif(1)
  }
  
  # Based on theta, simulate x_bar
  
  x_bar_sim[i] = rnorm(1, theta_sim[i], x_bar_std)
}

# Calculate t- (in this case, z-) scores

z_scores = (x_bar_sim - 0) / x_bar_std

# Determine which thetas will have z-scores near p = 0.05

near_t = ifelse(z_scores >= 1.63 & z_scores <= 1.67, 1, 0)
inds_near_t = which(near_t == 1)
theta_near_t = theta_sim[inds_near_t]

# Get the number of thetas with z-scores near p = 0.05
# that are zero and not zero

is_zero = length(which(theta_near_t == 0))
is_not_zero = length(theta_near_t) - is_zero

## b

# Define our rejection region

rej_thres = 1.65

# Find the thetas where the null is true and the null is false

null_true = which(theta_sim == 0)
null_false = which(!(theta_sim == 0))

# For true nulls and false nulls, count the z-scores that lie
# in the rejection region

nt_rej = sum(z_scores[null_true] >= rej_thres)
nf_rej = sum(z_scores[null_false] >= rej_thres)

# Calculate the percentage of true-null thetas that rejected the
# null in the simulation and do the same for false-null thetas

perc_rej_true = nt_rej / length(null_true)
perc_rej_false = nf_rej / length(null_false)

# Calculate the pre-experimental rejection ratio and 
# pre-experimental odds of correct to incorrect rejection

prior_odds = 1
pre_exp_rat = perc_rej_false / perc_rej_true
pre_exp_odds = pre_exp_rat * prior_odds

# 4

# For each theta near p = 0.05, simulate a set of second experiments
# for x_bar

sec_exp = numeric(length(theta_near_t))
for(i in 1:length(theta_near_t))
{
  sec_exp[i] = rnorm(1, theta_near_t[i], x_bar_std)
}

# Cacluate z-scores for the second experimetns and find the
# percentage of those z-scores that lie in the rejection region

sec_z = (sec_exp - 0) / x_bar_std
in_rej = sum(sec_z >= rej_thres)
perc_in_rej = in_rej / length(sec_exp)