library(catIrt)
#simulate item parameters and population parameters 

#simulate a sample of ability estimates 
theta_sim <- rnorm(n = 100, mean = 0, sd = 1)
hist(theta_sim)
