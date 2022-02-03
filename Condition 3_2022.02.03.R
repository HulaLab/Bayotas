library(sn)
library(catIrt)
library(catR)

setwd("~/Desktop/R Codes")
load("/Users/pauline/Desktop/R Codes/itpar_pnt.RData")
cp <- c(mean = 50.01, s.d. = 9.942, gamma1 = -0.6235)
dp <- cp2dp(cp, family="SN")
theta_sim <- rsn(n = 1000, dp=dp)

mean(theta_sim)
sd(theta_sim)
hist(theta_sim)
load("/Users/pauline/Documents/Bayotas/simresp_pretx.RData")
load("/Users/pauline/Documents/Bayotas/theta_sim.RData")

# simulate pre-treatment responses to the PNT
simresp_pretx <- simIrt( theta = theta_sim, params = itpar_pnt[,1:3], mod = c("brm") )

# similar to condition 2 and adding a cnst amount to the simulated thetas; small to large size
# small number of treated items 
itpar_3.05_list <- replicate(1000, itpar_pnt, simplify = FALSE)
for (i in 1:1000){
  theta_target_2.0 <- theta_sim[i] + 6.04
  itpar_3.05_list[[i]] <- cbind(itpar_3.05_list[[i]], abs(itpar_3.05_list[[i]][,2] - theta_target_2.0))
  item_select <- order(itpar_3.05_list[[i]][,5])[1:5]
  for(j in 1:5){
    itpar_3.05_list[[i]][item_select[j],2] <-  itpar_3.05_list[[i]][item_select[j],2] 
  }
}

# medium number of treated items 
itpar_3.10_list <- replicate(1000, itpar_pnt, simplify = FALSE)
for (i in 1:1000){
  theta_target_2.1 <- theta_sim[i] + 6.04
  itpar_3.10_list[[i]] <- cbind(itpar_3.10_list[[i]], abs(itpar_3.10_list[[i]][,2] - theta_target_2.1))
  item_select <- order(itpar_3.10_list[[i]][,5])[1:10]
  for(j in 1:10){
    itpar_3.10_list[[i]][item_select[j],2] <-  itpar_3.05_list[[i]][item_select[j],2] 
  }
}

# large number of treated items 
itpar_3.15_list <- replicate(1000, itpar_pnt, simplify = FALSE)
for (i in 1:1000){
  theta_target_2.2 <- theta_sim[i] + 6.04
  itpar_3.15_list[[i]] <- cbind(itpar_3.15_list[[i]], abs(itpar_3.15_list[[i]][,2] - theta_target_2.2))
  item_select <- order(itpar_3.15_list[[i]][,5])[1:5]
  for(j in 1:15){
    itpar_3.15_list[[i]][item_select[j],2] <-  itpar_3.05_list[[i]][item_select[j],2] 
  }
}

# largest effect (20 items as Max)
itpar_3.20_list <- replicate(1000, itpar_pnt, simplify = FALSE)
for (i in 1:1000){
  theta_target_2.3 <- theta_sim[i] + 6.04
  itpar_3.20_list[[i]] <- cbind(itpar_3.05_list[[i]], abs(itpar_3.20_list[[i]][,2] - theta_target_2.3))
  item_select <- order(itpar_3.20_list[[i]][,5])[1:5]
  for(j in 1:20){
    itpar_3.20_list[[i]][item_select[j],2] <-  itpar_3.20_list[[i]][item_select[j],2] 
  }
}