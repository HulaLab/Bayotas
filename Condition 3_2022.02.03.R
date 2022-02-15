library(sn)
library(catIrt)
library(catR)

setwd("~/Documents/Bayotas")
load("~/Documents/Bayotas/theta_sim.RData")
load("~/Documents/Bayotas/simresp_pretx.RData")
load("~/Documents/Bayotas/itpar_pnt.RData")
load("~/Documents/Bayotas/theta_est_pretx_cat30.RData")


theta_est_pretx <- NULL
for(i in 1:1000){
  theta_est_pretx[i] <- catR::eapEst(it = itpar_pnt, x = simresp_pretx$resp[i,1:175],priorPar = c(50,10),
                                     lower = 10, upper = 90)
}

effsize_1 <- 2.22783
itpar_pnt_posttx_3 <- itpar_pnt

theta_sim_condition3_posttx <- theta_sim + effsize_1  #add effsize_1 to thetas + support the item general treatment effect

# disregard lines 24-31
#  simulate post-tx responses for condition 3 
# simresp_posttx_3 <- simIrt( theta = theta_sim[i] + 6.04, params = itpar_pnt_posttx_3[,1:3], mod = c("brm") )
# 
# theta_est_posttx_3 <- NULL
# for(i in 1:1000){
#   theta_est_posttx_3[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_3$resp[i,1:175],priorPar = c(50,10),
#                                         lower = 10, upper = 90)
# } 

# similar to condition 2 and adding a cnst amount to the simulated thetas

itpar_3.20_list <- replicate(1000, itpar_pnt_posttx_3, simplify = FALSE)
for (i in 1:1000){
  theta_target_3 <- theta_sim[i] + 6.04
  itpar_3.20_list[[i]] <- cbind(itpar_3.20_list[[i]], abs(itpar_3.20_list[[i]][,2] - theta_target_3))
  item_select <- order(itpar_3.20_list[[i]][,5])[1:20]
  for(j in 1:20){
    itpar_3.20_list[[i]][item_select[j],2] <-  itpar_3.20_list[[i]][item_select[j],2] - 12.08 
  }
}

simresp_posttx_3.20_list <- list(NULL)
simresp_posttx_3.20_mat <- matrix(data = NA, nrow = 1000, ncol = 175, byrow = TRUE)
for (i in 1:1000){
  simresp_posttx_3.20_list[[i]] <- simIrt( theta = theta_sim_condition3_posttx[i], params = itpar_3.20_list[[i]][,1:3], mod = c("brm") )
  simresp_posttx_3.20_mat[i,] <- simresp_posttx_3.20_list[[i]]$resp
}


theta_est_posttx_3.20 <- NULL
for(i in 1:1000){
  theta_est_posttx_3.20[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_3.20_list[[i]]$resp,
                                           priorPar = c(50,10),
                                           lower = 10, upper = 90)
}

# simulate pnt-cat30 