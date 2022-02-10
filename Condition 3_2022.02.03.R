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

itpar_pnt_posttx_3[,2] <-itpar_pnt_posttx_3[,2] - effsize_1

# simulate post-tx responses for condition 3 
simresp_posttx_3 <- simIrt( theta = theta_sim[i] + 6.04, params = itpar_pnt_posttx_3[,1:3], mod = c("brm") )

theta_est_posttx_3 <- NULL
for(i in 1:1000){
  theta_est_posttx_3[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_3$resp[i,1:175],priorPar = c(50,10),
                                        lower = 10, upper = 90)
}

# similar to condition 2 and adding a cnst amount to the simulated thetas; small to large size
# small number of treated items 
itpar_3.05_list <- replicate(1000, itpar_pnt_posttx_3, simplify = FALSE)
for (i in 1:1000){
  theta_target_2.0 <- theta_sim[i] + 6.04 #because I already added thetas to the previous one would i need to also put this jere?
  itpar_3.05_list[[i]] <- cbind(itpar_3.05_list[[i]], abs(itpar_3.05_list[[i]][,2] - theta_target_2.0))
  item_select <- order(itpar_3.05_list[[i]][,5])[1:5]
  for(j in 1:5){
    itpar_3.05_list[[i]][item_select[j],2] <-  itpar_3.05_list[[i]][item_select[j],2] - 12.08 
  }
}

simresp_posttx_3.05_list <- list(NULL)
simresp_posttx_.05_mat <- matrix(data = NA, nrow = 1000, ncol = 175, byrow = TRUE)
for (i in 1:1000){
  simresp_posttx_3.05_list[[i]] <- simIrt( theta = theta_sim[i], params = itpar_2.20_list[[i]][,1:3], mod = c("brm") )
  simresp_posttx_3.05_mat[i,] <- simresp_posttx_3.05_list[[i]]$resp
}

theta_est_posttx_3.05 <- NULL
for(i in 1:1000){
  theta_est_posttx_3.05[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_3.05_list[[i]]$resp,
                                           priorPar = c(50,10),
                                           lower = 10, upper = 90)
}

# medium number of treated items 
itpar_3.10_list <- replicate(1000, itpar_pnt_posttx_3, simplify = FALSE)
for (i in 1:1000){
  theta_target_2.1 <- theta_sim[i] + 6.04
  itpar_3.10_list[[i]] <- cbind(itpar_3.10_list[[i]], abs(itpar_3.10_list[[i]][,2] - theta_target_2.1))
  item_select <- order(itpar_3.10_list[[i]][,5])[1:10]
  for(j in 1:10){
    itpar_3.10_list[[i]][item_select[j],2] <-  itpar_3.10_list[[i]][item_select[j],2] - 12.08 
  }
}

simresp_posttx_3.10_list <- list(NULL)
simresp_posttx_3.10_mat <- matrix(data = NA, nrow = 1000, ncol = 175, byrow = TRUE)
for (i in 1:1000){
  simresp_posttx_3.10_list[[i]] <- simIrt( theta = theta_sim[i], params = itpar_3.10_list[[i]][,1:3], mod = c("brm") )
  simresp_posttx_3.10_mat[i,] <- simresp_posttx_3.10_list[[i]]$resp
}

theta_est_posttx_3.10 <- NULL
for(i in 1:1000){
  theta_est_posttx_3.10[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_3.10_list[[i]]$resp,
                                           priorPar = c(50,10),
                                           lower = 10, upper = 90)
}

# large number of treated items 
itpar_3.15_list <- replicate(1000, itpar_pnt_posttx_3, simplify = FALSE)
for (i in 1:1000){
  theta_target_2.2 <- theta_sim[i] + 6.04
  itpar_3.15_list[[i]] <- cbind(itpar_3.15_list[[i]], abs(itpar_3.15_list[[i]][,2] - theta_target_2.2))
  item_select <- order(itpar_3.15_list[[i]][,5])[1:5]
  for(j in 1:15){
    itpar_3.15_list[[i]][item_select[j],2] <-  itpar_3.05_list[[i]][item_select[j],2] - 12.08 
  }
}

simresp_posttx_3.15_list <- list(NULL)
simresp_posttx_3.15_mat <- matrix(data = NA, nrow = 1000, ncol = 175, byrow = TRUE)
for (i in 1:1000){
  simresp_posttx_3.15_list[[i]] <- simIrt( theta = theta_sim[i], params = itpar_3.15_list[[i]][,1:3], mod = c("brm") )
  simresp_posttx_3.15_mat[i,] <- simresp_posttx_3.15_list[[i]]$resp
}

theta_est_posttx_3.15 <- NULL
for(i in 1:1000){
  theta_est_posttx_3.15[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_3.15_list[[i]]$resp,
                                           priorPar = c(50,10),
                                           lower = 10, upper = 90)
}

# largest effect (20 items as Max)
itpar_3.20_list <- replicate(1000, itpar_pnt_posttx_3, simplify = FALSE)
for (i in 1:1000){
  theta_target_2.3 <- theta_sim[i] + 6.04
  itpar_3.20_list[[i]] <- cbind(itpar_3.05_list[[i]], abs(itpar_3.20_list[[i]][,2] - theta_target_2.3))
  item_select <- order(itpar_3.20_list[[i]][,5])[1:5]
  for(j in 1:20){
    itpar_3.20_list[[i]][item_select[j],2] <-  itpar_3.20_list[[i]][item_select[j],2] - 12.08 
  }
}

simresp_posttx_3.20_list <- list(NULL)
simresp_posttx_3.20_mat <- matrix(data = NA, nrow = 1000, ncol = 175, byrow = TRUE)
for (i in 1:1000){
  simresp_posttx_3.20_list[[i]] <- simIrt( theta = theta_sim[i], params = itpar_3.20_list[[i]][,1:3], mod = c("brm") )
  simresp_posttx_3.20_mat[i,] <- simresp_posttx_3.20_list[[i]]$resp
}

theta_est_posttx_3.20 <- NULL
for(i in 1:1000){
  theta_est_posttx_3.20[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_3.20_list[[i]]$resp,
                                           priorPar = c(50,10),
                                           lower = 10, upper = 90)
}