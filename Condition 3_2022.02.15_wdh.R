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
# simulate fullpnt
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
theta_est_posttx_3.20_cat30_mat <- matrix(data = NA, nrow = 1000, ncol = 2, byrow = T)
theta_est_posttx_3.20_cat30_list <- list(NULL)
for (i in 1:1000){
  theta_est_posttx_3.20_cat30_list[[i]] <-
    simulateRespondents(
      thetas = theta_sim_condition3_posttx[i],
      itemBank = itpar_pnt[,1:4],
      responsesMatrix = simresp_posttx_3.20_list[[i]]$resp,
      model = NULL,
      genSeed = NULL,
      cbControl = NULL,
      rmax = 1,
      Mrmax = "restricted",
      start = list(
        fixItems = NULL,
        seed = NULL,
        nrItems = 1,
        theta = 50,
        D = 1,
        randomesque = 1,
        random.seed = NULL,
        startSelect = "MFI",rfbe
        cb.control = FALSE,
        random.cb = NULL
      ),
      test = list(
        method = "EAP",
        priorDist = "norm",
        priorPar = c(50, 10),
        weight = "Huber",
        tuCo = 1,
        sem.type = "classic",
        sem.exact = FALSE,
        se.ase = 10,
        range = c(10, 90),
        D = 1,
        parInt = c(-10, 90, 33),
        itemSelect = "MFI",
        infoType = "observed",
        randomesque = 1,
        random.seed = NULL,
        AP = 1,
        proRule = "length",
        proThr = 30,
        constantPatt = NULL
      ),
      stop = list(rule = "length",
                  thr = 30, alpha = 0.05),
      final = list(
        method = "EAP",
        priorDist = "norm",
        priorPar = c(50, 10),
        weight = "Huber",
        tuCo = 1,
        sem.type = "classic",
        sem.exact = FALSE,
        range = c(10, 90),
        D = 1,
        parInt = c(10, 90, 33),
        alpha = 0.05
      ),
      save.output = F,
      output = c("", "posttx_3_cat30_catR", "posttx_3_cat30.csv")
    )
  theta_est_posttx_3.20_cat30_mat[i,1] <- theta_est_posttx_3.20_cat30_list[[i]]$thFinal
  theta_est_posttx_3.20_cat30_mat[i,2] <- theta_est_posttx_3.20_cat30_list[[i]]$seFinal
}
save.image("~/bayotas bphil/Bayotas/Condition 3_2020.02.15.RData")
mean(theta_est_posttx_3.20)

cond_3.20_fullpnt_change_scores <- theta_est_posttx_3.20 - theta_est_pretx
cond_3.20_cat30_change_scores <- theta_est_posttx_3.20_cat30_mat[,1] - theta_est_pretx_cat30$final.values.df$estimated.theta

t.test(x = cond_3.20_fullpnt_change_scores, mu = 0)
sd(cond_3.20_fullpnt_change_scores)

t.test(x = cond_3.20_cat30_change_scores, mu = 0)
sd(cond_3.20_cat30_change_scores)



