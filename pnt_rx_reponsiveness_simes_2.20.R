library(sn)
library(catIrt)
library(catR)

load("~/Desktop/R Codes/theta_est_posttx_2.20_cat30_mat.RData")
load("~/Desktop/R Codes/theta_est_posttx_2.20.RData")
load("~/Desktop/R Codes/cat_simulation_output/theta_est_posttx_1_cat30.RData")
load("~/Desktop/R Codes/cat_simulation_output/theta_est_pretx_cat30.RData")
load("~/Desktop/R Codes/theta_est_posttx_2.20_cat30_list.RData")

setwd("~/Desktop/R Codes")
theta_est_mappd_r03 <- read.csv(file  = "thetas_Tscaled_MAPPDn296_R03n39_2021_10_04.csv")


# Load the distribution of 335 T-scaled ability estimates 
# 296 are from the Moss Aphasia Psycholinguistics Project Databased used by Huston
# 39 are from Fergadiotis and Hula's R03 project

theta_est_mappd_r03 <- read.csv(file  = "thetas_Tscaled_MAPPDn296_R03n39_2021_10_04.csv")

# summarized the moments of the distribution 
library(moments)
median(theta_est_mappd_r03$mean)
mean(theta_est_mappd_r03$mean)
sd(theta_est_mappd_r03$mean)
skewness(theta_est_mappd_r03$mean)
kurtosis(theta_est_mappd_r03$mean)

# the distribution is negatively skewed

library(sn) # sn = skewnormal
# Consider a probability distribution with mean value 3, standard deviation 1.2, skewnesss
# 0.8, for which we write
# cp <- c(mean=3, s.d.=1.2, gamma1=0.8)
# or simply
# cp <- c(3, 1.2, 0.8)
# and assume that they refer to some member of the SN family. From here we get the corresponding
# DP vector with
# dp <- cp2dp(cp, family="SN")
# If we print the content of dp, this is the outcome
# R> dp
# xi omega alpha
# 1.523 1.903 4.190
# Now we can sample 20, say, random values from this distribution with
# y <- rsn(20, dp=dp)

cp <- c(mean = 50.01, s.d. = 9.942, gamma1 = -0.6235)
dp <- cp2dp(cp, family="SN")
theta_sim <- rsn(n = 1000, dp=dp)

# check and compare the moments of the original empirical distribution
# to the new randomly generated one
mean(theta_sim)
mean(theta_est_mappd_r03$mean)

median(theta_sim)
median(theta_est_mappd_r03$mean)

sd(theta_sim)
sd(theta_est_mappd_r03$mean)

skewness(theta_sim)
skewness(theta_est_mappd_r03$mean)

kurtosis(theta_sim)
kurtosis(theta_est_mappd_r03$mean)

hist(theta_sim, xlim = c(0,100), ylim = c(0,200))
hist(theta_est_mappd_r03$mean, xlim = c(0,100), ylim = c(0,200))

min(theta_sim) 
min(theta_est_mappd_r03$mean) 
max(theta_sim) 
max(theta_est_mappd_r03$mean) 
theta_sim <- sort(theta_sim)
####################################################################
# load the T-scaled item parameters
load("~itpar_pnt.RData")
setwd("~/Desktop/R Codes")
load("/Users/pauline/Desktop/R Codes/itpar_pnt.RData")

# simulate pre-treatment responses to the PNT
simresp_pretx <- simIrt( theta = theta_sim, params = itpar_pnt[,1:3], mod = c("brm") )
mean(simresp_pretx$resp)

theta_est_pretx <- NULL
for(i in 1:1000){
theta_est_pretx[i] <- catR::eapEst(it = itpar_pnt, x = simresp_pretx$resp[i,1:175],priorPar = c(50,10),
                                  lower = 10, upper = 90)
}
# 

t.test(x = theta_est_pretx, y = theta_sim, paired = TRUE)
# p < 0.05; true difference in means is not equal to 0
plot(x = theta_est_pretx, y = theta_sim)
cor(x = theta_est_pretx, y = theta_sim) #0.9896458

mean(theta_est_pretx)
mean(theta_sim) 


theta_est_pretx_cat30 <-
simulateRespondents(
  thetas = theta_sim,
  itemBank = itpar_pnt,
  responsesMatrix = simresp_pretx$resp,
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
    startSelect = "MFI",
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
  save.output = TRUE,
  output = c("", "pretx_cat30_catR", "pretx_cat30.csv")
)

# simulate post-treatment responses under the assumption of an item-general treatment effect
# Based on PNT change in Gravier et al. (2021), assume OR = 1.5 = T-score  = 2.22783
effsize_1 <- 2.22783
itpar_pnt_posttx_1 <- itpar_pnt
itpar_pnt_posttx_1[,2] <-itpar_pnt_posttx_1[,2] - effsize_1

simresp_posttx_1 <- simIrt( theta = theta_sim, params = itpar_pnt_posttx_1[,1:3], mod = c("brm") )
mean(simresp_posttx_1$resp)

theta_est_posttx_1 <- NULL
for(i in 1:1000){
  theta_est_posttx_1[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_1$resp[i,1:175],priorPar = c(50,10),
                                     lower = 10, upper = 90)
}

theta_est_posttx_1_cat30 <-
  simulateRespondents(
    thetas = theta_sim,
    itemBank = itpar_pnt,
    responsesMatrix = simresp_posttx_1$resp,
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
      startSelect = "MFI",
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
    save.output = TRUE,
    output = c("", "posttx_1_cat30_catR", "posttx_1_cat30.csv")
  )


cond_1_fullpnt_change_scores <- theta_est_posttx_1 - theta_est_pretx
cond_1_cat30_change_scores <- theta_est_posttx_1_cat30$estimatedThetas - theta_est_pretx_cat30$estimatedThetas

hist(cond_1_fullpnt_change_scores)
hist(cond_1_cat30_change_scores)
mean(cond_1_fullpnt_change_scores>=2.22783)
mean(cond_1_cat30_change_scores>1)

mean(cond_1_cat30_change_scores)
mean(cond_1_fullpnt_change_scores)
sd(cond_1_cat30_change_scores)
sd(cond_1_fullpnt_change_scores)

t.test(x = cond_1_fullpnt_change_scores, mu = 0)
t.test(x = cond_1_cat30_change_scores, mu = 0)
t.test(x = cond_1_fullpnt_change_scores, y = cond_1_cat30_change_scores, mu = 0, paired = T)
mean(x = cond_1_fullpnt_change_scores, y = cond_1_cat30_change_scores)
sd(x = cond_1_fullpnt_change_scores, y = cond_1_cat30_change_scores)

library(BEST)
best_cond_1_fullpnt <- BESTmcmc(y1 = cond_1_fullpnt_change_scores, parallel = TRUE)
best_cond_1_cat30 <- BESTmcmc(y1 = cond_1_cat30_change_scores, parallel = TRUE)
best_cond_1_fullpnt_v_cat30 <- BESTmcmc(y1 = cond_1_fullpnt_change_scores,
                                        y2 = cond_1_cat30_change_scores, parallel = T)
mean(best_cond_1_cat30$mu<2.22783)
plot(best_cond_1_fullpnt)
plot(best_cond_1_cat30)
plot(best_cond_1_fullpnt_v_cat30)

plot(cond_1_cat30_change_scores, cond_1_fullpnt_change_scores)
hist(cond_1_cat30_change_scores)
hist(itpar_pnt[,2])

# For condition 2, pick 10 items that are as close as possible to to 6.04 Tunits < than Tability
# yields 25% correct
# for post-tx item difficulty, subtract 12.08 from treated items, corresponds to ~ 2.1 logit decrease
# in difficulty, corresponds to 25% pretx increasing to 75% post-tx

itpar_2.20_list <- replicate(1000, itpar_pnt, simplify=FALSE)
for (i in 1:1000){
  theta_target <- theta_sim[i] + 6.04
  itpar_2.20_list[[i]] <- cbind(itpar_2.20_list[[i]], abs(itpar_2.20_list[[i]][,2] - theta_target))
  item_select <- order(itpar_2.20_list[[i]][,5])[1:20]
  for(j in 1:20){
    itpar_2.20_list[[i]][item_select[j],2] <-  itpar_2.20_list[[i]][item_select[j],2] - 12.08 
  }
}

itpar_T30.70_list <- replicate(5, itpar_pnt, simplify=FALSE)
theta_tars <- c(30,40,50,60,70)
for (i in 1:5){
  theta_target <- theta_tars[i] + 6.04
  itpar_T30.70_list[[i]] <- cbind(itpar_T30.70_list[[i]], abs(itpar_T30.70_list[[i]][,2] - theta_target))
  item_select <- order(itpar_T30.70_list[[i]][,5])[1:20]
  #for(j in 1:20){
  #  itpar_T30.70_list[[i]][item_select[j],2] <-  itpar_T30.70_list[[i]][item_select[j],2] - 12.08 
  }
#}

itpar_t50_example <- itpar_T30.70_list[[3]]

# get item names
itpar_pnt_2015 <- read.csv(file = "itpar_pnt_2015.csv")
# sanity check
cor(itpar_pnt_2015$Item.Difficulty, itpar_t50_example[,2])
#
itpar_t50_example <- data.frame(cbind(itpar_pnt_2015$Item, itpar_t50_example))

itpar_t50_example$a <- as.numeric(itpar_t50_example$a)
itpar_t50_example$b <- as.numeric(itpar_t50_example$b)
itpar_t50_example$c <- as.numeric(itpar_t50_example$c)
itpar_t50_example$V5 <- as.numeric(itpar_t50_example$V5)
itpar_t50_example$V6 <- as.numeric(itpar_t50_example$V6)
itpar_t50_example <- itpar_t50_example[order(itpar_t50_example[,6]),]
library(catR)
itpar_t50_example$p <- Pi(50,itpar_t50_example[,2:5])[[1]]
write.csv(x = itpar_t50_example[1:20,c(1,3,7)], file = "cond2_example.csv")
itpar_t50_example[1:20,]
mean(itpar_t50_example$p[c(1:20)])


itpar_t50_example$pdiff <- abs(.25 - itpar_t50_example$p) 
ordvec <- itpar_t50_example[order(itpar_t50_example[,8]),]
ordvec[1:20,]
mean(ordvec$p[1:20])
# simulate post-treatment responses for condition 2, completely item-specific treatment effect
# itpar_list object created with 10 items selected to approximate 25% correct at baseline,
# within the limits of the item bank, for each simulee individually
simresp_posttx_2.20_list <- list(NULL)
simresp_posttx_2.20_mat <- matrix(data = NA, nrow = 1000, ncol = 175, byrow = TRUE)
for (i in 1:1000){
  simresp_posttx_2.20_list[[i]] <- simIrt( theta = theta_sim[i], params = itpar_2.20_list[[i]][,1:3], mod = c("brm") )
  simresp_posttx_2.20_mat[i,] <- simresp_posttx_2.20_list[[i]]$resp
}
mean(simresp_pretx$resp)
mean(simresp_posttx_2.20_mat[,])

theta_est_posttx_2.20 <- NULL
for(i in 1:1000){
  theta_est_posttx_2.20[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_2.20_list[[i]]$resp,
                                        priorPar = c(50,10),
                                        lower = 10, upper = 90)
}



theta_est_posttx_2.20_cat30_mat <- matrix(data = NA, nrow = 1000, ncol = 2, byrow = T)
for (i in 1:1000){
  theta_est_posttx_2.20_cat30_list[[i]] <-
    simulateRespondents(
      thetas = theta_sim[i],
      itemBank = itpar_2.20_list[[i]][,1:4],
      responsesMatrix = simresp_posttx_2.20_list[[i]]$resp,
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
        startSelect = "MFI",
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
      output = c("", "posttx_2_cat30_catR", "posttx_2_cat30.csv")
    )
  theta_est_posttx_2.20_cat30_mat[i,1] <- theta_est_posttx_2.20_cat30_list[[i]]$thFinal
  theta_est_posttx_2.20_cat30_mat[i,2] <- theta_est_posttx_2.20_cat30_list[[i]]$seFinal
}

mean(theta_est_posttx_2.20)
cond_2.20_fullpnt_change_scores <- theta_est_posttx_2.20 - theta_est_pretx
cond_2.20_cat30_change_scores <- theta_est_posttx_2.20_cat30_mat[,1] - theta_est_pretx_cat30$final.values.df$estimated.theta

t.test(x = cond_2.20_fullpnt_change_scores, mu = 0)
sd(cond_2.20_fullpnt_change_scores)

t.test(x = cond_2.20_cat30_change_scores, mu = 0)
sd(cond_2.20_cat30_change_scores)

library(effsize)
coh

t.test(x = cond_2.20_fullpnt_change_scores, y = cond_2.20_cat30_change_scores, mu = 0)
sd(x = cond_2.20_fullpnt_change_scores, y = cond_2.20_cat30_change_scores)
mean(x = cond_2.20_fullpnt_change_scores, y = cond_2.20_cat30_change_scores)

plot(x = theta_est_pretx, y = theta_est_posttx_2.20, ylim = c(10,90), xlim = c(10,90))
plot(x = theta_est_pretx_cat30$final.values.df$estimated.theta,
     y = theta_est_posttx_2.20_cat30_mat[,1], ylim = c(10,90), xlim = c(10,90))
lines(x = c(10,90), y = c(10,90))

plot(x = theta_sim, y = cond_2.20_fullpnt_change_scores, ylim = c(-20,20), xlim = c(10,90))
plot(x = theta_sim, y = cond_2.20_cat30_change_scores, ylim = c(-20,20), xlim = c(10,90))

hist(cond_2.20_fullpnt_change_scores)
hist(cond_2.20_cat30_change_scores)

mean(theta_est_pretx)
mean(theta_est_posttx_2.20)
sd(theta_est_posttx_2.20)
hist(theta_est_posttx_2.20)

mean(cond_2.20_cat30_change_scores[which(theta_sim<40)])
?which # gives you the position of the value in a logical vector 
## (can be of anything like rows, columns and even vector as well)
mean(cond_2.20_cat30_change_scores[which(theta_sim>=40 & theta_sim <= 60)])
mean(cond_2.20_cat30_change_scores[which(theta_sim>60)])

mean(cond_2.20_fullpnt_change_scores[which(theta_sim<40)])
mean(cond_2.20_fullpnt_change_scores[which(theta_sim>=40 & theta_sim <= 60)])
mean(cond_2.20_fullpnt_change_scores[which(theta_sim>60)])

theta_est_posttx_2.20_cat30_itpre <- NULL
for(i in 1:1000){
  theta_est_posttx_2.20_cat30_itpre[i] <- catR::eapEst(it = itpar_pnt, 
                                          x = simresp_posttx_2.20_list[[i]]$resp[c(theta_est_pretx_cat30$responses.df[i,3:32])],
                                           priorPar = c(50,10),
                                           lower = 10, upper = 90)
}

as.vector(c(theta_est_pretx_cat30$responses.df[i,3:32]))
theta_est_pretx_cat30$responses.df[1,3:32]


theta_est_posttx_2.20_cat30_list[[1]]$thFinal

theta_est_pretx_cat30$thetas[1]
                      
exp(.182*1.062604)

a = 51
b = 50

exp(.182*(a-b)) / (1 + exp(.182*(a-b)))
