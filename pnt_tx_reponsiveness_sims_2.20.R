library(sn)
library(catIrt)
library(catR)



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

# simulate pre-treatment responses to the PNT
simresp_pretx <- simIrt( theta = theta_sim, params = itpar_pnt[,1:3], mod = c("brm") )
mean(simresp_pretx$resp)

theta_est_pretx <- NULL
for(i in 1:1000){
theta_est_pretx[i] <- catR::eapEst(it = itpar_pnt, x = simresp_pretx$resp[i,1:175],priorPar = c(50,10),
                                  lower = 10, upper = 90)
}

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
itpar_pnt_posttx_1[,2] <- itpar_pnt_posttx_1[,2] - effsize_1

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

t.test(x = cond_1_fullpnt_change_scores, mu = 2.22783)
t.test(x = cond_1_cat30_change_scores, mu = 2.22783)
t.test(x = cond_1_fullpnt_change_scores, y = cond_1_cat30_change_scores, mu = 0, paired = T)


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

# For condition 2, pick 20 items that are as close as possible to to 6.04 Tunits < than Tability
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
mean(simresp_posttx_2_mat[,])

theta_est_posttx_2.20 <- NULL
for(i in 1:1000){
  theta_est_posttx_2.20[i] <- catR::eapEst(it = itpar_pnt, x = simresp_posttx_2.20_list[[i]]$resp,
                                        priorPar = c(50,10),
                                        lower = 10, upper = 90)
}


theta_est_posttx_2.20_cat30_list <- list(NULL)
theta_est_posttx_2.20_cat30_mat <- matrix(data = NA, nrow = 1000, ncol = 2, byrow = T)
for (i in 1:1000){
  theta_est_posttx_2.20_cat30_list[[i]] <-
    simulateRespondents(
      thetas = theta_sim[i],
      itemBank = itpar_pnt,
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
      output = c("", "posttx_2.20_cat30_catR", "posttx_2.20_cat30.csv")
    )
  theta_est_posttx_2.20_cat30_mat[i,1] <- theta_est_posttx_2.20_cat30_list[[i]]$thFinal
  theta_est_posttx_2.20_cat30_mat[i,2] <- theta_est_posttx_2.20_cat30_list[[i]]$seFinal
}




save(theta_est_posttx_2.20,file = "theta_est_posttx_2.20.RData")
save(theta_est_posttx_2.20_cat30_mat,file = "theta_est_posttx_2.20_cat30_mat.RData")
save(theta_est_posttx_2.20_cat30_list,file = "theta_est_posttx_2.20_cat30_list.RData")

cond_2.20_fullpnt_change_scores <- theta_est_posttx_2.20 - theta_est_pretx
cond_2.20_cat30_change_scores <- theta_est_posttx_2.20_cat30_mat[,1] - theta_est_pretx_cat30$final.values.df$estimated.theta
t.test(x = cond_2.20_fullpnt_change_scores, mu = 0)
sd(x = cond_2.20_fullpnt_change_scores)
t.test(x = cond_2.20_cat30_change_scores, mu = 0)
sd(x = cond_2.20_cat30_change_scores)
t.test(x = cond_2.20_fullpnt_change_scores, y = cond_2.20_cat30_change_scores, mu = 0)
sd(cond_2.20_fullpnt_change_scores - cond_2.20_cat30_change_scores)

plot(x = theta_est_pretx, y = theta_est_posttx_2.20, ylim = c(10,90), xlim = c(10,90))
plot(x = theta_est_pretx_cat30$final.values.df$estimated.theta,
     y = theta_est_posttx_2.20_cat30_mat[,1], ylim = c(10,90), xlim = c(10,90))
lines(x = c(10,90), y = c(10,90))

plot(x = theta_sim, y = cond_2.20_fullpnt_change_scores, ylim = c(-20,20), xlim = c(10,90))
plot(x = theta_sim, y = cond_2.20_cat30_change_scores, ylim = c(-20,20), xlim = c(10,90))

mean(cond_2.20_fullpnt_change_scores)
mean(cond_2.20_cat30_change_scores)

cor.test(cond_1_fullpnt_change_scores,cond_1_cat30_change_scores)
cor.test(cond_2.20_fullpnt_change_scores,cond_2.20_cat30_change_scores)

t.test(cond_1_fullpnt_change_scores,cond_1_cat30_change_scores, paired = T)
t.test(cond_2.20_fullpnt_change_scores,cond_2.20_cat30_change_scores, paired = T)

summary(lm(cond_1_cat30_change_scores~cond_1_fullpnt_change_scores))

plot(cond_2.20_fullpnt_change_scores,cond_2.20_cat30_change_scores)
lines(x = c(-10,10), y = c(-10*.96972+.01505, 10*.96972+.01505))

mean(theta_est_pretx)
mean(theta_est_posttx_2.20)
sd(theta_est_posttx_2.20)
hist(theta_est_posttx_2.20)

mean(cond_2.20_cat30_change_scores[which(theta_sim<40)])
mean(cond_2.20_cat30_change_scores[which(theta_sim>=40 & theta_sim <= 60)])
mean(cond_2.20_cat30_change_scores[which(theta_sim>60)])

mean(cond_2.20_fullpnt_change_scores[which(theta_sim<40)])
mean(cond_2.20_fullpnt_change_scores[which(theta_sim>=40 & theta_sim <= 60)])
mean(cond_2.20_fullpnt_change_scores[which(theta_sim>60)])

# collect item admin data for post-tx 2.20 CAT30 condition and compare to items
# affected by treatment

n_tx_items_in_posttx_cat30_2.20 <- NULL
for (i in 1:1000){
n_tx_items_in_posttx_cat30_2.20[i] <- length(intersect(theta_est_posttx_2.20_cat30_list[[i]]$testItems,
                   order(itpar_2.20_list[[i]][,5])[1:20]))
  
}

hist(n_tx_items_in_posttx_cat30_2.20, xlab = "NUmber of Treated Items Administered in Post-Tx PNT-CAT30",
     main = NULL)

cat_change_w_n_tx_items <- cbind(cond_2.20_cat30_change_scores,n_tx_items_in_posttx_cat30_2.20)

plot(x = n_tx_items_in_posttx_cat30_2.20, y = cond_2.20_cat30_change_scores, 
     xlab = "NUmber of Treated Items Administered in Post-Tx PNT-CAT30",
     ylab = "PNT-CAT30 Change Score")
cor.test(x = n_tx_items_in_posttx_cat30_2.20, y = cond_2.20_cat30_change_scores)


t.test(cat_change_w_n_tx_items[which(n_tx_items_in_posttx_cat30_2.20 <= 5)])
t.test(cat_change_w_n_tx_items[which(n_tx_items_in_posttx_cat30_2.20 > 5 &
                                       n_tx_items_in_posttx_cat30_2.20 <= 10)])
t.test(cat_change_w_n_tx_items[which(n_tx_items_in_posttx_cat30_2.20> 10 &
                                       n_tx_items_in_posttx_cat30_2.20 <= 15)])
t.test(cat_change_w_n_tx_items[which(n_tx_items_in_posttx_cat30_2.20 > 15)])


# estimate post-tx scores using post-tx cond 2.20 response strings with pretx CAT30 items
theta_est_posttx_2.20_pretx_cat30_items <- NULL
for(i in 1:1000){
  theta_est_posttx_2.20_pretx_cat30_items[i] <- catR::eapEst(it = itpar_pnt, 
                                          x = simresp_posttx_2.20_list[[i]]$resp[as.numeric(theta_est_pretx_cat30$responses.df[i,3:32])],
                                           priorPar = c(50,10),
                                           lower = 10, upper = 90)
}

cond_2.20_cat30_change_scores_pretx_items <- theta_est_posttx_2.20_pretx_cat30_items - 
                                              theta_est_pretx_cat30$final.values.df$estimated.theta
#count treated items in pretx cat30
n_tx_items_in_pretx_cat30_2.20 <- NULL
for (i in 1:1000){
  n_tx_items_in_pretx_cat30_2.20[i] <- length(intersect(as.numeric(theta_est_pretx_cat30$responses.df[i,3:32]),
                                                    order(itpar_2.20_list[[i]][,5])[1:20]))
  
}

cor.test(x = n_tx_items_in_pretx_cat30_2.20, y = cond_2.20_cat30_change_scores_pretx_items)
plot(x = n_tx_items_in_pretx_cat30_2.20, y = cond_2.20_cat30_change_scores_pretx_items)
hist(n_tx_items_in_pretx_cat30_2.20)


t.test(cond_2.20_cat30_change_scores_pretx_items)
t.test(cond_2.20_cat30_change_scores_pretx_items, cond_2.20_fullpnt_change_scores)


plot(cond_2.20_cat30_change_scores_pretx_items, cond_2.20_fullpnt_change_scores)
cor.test(cond_2.20_cat30_change_scores_pretx_items, cond_2.20_fullpnt_change_scores)
lm(cond_2.20_cat30_change_scores_pretx_items ~ cond_2.20_fullpnt_change_scores)
hist(cond_2.20_cat30_change_scores_pretx_items[which(n_tx_items_in_pretx_cat30_2.20==10)])

plot(x = theta_est_pretx_cat30$final.values.df$estimated.theta,
     y = cond_2.20_cat30_change_scores_pretx_items)

plot(x = theta_est_pretx_cat30$final.values.df$true.theta,
     y = cond_2.20_cat30_change_scores_pretx_items)

plot(x = theta_est_pretx,
     y = cond_2.20_fullpnt_change_scores)


t.test(cond_2.20_cat30_change_scores_pretx_items[which(theta_est_pretx_cat30$final.values.df$true.theta>40 &
                                                         theta_est_pretx_cat30$final.values.df$true.theta<60)])

t.test(cond_2.20_cat30_change_scores_pretx_items[which(theta_est_pretx_cat30$final.values.df$true.theta<40)])
t.test(cond_2.20_cat30_change_scores_pretx_items[which(theta_est_pretx_cat30$final.values.df$true.theta>60)])


t.test(cond_2.20_cat30_change_scores_pretx_items, cond_2.20_fullpnt_change_scores)

exp(.182*1.062604)

.57

a = 50-.7
b = 50

.57+.7

exp(.182*(a-b)) / (1 + exp(.182*(a-b)))


theta_est_pretx_cat30$final.values.df$estimated.theta[500]
#21.9
theta_est_posttx_2.10_cat30_list[[500]]$thFinal
#25.4

sort(theta_est_posttx_2.10_cat30_list[[500]]$testItems)
sort(order(itpar_2.10_list[[500]][,5])[1:10])


theta_est_pretx[500]
# 22.2
theta_est_posttx_2.10[500]
# 25.9


cor.test(theta_est_pretx, theta_est_posttx_1)
cor.test(theta_est_pretx_cat30$final.values.df$estimated.theta,
         theta_est_posttx_1_cat30$final.values.df$estimated.theta)


log(92/(175-92))-log(82/(175-82))
82/175
92/175



# pre-tx proportion correct = 0.63
# odds ratio
0.6214343/(1-0.6214343)
# 1.64

# post-tx proportion correct  = 0.67
# odds ratio
0.67452/(1-0.67452)
# 2.07

2.07/1.64
# 1.26


1.64*1.5
#2.46
2.46/(1+2.46)




2.1/12.08


.62*175

108 + 15
123/175
library(brms)

logit_scaled(.325)
-.73

inv_logit_scaled(-.73+.25)

2.2/175*20
.38*175
.325*175
inv_logit_scaled(-2.2)

logit_scaled(.7) - logit_scaled(.675)

mod1p <- function(b, d, p) {
  p <- exp(.182*(b - d)) / (1 + exp(.182*(b - d)))
  return(p)
}

mean(itpar_pnt[,2])
mod1p(b = 50, d = 46.07)*175
mod1p(b = 41.91, d = 46.07)*175 - mod1p(b = 40, d = 46.07)*175
mod1p(b = 61.91, d = 46.07)*175 - mod1p(b = 60, d = 46.07)*175
mod1p(b = 35, d = 46.07)*175 - mod1p(b = 30, d = 46.07)*175




