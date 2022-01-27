### Notes ####
# Key for Notes ("#"= start of a new comment; "##" cotinuation of the comment)
# Use Fergadiotis Excel sheet to practice IRT simulation 
# Import excel sheet to R
library("readxl")
read_excel("C:\\Users\\Pauline\\Desktop\\Fergadiotis_et_al._2015_Supplement_2.xlsx")

library(readr)
Fergadiotis_et_al_2015_Supplement_ <- read_csv("Desktop/College/BPhil Materials/Fergadiotis et al. 2015 Supplement .csv")
View(Fergadiotis_et_al_2015_Supplement_) 
# ^above information didn't work; instead go to the lower right panel --> click files--> house icon 
## -->navigate to where the file is located --> import data set 

# Rename Excel File
data = Fergadiotis_et_al_2015_Supplement_
for(i in 1:175){
item_spec[i] <- ifelse(data$`Item Difficulty`[i]>.31, 2.5, 0) 
}
  
data$item_diff_itemspecifictx <- data$`Item Difficulty`- item_spec
library(catIrt)

# Creating parameters 
## create a matrix of 175 1-PL item parameters where
## a = discrimination, set to constant a = 1.25
## b is difficulty, set to 0 for all items
## c is is the guessing (lower asymptote) parameter, set to constant c = 0
## itpart = (item parameter)
## cbind = column binding; takes vectors and binds it to a matrix of 3 arguments 
## rep = repeat 

itpar1 <- cbind(a = data$Discrimination, b = data$`Item Difficulty`, c = 0)
# refer back to Fergadiotis file

# (Practice) create a second matrix of item parameters named itpar2,
# for which all items are easier by 1 logit

itpar2 <- cbind(a = data$Discrimination, b = data$item_diff_itemspecifictx, c = 0)

# I wanted to see if I can create a second matrix of item parameters similar to the irt sim we did before
# code not working for this part because "number of rows of result is not a multiple 
## of vector length (arg 2)" --> is this due to the fact I inputed a decimal number? 
## UPDATE: as of now,  focus on item general 

# define a matrix to hold simulated responses to 175 items 
## by 50 simulated examiness ('simulees')
## itresp = item response 

nsimulees <- 10000 
itresp1 <- matrix(data = NA, nrow = nsimulees, ncol = 175, byrow = T)

# define more similar matrices
itresp2 <- matrix(data = NA, nrow = nsimulees, ncol = 175, byrow = T)

itresp3 <- matrix(data = NA, nrow = nsimulees, ncol = 175, byrow = T)

# simulate responses in two conditions 
# 1: pre-treatment, operationalized by low ability, theta = -0.5
# 2: post-treatment with general effect, operationalized by theta = 0.5 #1 logit difference 


set.seed(512)
for (i in 1:nsimulees) {
  itresp1[i,] <- simIrt(theta = -0.5, params = itpar1, mod = "brm")$resp
  itresp2[i,] <- simIrt(theta = -.165, params = itpar1, mod = "brm")$resp
  itresp3[i,] <- simIrt(theta = -0.5, params = itpar2, mod = "brm")$resp
}
mean(itresp1)
mean(itresp2)
mean(itresp3)
 

# For this part, I wanted to simulate three conditions before doing the catIRT
## code error "number of items to replace is not a multitple of replacement length" 
## is the error not working due to the conditions not being correct? or is my code incomplete? 
## UPDATE: when defining the matrixes, I first put 100 items instead of 175 items, so it was't working

# # create matrices to hold the expected a posteriori (eap) score estimates
# eap_pretx_fullpnt <- matrix(data = NA, nrow = nsimulees, ncol = 1, byrow = T)
# eap_postx_fullpnt <- matrix(data = NA, nrow = nsimulees, ncol = 1, byrow = T)
# eap_pretx_pntcat <- matrix(data = NA, nrow = nsimulees, ncol = 1, byrow = T)
# eap_postx_pntcat <- matrix(data = NA, nrow = nsimulees, ncol = 1, byrow = T)
# 
# # compute the score estimates and fill the matrices with them
# ?eapEst #estimated ability; Bayesian-Model Estimation; Expected-A-Posterior Estimation 
# ##"brm" = fit bayesian generalrized non-linear model 
# 
# eap_pretx_fullpnt <- eapEst(resp = itresp1, params = itpar1, range = c(-5, 5),
#                mod = "brm", ddist = dnorm, mean = 0, sd = 1.48, quad = 33)$theta
# eap_postx_fullpnt <- eapEst(resp = itresp2, params = itpar1, range = c(-5, 5),
#                             mod = "brm", ddist = dnorm, mean = 0, sd = 1.48, quad = 33)$theta
# mean(eap_pretx_fullpnt)
# #-0.482684
# 
# mean(eap_postx_fullpnt)
# #0.531278

#Use the catIRT command to the pretx_pntcat and postx_pntcat 
pretx_pntcat <- catIrt(params = itpar1, mod = "brm",resp = itresp1,
                       catStart = list( n.start = 5, init.theta = 0,
                                        select = c("UW-FI"), #review these terms in catIrt packages 
                                        at = c("theta"),
                                        it.range = NULL, n.select = 1,
                                        delta = .1,
                                        score = c("EAP"),
                                        range = c(-5, 5), 
                                        step.size = 3, leave.after.MLE = FALSE ),
                       catMiddle = list( select = c("UW-FI"),
                                         at = c("theta"),
                                         it.range = NULL, n.select = 1,
                                         delta = .1,
                                         score = c("EAP"),
                                        range = c(-5, 5),
                                         expos = c("none") ),
                       catTerm = list( term = c("fixed"),
                                       score = c("EAP"),
                                       range = c(-5, 5),
                                       n.min = 30, n.max = 30 ),
                       ddist = dnorm, mean = 0, sd = 1.48, quad = 33,
                       progress = TRUE) # does not tell you which items were selected 

# two brackets indicates which individual item within a list
# 
# eapEst(resp = pretx_pntcat$cat_indiv[[1]]$cat_resp, params = itpar1[pretx_pntcat$cat_indiv[[1]]$cat_it,], 
#        range = c(-5, 5),
#        mod = "brm", ddist = dnorm, mean = 0, sd = 1.48, quad = 33)

postx_pntcat_gen <- catIrt(params = itpar1, mod = "brm",resp = itresp2,
                       catStart = list( n.start = 5, init.theta = 0,
                                        select = c("UW-FI"), #review these terms in catIrt packages 
                                        at = c("theta"),
                                        it.range = NULL, n.select = 1,
                                        delta = .1,
                                        score = c("EAP"),
                                        range = c(-5, 5),
                                        step.size = 3, leave.after.MLE = FALSE ),
                       catMiddle = list( select = c("UW-FI"),
                                         at = c("theta"),
                                         it.range = NULL, n.select = 1,
                                         delta = .1,
                                         score = c("EAP"),
                                         range = c(-5, 5),
                                         expos = c("none") ),
                       catTerm = list( term = c("fixed"),
                                       score = c("EAP"),
                                       range = c(-5, 5),
                                       n.min = 30, n.max = 30 ),
                       ddist = dnorm, mean = 0, sd = 1.48, quad = 33,
                       progress = TRUE )

postx_pntcat_spec <- catIrt(params = itpar1, mod = "brm",resp = itresp3,
                           catStart = list( n.start = 5, init.theta = 0,
                                            select = c("UW-FI"), #review these terms in catIrt packages 
                                            at = c("theta"),
                                            it.range = NULL, n.select = 1,
                                            delta = .1,
                                            score = c("EAP"),
                                            range = c(-5, 5),
                                            step.size = 3, leave.after.MLE = FALSE ),
                           catMiddle = list( select = c("UW-FI"),
                                             at = c("theta"),
                                             it.range = NULL, n.select = 1,
                                             delta = .1,
                                             score = c("EAP"),
                                             range = c(-5, 5),
                                             expos = c("none") ),
                           catTerm = list( term = c("fixed"),
                                           score = c("EAP"),
                                           range = c(-5, 5),
                                           n.min = 30, n.max = 30 ),
                           ddist = dnorm, mean = 0, sd = 1.48, quad = 33,
                           progress = TRUE )



##Homework: do the same thing for the postx_pnt cat & compare eap for both treatments 

eap_pretx_pntcat <- pretx_pntcat$cat_theta
eap_pretx_fullpnt <- pretx_pntcat$tot_theta

eap_postx_pntcat_gen <- postx_pntcat_gen$cat_theta
eap_postx_fullpnt_gen <- postx_pntcat_gen$tot_theta

eap_postx_pntcat_spec <- postx_pntcat_spec$cat_theta
eap_postx_fullpnt_spec <- postx_pntcat_spec$tot_theta


mean(pretx_pntcat$tot_theta)
mean(postx_pntcat_gen$tot_theta)

mean(pretx_pntcat$cat_theta)
mean(postx_pntcat_gen$cat_theta)

mean(postx_pntcat_spec$cat_theta)
mean(postx_pntcat_spec$tot_theta)

chg_full_gen <- eap_postx_fullpnt_gen - eap_pretx_fullpnt
chg_cat_gen <- eap_postx_pntcat_gen - eap_pretx_pntcat
chg_full_spec <- eap_postx_fullpnt_spec - eap_pretx_fullpnt
chg_cat_spec <- eap_postx_pntcat_spec - eap_pretx_pntcat

mean(chg_full_gen)
mean(chg_cat_gen)
mean(chg_full_spec)
mean(chg_cat_spec)
sd(chg_full_gen)
sd(chg_cat_gen)
sd(chg_full_spec)
sd(chg_cat_spec)


t.test(chg_full_gen, chg_cat_gen, paired = T)
t.test(chg_full_spec, chg_cat_spec, paired = T)


plot(eap_pretx_fullpnt,chg_cat_gen)
cor.test(eap_pretx_fullpnt,chg_cat_gen)

plot(eap_pretx_fullpnt,chg_full_gen)
cor.test(eap_pretx_fullpnt,chg_full_gen)

plot(eap_pretx_pntcat,chg_full_gen)
cor.test(eap_pretx_pntcat,chg_full_gen)

plot(eap_pretx_fullpnt,chg_full_spec)
cor.test(eap_pretx_fullpnt,chg_full_spec)

plot(eap_pretx_fullpnt,chg_cat_spec)
cor.test(eap_pretx_fullpnt,chg_cat_spec)

plot(eap_pretx_pntcat,chg_full_spec)
cor.test(eap_pretx_pntcat,chg_full_spec)


hist(chg_full_gen, xlim = c(-2,2), ylim = c(0,2000))
hist(chg_cat_gen, xlim = c(-2,2), ylim = c(0,2000))


hist(chg_full_spec, xlim = c(-2,2), ylim = c(0,2000))
hist(chg_cat_spec, xlim = c(-2,2), ylim = c(0,2000))
