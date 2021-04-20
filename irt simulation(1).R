#Located in GitHub
##Notes (4/15/21)##
#version control --> a system of keeping track of updates/changes of code; to ensure and keep track of all changes
#github (it's this!)
#set up a github compository; space for this project --> keep track of the changes

library(catIrt)

# create a matrix of 100 1-PL item parameters where
# a is discrimination, set to constant a = 1 ;--> makes it a 1PL model 
# b is difficulty, set to 0 for all items
# c is is the guessing (lower asymptote) parameter, set to constant c = 0
#itpart = (item parameter)
#cbind = column binding; takes vectors and binds it to a matrix of 3 arguments 
#rep = repeat 
itpar1 <- cbind(a = rep(1,100), b = rep(0,100), c = 0)

# create a second matrix of item parameters named itpar2,
# for which half of the items have difficulty = 0 and 
# the remaining half are easier by 2 logits
itpar2 <- cbind(a = rep(1,100), b = c(rep(-2,50), rep(0,50)), c = 0)

# define a matrix to hold simulated responses to 100 items 
# by 200 simulated examiness ('simulees')
#itresp = item response 
itresp1 <- matrix(data = NA, nrow = 200, ncol = 100, byrow = T)

# define 2 more similar matrices
itresp2 <- matrix(data = NA, nrow = 200, ncol = 100, byrow = T)
itresp3 <- matrix(data = NA, nrow = 200, ncol = 100, byrow = T)

# simulate responses in three conditions with static (i.e., non-adaptive)
# assessment in three conditions
# 1: pre-treatment, operationalized by low ability, theta = -1
# 2: post-treatment with general effect, operationalized by theta = 0
# 3: post-treatment with item-specific effect only, operationalized by 
#       theta = -1, and using itpar2, which has half of the items
#       easier by 2 logits
simIrt(theta = -1, params = itpar1, mod = "brm")
for (i in 1:200) {
itresp1[i,] <- simIrt(theta = -1, params = itpar1, mod = "brm")$resp
itresp2[i,] <- simIrt(theta = 0, params = itpar1, mod = "brm")$resp
itresp3[i,] <- simIrt(theta = -1, params = itpar2, mod = "brm")$resp
} #review and manipulate it! Also look at lines 45-59

# create matrices to hold the expected a posteriori (eap) score estimates
eap1 <- matrix(data = NA, nrow = 200, ncol = 1, byrow = T)
eap2 <- matrix(data = NA, nrow = 200, ncol = 1, byrow = T)
eap3 <- matrix(data = NA, nrow = 200, ncol = 1, byrow = T)

# compute the score estimates and fill the matrices with them
eap1 <- eapEst(resp = itresp1, params = itpar1, range = c(-4, 4),
               mod = "brm", ddist = dnorm, mean = 0, sd = 1, quad = 33)$theta
eap2 <- eapEst(resp = itresp2, params = itpar1)$theta
eap3 <- eapEst(resp = itresp3, params = itpar1)$theta

# find the mean of each condition
mean(eap1)
mean(eap2)
mean(eap3)

t.test(x = eap2,y = eap3, alternative = "two.sided")


# nothing well-developed below this line, just stating to think about how we might simulate CAT administration
catgen <- list()
catspec <- list()

for (i in 1:100) {
catgen[i] <- catIrt( itpar1, mod = c("brm"),
        resp = itresp2,
        theta = NULL,
        catStart = list( n.start = 5, init.theta = 0,
                         select = c("UW-FI"),
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
                        n.min = 30, n.max = 50 ),
        ddist = dnorm,
        progress = TRUE )


catspec[i] <- catIrt( itpar1, mod = c("brm"),
                  resp = itresp3,
                  theta = NULL,
                  catStart = list( n.start = 5, init.theta = 0,
                                   select = c("UW-FI"),
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
                                  n.min = 30, n.max = 50 ),
                  ddist = dnorm,
                  progress = TRUE )

}