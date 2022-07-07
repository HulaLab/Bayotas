

plot(x = theta_sim,y = cond_2.20_cat30_change_scores, xlab = "True Naming Ability", ylab = "PNT-CAT30 Change Score")
plot(x = theta_sim,y = cond_2.20_fullpnt_change_scores ,xlab = "True Naming Ability", ylab = "Full PNT Change Score")
plot(x = theta_sim,y = (cond_2.20_cat30_change_scores-cond_2.20_fullpnt_change_scores),
     xlab = "True Naming Ability", ylab = "PNT-CAT30 - Full PNT Change Score Difference")



plot(x = theta_sim,y = cond_1_cat30_change_scores, xlab = "True Naming Ability", ylab = "PNT-CAT30 Change Score")
plot(x = theta_sim,y = cond_1_fullpnt_change_scores ,xlab = "True Naming Ability", ylab = "Full PNT Change Score")
plot(x = theta_sim,y = (cond_1_cat30_change_scores-cond_1_fullpnt_change_scores),
     xlab = "True Naming Ability", ylab = "PNT-CAT30 - Full PNT Change Score Difference")




plot(x = theta_est_pretx_cat30$final.values.df$estimated.theta,y = cond_2.20_cat30_change_scores)
plot(x = theta_est_pretx,y = cond_2.20_fullpnt_change_scores)


cor.test(x = theta_sim,y = cond_2.20_cat30_change_scores)
cor.test(x = theta_sim,y = cond_2.20_fullpnt_change_scores)


cor.test(x = theta_sim,y = cond_1_cat30_change_scores, xlab = "True Naming Ability", ylab = "PNT-CAT30 Change Score")
cor.test(x = theta_sim,y = cond_1_fullpnt_change_scores ,xlab = "True Naming Ability", ylab = "Full PNT Change Score")
cor.test(x = theta_sim,y = (cond_1_cat30_change_scores-cond_1_fullpnt_change_scores),
     xlab = "True Naming Ability", ylab = "PNT-CAT30 - Full PNT Change Score Difference")

plot(x = theta_sim, y = n_tx_items_in_pretx_cat30_2.20, 
     xlab = "True Naming Ability", ylab = "Number of Treated Items in Pre-Treatment PNT-CAT30")
plot(x = theta_sim, y = n_tx_items_in_posttx_cat30_2.20, 
     xlab = "True Naming Ability", ylab = "Number of Treated Items in Post-Treatment PNT-CAT30")
