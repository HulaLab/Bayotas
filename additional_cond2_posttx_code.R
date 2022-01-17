# collect item admin data for post-tx 2.20 CAT30 condition and compare to items
# affected by treatment

n_tx_items_in_posttx_cat30_2.20 <- NULL
for (i in 1:1000){
  n_tx_items_in_posttx_cat30_2.20[i] <- length(intersect(theta_est_posttx_2.20_cat30_list[[i]]$testItems,
                                                         order(itpar_2.20_list[[i]][,5])[1:20]))
  
}

?length

hist(n_tx_items_in_posttx_cat30_2.20, xlab = "Number of Treated Items Administered in Post-Tx PNT-CAT30",
     main = NULL)

cat_change_w_n_tx_items <- cbind(cond_2.20_cat30_change_scores,n_tx_items_in_posttx_cat30_2.20)

plot(x = n_tx_items_in_posttx_cat30_2.20, y = cond_2.20_cat30_change_scores, 
     xlab = "Number of Treated Items Administered in Post-Tx PNT-CAT30", ylab = "PNT-CAT30 Change Score")
cor.test(x = n_tx_items_in_posttx_cat30_2.20, y = cond_2.20_cat30_change_scores)


t.test(cat_change_w_n_tx_items[which(n_tx_items_in_posttx_cat30_2.20 <= 5)])
t.test(cat_change_w_n_tx_items[which(n_tx_items_in_posttx_cat30_2.20 > 5 &
                                       n_tx_items_in_posttx_cat30_2.20 <= 10)])
t.test(cat_change_w_n_tx_items[which(n_tx_items_in_posttx_cat30_2.20> 10 &
                                       n_tx_items_in_posttx_cat30_2.20 <= 15)])
t.test(cat_change_w_n_tx_items[which(n_tx_items_in_posttx_cat30_2.20 > 15)])
?
# > 5 items that are selected are ovverstimated 

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
# p < 0.05; true correlation is not equal to 0
#cor: -0.19 or ~ -0.2 <- weak correlation 
plot(x = n_tx_items_in_pretx_cat30_2.20, y = cond_2.20_cat30_change_scores_pretx_items)
hist(n_tx_items_in_pretx_cat30_2.20)


t.test(cond_2.20_cat30_change_scores_pretx_items)
# p < 0.05; true mean is approximate not equal to 0
t.test(cond_2.20_cat30_change_scores_pretx_items, cond_2.20_fullpnt_change_scores)
