#####################################################
#### Skill ~ age plot, broken down by sex ###########

pdf(file = "male_skill_avg.pdf", width = 6, height= 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=1)

plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.3), xlim=c(0,20), ylab="", xlab="", axes=F)
lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=m_col)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha(m_col,0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha(m_col,0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha(m_col,0.05))

axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

## lines to connect different ages ####
# 0 to 5 diff 
lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed",col=m_col, lwd=2)
lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed",col=m_col, lwd=2)

# 5 to 10 diff 
lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed",col=m_col, lwd=2)
lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed",col=m_col, lwd=2)

# 10 to 20 diff
lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed",col=m_col, lwd=2)
lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed",col=m_col, lwd=2)

dev.off()

pdf(file = "female_skill_avg.pdf", width = 6, height= 8)
par(pty='s',
    oma=c(0,0,0,0),
    mai = c(0.5,0.5,0.5,0.5),
    cex=1.3
)

preds_both <- cfr_pred(age=age_seq, resp="S_returns", male=0)

plot(NULL, ylim=c(0,max(apply(preds_both, 2, median))+0.3), xlim=c(0,20), ylab="", xlab="", axes=F)
lines(apply(preds_both, 2, median), x=age_seq, lwd=3, col=f_col)
shade(apply(preds_both, 2, PI, prob=0.9), age_seq, col=col.alpha(f_col,0.05))
shade(apply(preds_both, 2, PI, prob=0.6), age_seq, col=col.alpha(f_col,0.05))
shade(apply(preds_both, 2, PI, prob=0.3), age_seq, col=col.alpha(f_col,0.05))

axis(1, at=c(0,5,10,15,20), tck=-0.02, labels=NA)
axis(1, at=c(0,5,10,15,20), tck=0, lwd=0, line=-0.5)

## lines to connect different ages ####
# 0 to 5 diff 
lines(x = rep(0,2), y = c(0, median(preds_both[,6])), lty="dashed",col=f_col, lwd=2)
lines(x = c(0, 5), y = rep(median(preds_both[,6]),2), lty="dashed",col=f_col, lwd=2)

# 5 to 10 diff 
lines(x = rep(5,2), y = c(median(preds_both[,6]), median(preds_both[,11])), lty="dashed",col=f_col, lwd=2)
lines(x = c(5, 10), y = rep(median(preds_both[,11]),2), lty="dashed",col=f_col, lwd=2)

# 10 to 20 diff
lines(x = rep(10,2), y = c(median(preds_both[,11]), median(preds_both[,21])), lty="dashed",col=f_col, lwd=2)
lines(x = c(10, 20), y = rep(median(preds_both[,21]),2), lty="dashed",col=f_col, lwd=2)

dev.off()

#####################################################
#### Different source of variance ###################

sigma_k <- data.frame(
  sex = post$sigma_sex[,1],
  outcome = post$sigma_outcome[,1],
  outcome_sex = (post$sigma_outcome[,8] + post$sigma_outcome[,15])/2,
  resource = post$sigma_resource[,1],
  resource_sex = (post$sigma_resource[,8] + post$sigma_resource[,15])/2
)

sigma_b <- data.frame(
  sex = post$sigma_sex[,2],
  outcome = post$sigma_outcome[,2],
  outcome_sex = (post$sigma_outcome[,9] + post$sigma_outcome[,16])/2,
  resource = post$sigma_resource[,2],
  resource_sex = (post$sigma_resource[,9] + post$sigma_resource[,16])/2
)

sigma_eta_p <- data.frame(
  sex = post$sigma_sex[,3],
  outcome = post$sigma_outcome[,3],
  outcome_sex = (post$sigma_outcome[,10] + post$sigma_outcome[,17])/2,
  resource = post$sigma_resource[,3],
  resource_sex = (post$sigma_resource[,10] + post$sigma_resource[,17])/2
)

sigma_eta_mu <- data.frame(
  sex = post$sigma_sex[,4],
  outcome = post$sigma_outcome[,4],
  resource = post$sigma_resource[,4],
  samp = 1:n_samps
)

sigma_eta_mu_long <- sigma_eta_mu %>% pivot_longer(-samp)

ggplot(sigma_eta_mu_long, aes(x = value)) + geom_density(aes(color=name))

# 1 = marine
# 2 = game/mixed
# 3 = fruit
# 4 = USOs

k_marine <- exp(post$a[,1] + post$resource_v[,1,1])
k_game <- exp(post$a[,1] + post$resource_v[,2,1])
k_fruit <- exp(post$a[,1] + post$resource_v[,3,1])
k_USO <- exp(post$a[,1] + post$resource_v[,4,1])

b_marine <- exp(post$a[,2] + post$resource_v[,1,2])
b_game <- exp(post$a[,2] + post$resource_v[,2,2])
b_fruit <- exp(post$a[,2] + post$resource_v[,3,2])
b_USO <- exp(post$a[,2] + post$resource_v[,4,2])

eta_marine <- exp(post$a[,4] + post$resource_v[,1,4])
eta_game <- exp(post$a[,4] + post$resource_v[,2,4])
eta_fruit <- exp(post$a[,4] + post$resource_v[,3,4])
eta_USO <- exp(post$a[,4] + post$resource_v[,4,4])
