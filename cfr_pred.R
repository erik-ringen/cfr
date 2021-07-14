#### Convenience function to plot age curves from cfr model ####

pred_fun <- function( outcome=NA, male=0, id=NA, resource=NA, resp="returns", age=14 ) {
  
  if (!is.na(resource)) resource_v <- post$resource_v[,resource,]
  else resource_v <- matrix(0, nrow=n_samps, ncol=14)
  
  if (!is.na(outcome)) sd <- exp(post$a_sd_outcome[,1] + post$a_sd_outcome[,2]*male + resource_v[,7] + resource_v[,14]*male)
  else sd <- exp(post$a_sd_outcome[,1] + post$a_sd_outcome[,2]*male + resource_v[,7] + resource[,14]*male)
  
  if (!is.na(outcome)) outcome_v <- post$outcome_v[,outcome,]
  else outcome_v <- matrix(0, nrow=n_samps, ncol=14)
  
  if (!is.na(id)) id_v <- post$id_v[,id,]
  else id_v <- matrix(0, nrow=n_samps, ncol=2)
  
  n_preds <- length(age)
  
  eta <- matrix(NA, n_samps, 2); k <- rep(NA, n_samps); b <- k
  S <- matrix(NA, n_samps, n_preds)
  mu_p <- matrix(NA, n_samps, n_preds)
  mu_r <- mu_p
  
  k = exp( post$a_k[,1] + post$a_k[,2]*male + outcome_v[,1] + outcome_v[,8]*male + resource_v[,1] + resource_v[,8]*male )
  
  b = exp( post$a_b[,1] + post$a_b[,2]*male + outcome_v[,2] + outcome_v[,9]*male + resource_v[,2] + resource_v[,9]*male )
  
  eta[,1] = exp( post$a_eta[,1,1] + post$a_eta[,2,1]*male + outcome_v[,3] + outcome_v[,9]*male + resource_v[,3] + resource_v[,10]*male )
  eta[,2] = exp( post$a_eta[,1,2] + post$a_eta[,2,2]*male + outcome_v[,4] + outcome_v[,10] + resource_v[,4] + resource_v[,11]*male )
  
  for (n in 1:n_preds) S[,n] = ( 1 - exp(-k * (age[n]/20) ))^b
  
  p = exp( post$a_p[,1] + post$a_p[,2]*male + id_v[,1] + outcome_v[,5] + resource_v[,5] + resource_v[,12]*male)
  
  alpha = exp( post$a_alpha[,1] + post$a_alpha[,2]*male + id_v[,2] + outcome_v[,6] + resource_v[,6] + resource_v[,13]*male )
  
  for (n in 1:n_preds) {
    mu_p[,n] = (S[,n]^eta[,1]) * p
    mu_r[,n] = (S[,n]^eta[,2]) * alpha
  }
  
  if (resp == "S_returns") return( S )
  if (resp == "dim_returns") return(  2*(inv_logit(mu_p) - 0.5) * exp( log(mu_r) + (sd^2)/2) )
  if (resp == "nodim_returns") return( mu_r/alpha )
}

