# color palette to denote resources
resource_cols <- c("#046C9A", "#CB2313", "#0C775E", "#EBCC2A") 

# pub-friendly labels
resource_names <- c("Marine", "Game/Mixed", "Fruit", "USOs") 

# sex-coded colors
f_col <- "slategray"
m_col <- "orange"

# generic age sequence to make curves, one element for each age
age_seq <- seq(from=0, to=20, length.out = 21)

#### Convenience function to plot age curves from cfr model ####
cfr_pred <- function( outcome=NA, male=NA, id=NA, resource=NA, resp="returns", age=14 ) {
  
  ## female dummy var
  male <- ifelse(is.na(male), 0, male)
  female <- ifelse(male == 0, 1, 
                   ifelse(is.na(male), 0, 0))
  
  ## replace random effects with 0 if marginalizing over them
  if (!is.na(resource)) resource_v <- post$resource_v[,resource,]
  else resource_v <- matrix(0, nrow=n_samps, ncol=21)
  
  ## same 
  if (!is.na(outcome)) outcome_v <- post$outcome_v[,outcome,]
  else outcome_v <- matrix(0, nrow=n_samps, ncol=21)
  
  ## same
  if (!is.na(id)) id_v <- post$id_v[,id,]
  else id_v <- matrix(0, nrow=n_samps, ncol=2)
  
  ## log normal sd
  sd <- exp( post$a[,7] + post$a_sd_outcome[,1]*post$sigma_sex[,7]*female + outcome_v[,7] + outcome_v[,14]*female + outcome_v[,21]*male + post$a_sd_outcome[,2]*post$sigma_sex[,7]*male + resource_v[,7] + resource_v[,14]*female + resource_v[,21]*male) 
  
  # number of ages to predict over
  n_preds <- length(age)
  
  # Set up other parameters
  eta <- matrix(NA, n_samps, 2); k <- rep(NA, n_samps); b <- k
  S <- matrix(NA, n_samps, n_preds)
  mu_p <- matrix(NA, n_samps, n_preds)
  mu_r <- mu_p
  
  ## k
  k = exp( post$a[,1] + post$a_k[,1]*post$sigma_sex[,1]*female + post$a_k[,2]*post$sigma_sex[,1]*male + outcome_v[,1] + outcome_v[,8]*female + outcome_v[,15]*male + resource_v[,1] + resource_v[,8]*female + resource_v[,15] )
  
  ## b
  b = exp( post$a[,2] + post$a_b[,1]*post$sigma_sex[,2]*female + post$a_b[,2]*post$sigma_sex[,2]*male + outcome_v[,2] + outcome_v[,9]*female + outcome_v[,16]*male + resource_v[,2] + resource_v[,9]*female + resource_v[,16] )
  
  ## eta_p
  eta[,1] = exp( post$a[,3] + post$a_eta[,1,1]*post$sigma_sex[,3]*female + post$a_eta[,2,1]*post$sigma_sex[,3]*male + outcome_v[,3] + outcome_v[,10]*female + outcome_v[,17]*male + resource_v[,3] + resource_v[,10]*female + resource_v[,17] )
  
  ## eta_r
  eta[,2] = exp( post$a[,4] + post$a_eta[,1,2]*post$sigma_sex[,4]*female + post$a_eta[,2,2]*post$sigma_sex[,4]*male + outcome_v[,4] + outcome_v[,11]*female + outcome_v[,18]*male + resource_v[,4] + resource_v[,11]*female + resource_v[,18] )
  
  ## Skill
  for (n in 1:n_preds) S[,n] = ( 1 - exp(-k * (age[n]/20) ))^b
  
  p = exp( post$a[,5] + post$a_p[,1]*post$sigma_sex[,5]*female + post$a_p[,2]*post$sigma_sex[,5]*male + outcome_v[,5] + outcome_v[,12]*female + outcome_v[,19]*male + resource_v[,5] + resource_v[,12]*female + resource_v[,19] )
  
  alpha = exp( post$a[,6] + post$a_alpha[,1]*post$sigma_sex[,6]*female + post$a_alpha[,2]*post$sigma_sex[,6]*male + outcome_v[,6] + outcome_v[,13]*female + outcome_v[,20]*male + resource_v[,6] + resource_v[,13]*female + resource_v[,20] )
  
  for (n in 1:n_preds) {
    mu_p[,n] = (S[,n]^eta[,1]) * p
    mu_r[,n] = (S[,n]^eta[,2]) * alpha
  }
  
  # Prob of a non-zero return
  prob_return <- 2*(rethinking::inv_logit(p) - 0.5)

  if (resp == "S_returns") return( S )
  if (resp == "dim_returns") return(  prob_return * exp( log(mu_r) + (sd^2)/2) )
  if (resp == "nodim_returns") return( prob_return * (mu_r/alpha) )
  if (resp == "CoV") {

    y_pred <- matrix(NA, nrow=n_samps, ncol=1000)
    
    for (i in 1:n_samps) {
      for (n in 1:1000) {
        y_pred[i,n] = rbinom(1, 1, prob_return[i]) * rlnorm(1, log(mu_r[i]), sd[i])
      }
    }
      return( apply(y_pred, 1, sd) / exp( log(mu_r) + (sd^2 / 2) ) )
  }
}
