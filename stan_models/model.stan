data{
  int N; // total num obs
  int N_outcomes; // num distinct outcomes
  int N_id; // num unique individuals that appear
  int outcome[N]; // index of which outcome
  int outcome_var[N]; // index, whether outcome has known variance
  int sex_diff[N]; // index, whether it is possible to estiamte sex diff
  real sex[N]; // probability male (sex = 1) forager
  int id_diff[N]; // index, whether it is possible to estimate individual diff
  int id[N]; // index of individual id
  vector[N] age; // mean age
  vector[N] age_lower; // low-end of age, if given as interval
  vector[N] age_upper; // high-end of age, if given as interval
  vector[N] age_sd; // std. dev of age, if given
  vector[N] returns; // return value (child)
  vector[N] sd_child; // std dev of child returns, where appropriate
  vector[N] se_child; // SEM child 
  vector[N] mu_adult; // adult mean, observation scale
  vector[N] sd_adult;
  vector[N] se_adult; // SEM adult, observation sacle
  
  int adult_values_present[N]; // indicator whether adult returns are absent (0) or present, in which case value is an index 1-N_AV
  int N_AV; // number adult values present
  
  int child_summary_returns[N]; //
}

parameters{
  // Fixed effects 
  // only partially-pool parameters that are scale-free
  matrix[2,2] a_k; // log-scale intercept for growth rate, sex-specific
  matrix[2,2] a_b; // log-scale intercept for growth elasticity
  matrix[2,2] a_eta; // log-scale intercept for skill elasticity
  
  vector[2] a_p;
  vector[2] a_alpha;
  
  real a_sd_outcome; // average lognormal sd 
  vector[N_outcomes] sd_outcome; // lognormal sd deviations for speific outcomes
  real<lower=0> sigma_sd_outcome; // std deviation of outcome sd random effects
  
  vector<lower=0>[N_AV] adult_returns;  // avg adult return, parameterized to carry forward uncertainty, zero-inflated
  
  // study*outcome-level random effects
  matrix[16,N_outcomes] outcome_z;
  vector<lower=0>[16] sigma_outcome;
  cholesky_factor_corr[16] L_outcome;
  
  // individual-level random effects
  matrix[2,N_id] id_z;
  vector<lower=0>[2] sigma_id;
  cholesky_factor_corr[2] L_id;
  
  // Age measurement error
  vector<lower=0,upper=1>[N] age_me; // measurement error on age, constraints will be rescaled in transformed parameter block
}

transformed parameters{
  vector[N] age_merged; // all age values
  vector[N] sd_merged;
  matrix[N,2] mu_p; // mean vector for prob of non-zero return
  matrix[N,2] mu_r; // mean vector for quantity of returns 
  
  vector[N] mu_obs; // for summary statistics, need to adjust for zero-deflation
  vector[N] sem_obs; // for summary statistics, need to adjust for zero-deflation  
  vector[N] p_obs;
  
  vector[N] mu_adult_obs; // for summary statistics, need to adjust for zero-deflation
  vector[N] sem_adult_obs; // for summary statistics, need to adjust for zero-deflation  
  
  vector[N] adult_returns_merged; // combination of observed and unobserved adult mean returns
  vector[N] child_returns_merged; // return values, divided by adult avg
  vector[N] scaled_returns;
  
  matrix[N_id,2] id_v;
  matrix[N_outcomes,16] outcome_v; 
  
  ///////// scaling and correlating random effects /////////////////////////
  outcome_v = (diag_pre_multiply(sigma_outcome, L_outcome) * outcome_z)';
  id_v = (diag_pre_multiply(sigma_id, L_id) * id_z)';

  //////////////////////////////////////////////////////////////////////////
  // contraints on age will depend on whether the error is gaussian or interval
  for (i in 1:N) {
    // uniform error
    if (age_lower[i] != -99) {
      age_merged[i] = age_lower[i] + (age_upper[i] - age_lower[i]) * age_me[i];
    }
    // gaussian error is anywhere from 0 to 20 years, thus no need to adjust bounds
    else {
      age_merged[i] = age_me[i];
    }
  }
  
  ////////////////////////////////////////////////////////////////////
  // account for zero-inflation and/or missing adult mean values, based on estimate of zero-return rate for a 20 year old
  for (i in 1:N) {
  for (s in 1:2) {
    
    // s denotes sex, q denotes whether p or return
    real k[2];
    real b[2];
    real eta[2];
    real S[2];
    real p;
    real alpha;
    
    for (q in 1:2) {
      
    int ticker = 0;
    // growth rate k
    k[q] = exp( a_k[s,q] + outcome_v[outcome[i],(ticker + 1 + q - 1)] + outcome_v[outcome[i],(ticker + 3 + q - 1)]*(s-1) );
    ticker = ticker + 4; // update index position
    
    // elasticity of growth
    b[q] = exp( a_b[s,q] + outcome_v[outcome[i],(ticker + 1 + q - 1)] + outcome_v[outcome[i],(ticker + 3 + q - 1)]*(s-1) );
    ticker = ticker + 4; // update index position
    
    // elasticity of skill
    eta[q] = exp( a_eta[s,q] + outcome_v[outcome[i],(ticker + 1 + q - 1)] + outcome_v[outcome[i],(ticker + 3 + q - 1)]*(s-1) );
    ticker = ticker + 4;
    
    // Skill, age 20 = 1
    S[q] = pow( 1 - exp(-k[q] * 1), b[q] );
    
    // add individual random effects, where appropriate
    if (id[i] > 0 ) {
    // prob foraging success
    p = exp( a_p[1] + a_p[2]*(s-1) + id_v[id[i],1] + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1) );
    ticker = ticker + 2;
    
    // expected yield
    alpha = exp( a_alpha[1] + a_alpha[2]*(s-1) + id_v[id[i],2] + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1) );
    }
    
    else {
    p = exp( a_p[1] + a_p[2]*(s-1) + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1) );
    ticker = ticker + 2;
    
    // expected yield
    alpha = exp( a_alpha[1] + a_alpha[2]*(s-1) + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1)     );
    }
    
    }
    
    mu_p[i,s] = pow(S[1],eta[1]) * p; 
    mu_r[i,s] = pow(S[2],eta[2]) * alpha;
    }
  
  
  // When there are no adult return values
  if (mu_adult[i] == -99) {
   
  real adult_returns_adj_f = (1 - 2*( inv_logit(mu_p[i,1]) - 0.5  )) * exp( log(mu_r[i,1]) + square(sd_merged[i])/2 );
  real adult_returns_adj_m = (1 - 2*( inv_logit(mu_p[i,2]) - 0.5  )) * exp( log(mu_r[i,2]) + square(sd_merged[i])/2 );
  
  mu_adult_obs[i] = -99;
  sem_adult_obs[i] = -99;
  
  // Average over sex differences in adult mean returns
  adult_returns_merged[i] = (1 - sex[i])*adult_returns_adj_f + sex[i]*adult_returns_adj_m;
  }
  
  // When there are adult returns, need to Adjust adult means for zero-deflation
  else {
  real var_nz; // variance of non-zero returns
  real n_est; // approximate sample size
  real pr_fail = (1 - 2*( inv_logit(mu_p[i,1]) - 0.5  ))*(1 - sex[i]) + 
      (1 - 2*( inv_logit(mu_p[i,2]) - 0.5  ))*(sex[i]);
      
  n_est = square(sd_adult[i]/se_adult[i]);
      
  mu_adult_obs[i] = mu_adult[i]/(1 - pr_fail);
  var_nz = (square(sd_adult[i])/(1 - pr_fail)) - pr_fail*square(mu_adult_obs[i]);
  sem_adult_obs[i] = sqrt(var_nz/n_est);
  
  adult_returns_merged[i] = adult_returns[adult_values_present[i]];
  }  
  
  }
  ///////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////
  // Model loop for child foragers /////////////////////////
  for (i in 1:N) 
  for (s in 1:2) {
    
    // s denotes sex, q denotes whether p or return
    real k[2];
    real b[2];
    real eta[2];
    real S[2];
    real p;
    real alpha;
    
    for (q in 1:2) {
      
    int ticker = 0;
    // growth rate k
    k[q] = exp( a_k[s,q] + outcome_v[outcome[i],(ticker + 1 + q - 1)] + outcome_v[outcome[i],(ticker + 3 + q - 1)]*(s-1) );
    ticker = ticker + 4; // update index position
    
    // elasticity of growth
    b[q] = exp( a_b[s,q] + outcome_v[outcome[i],(ticker + 1 + q - 1)] + outcome_v[outcome[i],(ticker + 3 + q - 1)]*(s-1) );
    ticker = ticker + 4; // update index position
    
    // elasticity of skill
    eta[q] = exp( a_eta[s,q] + outcome_v[outcome[i],(ticker + 1 + q - 1)] + outcome_v[outcome[i],(ticker + 3 + q - 1)]*(s-1) );
    ticker = ticker + 4;
    
    // Skill
    S[q] = pow( 1 - exp(-k[q] * age_merged[i]), b[q] );
    
    // add individual random effects, where appropriate
    if (id[i] > 0 ) {
    // prob foraging success
    p = exp( a_p[1] + a_p[2]*(s-1) + id_v[id[i],1] + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1) );
    ticker = ticker + 2;
    
    // expected yield
    alpha = exp( a_alpha[1] + a_alpha[2]*(s-1) + id_v[id[i],2] + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1) );
    }
    
    else {
    p = exp( a_p[1] + a_p[2]*(s-1) + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1) );
    ticker = ticker + 2;
    
    // expected yield
    alpha = exp( a_alpha[1] + a_alpha[2]*(s-1) + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1)     );
    }
    
    }
    
    mu_p[i,s] = pow(S[1],eta[1]) * p; 
    mu_r[i,s] = pow(S[2],eta[2]) * alpha;
    }
    
  ///////////////////////////////////////////////////////////////////////
  // Adjust summary statistics for zero-deflation //////////////////////
  for (i in 1:N) {
    
    // if we have individual-level returns, no need for this step
    if (outcome_var[i] > 0) {
      mu_obs[i] = -99;
      sem_obs[i] = -99;
      child_returns_merged[i] = returns[i];
    }
    
    else {
      real var_nz; // variance of non-zero returns
      real n_est; // approximate sample size
      real pr_fail = (1 - 2*( inv_logit(mu_p[i,1]) - 0.5  ))*(1 - sex[i]) + 
      (1 - 2*( inv_logit(mu_p[i,2]) - 0.5  ))*(sex[i]);
      
      n_est = square(sd_child[i]/se_child[i]);
      
      mu_obs[i] = returns[i]/(1 - pr_fail);
      var_nz = (square(sd_child[i])/(1 - pr_fail)) - pr_fail*square(mu_obs[i]);
      sem_obs[i] = sqrt(var_nz/n_est);
      
      child_returns_merged[i] = mu_obs[i];
    }
    
  }
  
  /////////////////////////////////////////////
  //// Scale child returns by adult returns ///
  scaled_returns = child_returns_merged ./ adult_returns_merged;
  
////////////////////////////////////////////////////////////////////// sds for log-normal returns for each outcome
  for (i in 1:N) {
      sd_merged[i] = exp(a_sd_outcome + sd_outcome[outcome[i]]*sigma_sd_outcome);
    }
    
} // end transformed parameters block

model{
  //// Adult returns meausrement error model //////////////////////////////
  for (i in 1:N) {
    if (mu_adult[i] != -99) adult_returns[adult_values_present[i]] ~ normal(mu_adult_obs[i], 0.1);
  }
  
  // Priors ///////////////////////////
  to_vector(a_k) ~ std_normal();
  to_vector(a_b) ~ std_normal();
  to_vector(a_eta) ~ std_normal();
  a_alpha ~ std_normal();
  a_p ~ std_normal();
  
  a_sd_outcome ~ std_normal();
  sigma_sd_outcome ~ exponential(1);
  sd_outcome ~ std_normal();
  
  to_vector(outcome_z) ~ std_normal();
  to_vector(id_z) ~ std_normal();
  
  sigma_outcome ~ exponential(1);
  to_vector(sigma_id) ~ exponential(1);
  
  L_outcome ~ lkj_corr_cholesky(2);
  L_id ~ lkj_corr_cholesky(2);
  //////////////////////////////////////
  
  // priors on age, depending on error structure
  for (i in 1:N) {
    if (age_lower[i] != -99) age_merged[i] ~ uniform(age_lower[i], age_upper[i]);
    else if (age_sd[i] != -99) age_merged[i] ~ normal(age[i], age_sd[i]);
    else age_merged[i] ~ normal(age[i], 0.5/20); // generic age error when no other info available
  }
  
  /////////////////////////////////////
  // model likelihood ////////////////
  for (i in 1:N) {
    
  // If sex unknown, need to mix over the possibilites
  if (sex[i] > 0 && sex[i] < 1) {
  vector[2] lp_p; // log prob for foraging success
  vector[2] lp_r; // log prob for foraging return
    
  if (scaled_returns[i] == 0) {
    lp_p[1] = bernoulli_lpmf( 0 | 2*( inv_logit(mu_p[i,1]) - 0.5 ) );   
    lp_p[2] = bernoulli_lpmf( 0 | 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
  }
  
  else if (scaled_returns[i] > 0) {
    
    lp_p[1] = bernoulli_lpmf( 1 | 2*( inv_logit(mu_p[i,1]) - 0.5 ) );   
    lp_p[2] = bernoulli_lpmf( 1 | 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    
    // If data were given as summary statitics, can only meta-analyze the expected value
    if (child_summary_returns[i] > 0) {
    lp_r[1] = normal_lpdf( scaled_returns[i] | exp(log(mu_r[i,1]) + square(sd_merged[i])/2), sem_obs[i] );
    lp_r[2] = normal_lpdf( scaled_returns[i] | exp(log(mu_r[i,1]) + square(sd_merged[i])/2), sem_obs[i] );
    
    // Mix over male or female in proportion to their probability
    target += log_mix( 1 - sex[i], lp_r[1], lp_r[2] );
    }
    
    // If data were given as individual-level data, use full model
    else {
    lp_r[1] = lognormal_lpdf( scaled_returns[i] | log(mu_r[i,1]), sd_merged[i] );
    lp_r[2] = lognormal_lpdf( scaled_returns[i] | log(mu_r[i,2]), sd_merged[i] );
    
    // Mix over male or female in proportion to their probability
    target += log_mix( 1 - sex[i], lp_p[1], lp_p[2] );
    target += log_mix( 1 - sex[i], lp_r[1], lp_r[2] );
    }
  }
  }
  
  // If sex female
  else if (sex[i] == 0) {
    
    // only summary statistics
    if (child_summary_returns[i] > 0) {
    
    scaled_returns[i] ~ normal( exp(log(mu_r[i,1]) + square(sd_merged)/2), sem_obs[i] );
    }
    
    // individual-level returns
    else {
    
    if (scaled_returns[i] == 0) 0 ~ bernoulli( 2*( inv_logit(mu_p[i,1]) - 0.5 ) );
    else if (scaled_returns[i] > 0) {
    
    1 ~ bernoulli( 2*( inv_logit(mu_p[i,1]) - 0.5   ));
    scaled_returns[i] ~ lognormal( log(mu_r[i,1]), sd_merged[i] );
    }
    }
  }
  
    // If sex male
  else if (sex[i] == 1) {
    
    // only summary statistics
    if (child_summary_returns[i] > 0) {
    
    scaled_returns[i] ~ normal( exp(log(mu_r[i,2]) + square(sd_merged[i])/2), sem_obs[i] );
    }
    
    // individual-level returns
    else {
    
    if (scaled_returns[i] == 0) 0 ~ bernoulli( 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    else if (scaled_returns[i] > 0) {
    
    1 ~ bernoulli( 2*( inv_logit(mu_p[i,2]) - 0.5   ));
    scaled_returns[i] ~ lognormal( log(mu_r[i,2]), sd_merged[i] );
    }
    }
  }
  
} // end likelihood loop
////////////////////////////////////////////////////////

} // end model block
