data{
  int N; // total num obs
  int N_outcomes; // num distinct outcomes
  int N_id; // num unique individuals that appear
  int outcome[N]; // index of which outcome
  int outcome_var[N]; // index, whether outcome has known variance
  int sex_diff[N]; // index, whether it is possible to estiamte sex diff
  int male[N]; // indicator whether forager is female (0), male (1), or unknown (2 for unknown proportion)
  int id_diff[N]; // index, whether it is possible to estimate individual diff
  int id[N]; // index of individual id
  vector[N] age; // mean age
  vector[N] age_lower; // low-end of age, if given as interval
  vector[N] age_upper; // high-end of age, if given as interval
  vector[N] age_sd; // std. dev of age, if given
  vector[N] returns; // return value, scaled by adult means
  vector[N] lsd_child; // log-normal sd, where appropriate
  vector[N] mu_adult; // adult mean, observation scale
  vector[N] se_adult; // SEM adult, observation sacle
}

parameters{
  // Fixed effects 
  // only partially-pool parameters that are scale-free
  matrix[2,2] a_k; // log-scale intercept for growth rate, sex-specific
  matrix[2,2] a_b; // log-scale intercept for growth elasticity
  matrix[2,2] a_eta; // log-scale intercept for skill elasticity
  
  vector[2] a_p;
  vector[2] a_alpha;
  
  vector<lower=0>[max(outcome_var)] sd_outcome; // lognormal sd of outcome
  
  vector<lower=0>[N] adult_returns;  // avg adult return, parameterized to carry forward uncertainty
  
  // study*outcome-level random effects
  matrix[16,N_outcomes] outcome_z;
  vector<lower=0>[16] sigma_outcome;
  cholesky_factor_corr[16] L_outcome;
  
  // individual-level random effects
  matrix[2,N_id] id_z;
  vector<lower=0>[2] sigma_id;
  cholesky_factor_corr[2] L_id;
  
  // Age meausrement error
  vector<lower=0,upper=1>[N] age_me; // measurement error on age, constraints will be rescaled in transformed parameter block
}

transformed parameters{
  vector[N] age_merged; // all age values
  matrix[N,2] mu_p; // mean vector for prob of non-zero return
  matrix[N,2] mu_r; // mean vector for quantity of returns 
  
  matrix[N_id,2] id_v;
  matrix[N_outcomes,16] outcome_v;

  // scaling and correlating random effects
  outcome_v = (diag_pre_multiply(sigma_outcome, L_outcome) * outcome_z)';
  id_v = (diag_pre_multiply(sigma_id, L_id) * id_z)';

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
  
  ////////////////////////////////////////
  // Model loop /////////////////////////
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
    alpha = exp( a_alpha[1] + a_alpha[2]*(s-1) + outcome_v[outcome[i],ticker + 1] + outcome_v[outcome[i],ticker + 2]*(s-1) );
    }
    
    }
    
    mu_p[i,s] = pow(S[1],eta[1]) * p; 
    mu_r[i,s] = pow(S[2],eta[2]) * alpha;
    }
}

model{
  vector[N] sd_merged;
  
  // Piece together observed and unknown variances
  for (i in 1:N) {
    if (outcome_var[i] > 0) {
      sd_merged[i] = sd_outcome[outcome_var[i]];
    }
    else sd_merged[i] = lsd_child[i];
  }
  
  // Priors ///////////////////////////
  to_vector(a_k) ~ std_normal();
  to_vector(a_b) ~ std_normal();
  to_vector(a_eta) ~ std_normal();
  a_alpha ~ std_normal();
  a_p  ~ std_normal();
  sd_outcome ~ std_normal();
  
  to_vector(outcome_z) ~ std_normal();
  to_vector(id_z) ~ std_normal();
  
  sigma_outcome ~ normal(0,0.5);
  to_vector(sigma_id) ~ normal(0,0.5);
  
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
  if (male[i] == 2) {
  vector[2] lp_p; // log prob for foraging success
  vector[2] lp_r; // log prob for foraging return
    
  if (returns[i] == 0) {
    lp_p[1] = bernoulli_lpmf( 0 | 2*( inv_logit(mu_p[i,1]) - 0.5 ) );   
    lp_p[2] = bernoulli_lpmf( 0 | 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
  }
  
  else if (returns[i] > 0) {
    lp_p[1] = bernoulli_lpmf( 1 | 2*( inv_logit(mu_p[i,1]) - 0.5 ) );   
    lp_p[2] = bernoulli_lpmf( 1 | 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    
    lp_r[1] = lognormal_lpdf( returns[i] | log(mu_r[i,1]), sd_merged[i] );
    lp_r[2] = lognormal_lpdf( returns[i] | log(mu_r[i,2]), sd_merged[i] );
  }
  
  // Mix over male or female with equal prob
  target += log_mix( 0.5, lp_p[1], lp_p[2] );
  target += log_mix( 0.5, lp_r[1], lp_r[2] );
  }
  
  // If sex female
  else if (male[i] == 0) {
    
    if (returns[i] == 0) 0 ~ bernoulli( 2*( inv_logit(mu_p[i,1]) - 0.5 ) );
    else if (returns[i] > 0) {
    
    1 ~ bernoulli( 2*( inv_logit(mu_p[i,1]) - 0.5   ));
    returns[i] ~ lognormal( log(mu_r[i,1]), sd_merged[i] );
    }
  }
  
    // If sex male
  else if (male[i] == 1) {
    
    if (returns[i] == 0) 0 ~ bernoulli( 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    else if (returns[i] > 0) {
      
    1 ~ bernoulli( 2*( inv_logit(mu_p[i,2]) - 0.5   ));
    returns[i] ~ lognormal( log(mu_r[i,2]), sd_merged[i] );
    }
  }
  
} // end likelihood loop
////////////////////////////////////////////////////////
//// Adult returns model //////////////////////////////
  adult_returns ~ normal(mu_adult, se_adult);

} // end model block

generated quantities{
  vector[N] return_ratio;
  
  for (i in 1:N) {
    return_ratio[i] = (returns[i]*mu_adult[i])/adult_returns[i];
  }
}
