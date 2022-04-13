data{
  int N; // total num obs
  int N_outcomes; // num distinct outcomes
  int N_id; // num unique individuals that appear
  int N_resource; // num resource categories
  int outcome[N]; // index of which outcome
  int resource[N]; // index of resource types
  real sex[N]; // probability male (sex = 1) forager
  int id[N]; // index of individual id
  vector[N] age; // mean age
  vector[N] age_sd; // std. dev of age, if given
  vector[N] returns; // return value (child)
  vector[N] se_child; // SEM child 
  int child_summary_returns[N]; // indicator of whether data given as summary stat
}

parameters{
  // Fixed effects 
  vector[7] a; // global intercepts, we'll use these for different parameters
  vector[2] a_k; // log-scale intercept for growth rate, sex-specific (index 1 = female, index 2 = male)
  vector[2] a_b; // log-scale intercept for growth elasticity, sex-specific
  matrix[2,2] a_eta; // log-scale intercept for skill elasticity, sex-specific
  
  vector[2] a_p; // zero-return prob, sex-specific
  vector[2] a_alpha; // observation scale intercept, sex-specific
  
  vector[2] a_sd_outcome; // average lognormal sd, with sex diffs
  vector[N_outcomes] sd_outcome; // lognormal sd deviations for speific outcomes
  real<lower=0> sigma_sd_outcome; // std deviation of outcome sd random effects
  vector<lower=0>[7] sigma_sex; // partial pooling for sex diffs
  
  // study*outcome-level random effects
  matrix[21,N_outcomes] outcome_z;
  vector<lower=0>[21] sigma_outcome;
  cholesky_factor_corr[21] L_outcome;
  
  //resource type random effects
  matrix[21,N_resource] resource_z;
  vector<lower=0>[21] sigma_resource;
  cholesky_factor_corr[21] L_resource;
  
  // individual-level random effects on skill
  vector[N_id] id_z;
  real<lower=0> sigma_id;
  
  // Age measurement error
  vector<lower=0,upper=1>[N] age_me;
  
  // pooling of age based on study outcome
  vector[N_outcomes] age_mu;
  vector<lower=0>[N_outcomes] age_sigma; 
}

transformed parameters{
  matrix[N,2] sd_merged; // log-normal sigma
  matrix[N,2] mu_p; // mean vector for prob of non-zero return
  matrix[N,2] mu_r; // mean vector for quantity of returns 
  
  vector[N_id] id_v; // scaled individual random effects
  matrix[N_outcomes,21] outcome_v;  // scaled and correlated outcome random effects
  matrix[N_resource,21] resource_v; // scaled and correlated resource random effects
  
  ///////// scaling and correlating random effects /////////////////////////
  outcome_v = (diag_pre_multiply(sigma_outcome, L_outcome) * outcome_z)';
  id_v = id_z * sigma_id;
  resource_v = (diag_pre_multiply(sigma_resource, L_resource) * resource_z)';
  
  ///// sds for log-normal returns for each outcome
  for (i in 1:N)  {
        sd_merged[i,1] = exp( a[7] + a_sd_outcome[1]*sigma_sex[7] + outcome_v[outcome[i],7] + outcome_v[outcome[i],14] + resource_v[resource[i],7] + resource_v[resource[i],14]);
        
        sd_merged[i,2] = exp( a[7] + a_sd_outcome[2]*sigma_sex[7] + outcome_v[outcome[i],7] + outcome_v[outcome[i],21] + resource_v[resource[i],7] + resource_v[resource[i],21]);
    }
  
//////////////////////////////////////////////////////////
  // Model loop for child foragers /////////////////////////
  // i denotes observation
 for (i in 1:N) {
   // s denotes sex, 
  for (s in 1:2) {
    
    // q denotes whether the model is for non-zero return prob (p) or quantitative return (mu)
    real k;
    real b;
    real eta[2];
    real S;
    real alpha_p;
    real alpha_r;
      
    // growth rate k
    if (s == 1) k = exp( a[1] + a_k[1]*sigma_sex[1] + outcome_v[outcome[i],1] + outcome_v[outcome[i],8] + resource_v[resource[i],1] + resource_v[resource[i],8] );
    if (s == 2) k = exp( a[1] + a_k[2]*sigma_sex[1] + outcome_v[outcome[i],1] + outcome_v[outcome[i],15] + resource_v[resource[i],1] + resource_v[resource[i],15] );

    // elasticity of growth b
    if (s == 1) b = exp( a[2] + a_b[1]*sigma_sex[2] + outcome_v[outcome[i],2] + outcome_v[outcome[i],9] + resource_v[resource[i],2] + resource_v[resource[i],9] );
    if (s == 2) b = exp( a[2] + a_b[2]*sigma_sex[2] + outcome_v[outcome[i],2] + outcome_v[outcome[i],16] + resource_v[resource[i],2] + resource_v[resource[i],16] );

    // elasticity of skill eta, for prob non-zero harvest[1] and mu return[2]
    if (s == 1) eta[1] = exp( a[3] + a_eta[1,1]*sigma_sex[3] + outcome_v[outcome[i],3] + outcome_v[outcome[i],10] + resource_v[resource[i],3] + resource_v[resource[i],10] );
    if (s == 2) eta[1] = exp( a[3] + a_eta[2,1]*sigma_sex[3] + outcome_v[outcome[i],3] + outcome_v[outcome[i],17] + resource_v[resource[i],3] + resource_v[resource[i],17] );
    
    if (s == 1) eta[2] = exp( a[4] + a_eta[1,2]*sigma_sex[4] + outcome_v[outcome[i],4] + outcome_v[outcome[i],11] + resource_v[resource[i],4] + resource_v[resource[i],11] );
    if (s == 2) eta[2] = exp( a[4] + a_eta[2,2]*sigma_sex[4] + outcome_v[outcome[i],4] + outcome_v[outcome[i],18] + resource_v[resource[i],4] + resource_v[resource[i],18] );
    
    // Skill, age = age with measurement error
    S = pow( 1 - exp(-k * age_me[i]), b );
    
    // Add individual random effets, where appropriate
    if (id[i]>0) {
      S = exp( log(S) + id_v[id[i]] );
    }
    
    if (s == 1) alpha_p = exp( a[5] + a_p[1]*sigma_sex[5] + outcome_v[outcome[i],5] + resource_v[resource[i],5] + outcome_v[outcome[i],12] + resource_v[resource[i],12]);
    if (s == 2) alpha_p = exp( a[5] + a_p[2]*sigma_sex[5] + outcome_v[outcome[i],5] + resource_v[resource[i],5] + outcome_v[outcome[i],19] + resource_v[resource[i],19]);
    
    if (s == 1) alpha_r = exp( a[6] + a_alpha[1]*sigma_sex[6] + outcome_v[outcome[i],6] + resource_v[resource[i],6] + outcome_v[outcome[i],13] + resource_v[resource[i],13]);
    if (s == 2) alpha_r = exp( a[6] + a_alpha[2]*sigma_sex[6] + outcome_v[outcome[i],6] + resource_v[resource[i],6] + outcome_v[outcome[i],20] + resource_v[resource[i],20]);

    
    mu_p[i,s] = pow(S,eta[1]) * alpha_p; 
    mu_r[i,s] = pow(S,eta[2]) * alpha_r;
    } // end loop over sex
  
    } // end loop over obs
/////////////////////////////////////////////////////////////////
    
} // end transformed parameters block

model{
  // Priors ///////////////////////////
  a ~ std_normal();
  a_k ~ std_normal();
  a_b ~ std_normal();
  to_vector(a_eta) ~ std_normal();
  a_alpha ~ std_normal();
  a_p ~ std_normal();
  a_sd_outcome ~ std_normal();
  sd_outcome ~ std_normal();
  
  to_vector(outcome_z) ~ std_normal();
  id_z ~ std_normal();
  to_vector(resource_z) ~ std_normal();
  
  // half-normal priors on variance components
  sigma_sex ~ std_normal();
  sigma_outcome ~ std_normal();
  sigma_resource ~ std_normal();
  sigma_id ~ std_normal();
  sigma_sd_outcome ~ std_normal();
  
  L_outcome ~ lkj_corr_cholesky(2);
  L_resource ~ lkj_corr_cholesky(2);
  //////////////////////////////////////
  
  // meausrement error model for age
  age_mu ~ std_normal();
  age_sigma ~ exponential(1);
  
  for (i in 1:N) {
    
    age_me[i] ~ normal( age_mu[outcome[i]], age_sigma[outcome[i]]);
    
    if (age_sd[i] != -99) age[i] ~ normal(age_me[i], age_sd[i]);
    else age[i] ~ normal(age_me[i], 0.5/20); // generic age error when no other info available
  }
  
  /////////////////////////////////////
  // model likelihood ////////////////
  for (i in 1:N) {
    
  // If sex unknown, need to mix over the possibilites
  if (sex[i] > 0 && sex[i] < 1) {
  vector[2] lp_p; // log prob for foraging success
  vector[2] lp_r; // log prob for foraging return
    
  if (returns[i] == 0) {
    lp_p[1] = bernoulli_lpmf( 0 | 2*( inv_logit(mu_p[i,1]) - 0.5 ) );   
    lp_p[2] = bernoulli_lpmf( 0 | 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
  }
  
  else if (returns[i] > 0) {
    
    lp_p[1] = bernoulli_lpmf( 1 | 2*( inv_logit(mu_p[i,1]) - 0.5 ) );   
    lp_p[2] = bernoulli_lpmf( 1 | 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    
    // If data were given as summary statitics, can only meta-analyze the expected value
    if (child_summary_returns[i] > 0) {
    lp_r[1] = normal_lpdf( returns[i] | exp(log(mu_r[i,1]) + square(sd_merged[i,1])/2), se_child[i] );
    lp_r[2] = normal_lpdf( returns[i] | exp(log(mu_r[i,2]) + square(sd_merged[i,2])/2), se_child[i] );
    
    // Mix over male or female in proportion to their probability
    target += log_mix( 1 - sex[i], lp_r[1], lp_r[2] );
    }
    
    // If data were given as individual-level data, use full model
    else {
    lp_r[1] = lognormal_lpdf( returns[i] | log(mu_r[i,1]), sd_merged[i,1] );
    lp_r[2] = lognormal_lpdf( returns[i] | log(mu_r[i,2]), sd_merged[i,2] );
    
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
    
    returns[i] ~ normal( exp(log(mu_r[i,1]) + square(sd_merged[i,1])/2), se_child[i] );
    }
    
    // individual-level returns
    else {
    
    if (returns[i] == 0) 0 ~ bernoulli( 2*( inv_logit(mu_p[i,1]) - 0.5 ) );
    else if (returns[i] > 0) {
    
    1 ~ bernoulli( 2*( inv_logit(mu_p[i,1]) - 0.5   ));
    returns[i] ~ lognormal( log(mu_r[i,1]), sd_merged[i,1] );
    }
    }
  }
  
    // If sex male
  else if (sex[i] == 1) {
    
    // only summary statistics
    if (child_summary_returns[i] > 0) {
    
    returns[i] ~ normal( exp(log(mu_r[i,2]) + square(sd_merged[i,2])/2), se_child[i] );
    }
    
    // individual-level returns
    else {
    
    if (returns[i] == 0) 0 ~ bernoulli( 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    else if (returns[i] > 0) {
    
    1 ~ bernoulli( 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    returns[i] ~ lognormal( log(mu_r[i,2]), sd_merged[i,2] );
    }
    }
  }
  
} // end likelihood loop
////////////////////////////////////////////////////////

} // end model block
