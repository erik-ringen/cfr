data{
  int N; // total num obs
  int N_outcomes; // num distinct outcomes
  int N_id; // num unique individuals that appear
  int N_resource;
  int outcome[N]; // index of which outcome
  int outcome_var[N]; // index, whether outcome has known variance
  int resource[N];
  real sex[N]; // probability male (sex = 1) forager
  int id[N]; // index of individual id
  vector[N] age; // mean age
  vector[N] age_lower; // low-end of age, if given as interval
  vector[N] age_upper; // high-end of age, if given as interval
  vector[N] age_sd; // std. dev of age, if given
  vector[N] returns; // return value (child)
  vector[N] se_child; // SEM child 
  int child_summary_returns[N]; //
}

parameters{
  // Fixed effects 
  // only partially-pool parameters that are scale-free
  vector[2] a_k; // log-scale intercept for growth rate, sex-specific
  vector[2] a_b; // log-scale intercept for growth elasticity
  matrix[2,2] a_eta; // log-scale intercept for skill elasticity
  
  vector[2] a_p;
  vector[2] a_alpha;
  
  vector[2] a_sd_outcome; // average lognormal sd, with sex diffs
  vector[N_outcomes] sd_outcome; // lognormal sd deviations for speific outcomes
  real<lower=0> sigma_sd_outcome; // std deviation of outcome sd random effects
  
  // study*outcome-level random effects
  matrix[9,N_outcomes] outcome_z;
  vector<lower=0>[9] sigma_outcome;
  cholesky_factor_corr[9] L_outcome;
  
  //resource type random effects
  matrix[9,N_resource] resource_z;
  vector<lower=0>[9] sigma_resource;
  cholesky_factor_corr[9] L_resource;
  
  // individual-level random effects
  matrix[2,N_id] id_z;
  vector<lower=0>[2] sigma_id;
  cholesky_factor_corr[2] L_id;
  
  // Age measurement error
  vector<lower=0,upper=1>[N] age_me; // measurement error on age, constraints will be rescaled in transformed parameter block
}

transformed parameters{
  vector[N] age_merged; // all age values
  matrix[N,2] sd_merged;
  matrix[N,2] mu_p; // mean vector for prob of non-zero return
  matrix[N,2] mu_r; // mean vector for quantity of returns 
  
  matrix[N_id,2] id_v;
  matrix[N_outcomes,9] outcome_v; 
  matrix[N_resource,9] resource_v;
  
  ///////// scaling and correlating random effects /////////////////////////
  outcome_v = (diag_pre_multiply(sigma_outcome, L_outcome) * outcome_z)';
  id_v = (diag_pre_multiply(sigma_id, L_id) * id_z)';
  resource_v = (diag_pre_multiply(sigma_resource, L_resource) * resource_z)';
  
  ///// sds for log-normal returns for each outcome
  for (i in 1:N) 
  for (s in 1:2) {
      if (outcome_var[i] > 0) sd_merged[i,s] = exp(a_sd_outcome[1] + a_sd_outcome[2]*(s-1) + outcome_v[outcome[i],9] + resource_v[resource[i],9]);
      else sd_merged[i,s] = exp(a_sd_outcome[1] + a_sd_outcome[2]*(s-1) + resource_v[resource[i],9]);
    }

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
    k = exp( a_k[1] + a_k[2]*(s-1) + outcome_v[outcome[i],1] + resource_v[resource[i],1] );
    
    // elasticity of growth b
    b = exp( a_b[1] + a_b[2]*(s-1) + outcome_v[outcome[i],2] + resource_v[resource[i],2] );

    // elasticity of skill eta, for prob non-zero harvest and mu return
    eta[1] = exp( a_eta[1,1] + a_eta[2,1]*(s-1) + outcome_v[outcome[i],3] + resource_v[resource[i],3] );
    
    eta[2] = exp( a_eta[1,2] + a_eta[2,2]*(s-1) + outcome_v[outcome[i],4] + resource_v[resource[i],4] );
    
    // Skill, age = age with measurement error
    S = pow( 1 - exp(-k * age_merged[i]), b );
    
    // add individual random effects to alpha_p and alpha_r, where appropriate
    if (id[i] > 0 ) {
    // prob foraging success
    alpha_p = exp( a_p[1] + a_p[2]*(s-1) + id_v[id[i],1] + outcome_v[outcome[i],5] + resource_v[resource[i],5]);
    
    // expected yield
    alpha_r = exp( a_alpha[1] + a_alpha[2]*(s-1) + id_v[id[i],2] + outcome_v[outcome[i],6] + resource_v[resource[i],6]);
    }
    
    else {
    alpha_p = exp( a_p[1] + a_p[2]*(s-1) + outcome_v[outcome[i],5] + resource_v[resource[i],5]);
    
    // expected yield
    alpha_r = exp( a_alpha[1] + a_alpha[2]*(s-1) + outcome_v[outcome[i],6] + resource_v[resource[i],6]);
    }
    
    mu_p[i,s] = pow(S,eta[1]) * alpha_p; 
    mu_r[i,s] = pow(S,eta[2]) * alpha_r;
    } // end loop over sex
  
    } // end loop over obs
/////////////////////////////////////////////////////////////////
    
} // end transformed parameters block

model{
  // Priors ///////////////////////////
  a_k ~ std_normal();
  a_b ~ std_normal();
  to_vector(a_eta) ~ std_normal();
  a_alpha ~ std_normal();
  a_p ~ std_normal();
  a_sd_outcome ~ std_normal();
  sd_outcome ~ std_normal();
  
  to_vector(outcome_z) ~ std_normal();
  to_vector(id_z) ~ std_normal();
  to_vector(resource_z) ~ std_normal();
  
  sigma_outcome ~ exponential(1);
  sigma_resource ~ exponential(1);
  to_vector(sigma_id) ~ exponential(1);
  sigma_sd_outcome ~ exponential(1);
  
  L_outcome ~ lkj_corr_cholesky(2);
  L_id ~ lkj_corr_cholesky(2);
  L_resource ~ lkj_corr_cholesky(2);
  //////////////////////////////////////
  
  // meausrement error model for age, depending on error structure
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
    
  if (returns[i] == 0) {
    lp_p[1] = bernoulli_lpmf( 0 | 1 - 2*( inv_logit(mu_p[i,1]) - 0.5 ) );   
    lp_p[2] = bernoulli_lpmf( 0 | 1 - 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
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
    
    if (returns[i] == 0) 0 ~ bernoulli( 1 - 2*( inv_logit(mu_p[i,1]) - 0.5 ) );
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
    
    if (returns[i] == 0) 0 ~ bernoulli( 1 - 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    else if (returns[i] > 0) {
    
    1 ~ bernoulli( 2*( inv_logit(mu_p[i,2]) - 0.5 ) );
    returns[i] ~ lognormal( log(mu_r[i,2]), sd_merged[i,2] );
    }
    }
  }
  
} // end likelihood loop
////////////////////////////////////////////////////////

} // end model block
