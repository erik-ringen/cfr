data{
  int N; // total num obs
  int N_studies; // num studies
  int N_outcomes; // num distinct outcomes
  int N_id; // num unique individuals that appear
  int outcome[N]; // index of which outcome
  int outcome_var[N]; // index, whether outcome has known variance
  int sex_diff[N]; // index, whether it is possible to estiamte sex diff
  int male[N]; // indicator whether forager is female (0), male (1), or unknown (2 for unknown proportion)
  int id_diff[N]; // index, whether it is possible to estimate individual diff
  int study[N]; // index of study
  int id[N]; // index of individual id
  vector[N] age; // mean age
  vector[N] age_lower; // low-end of age, if given as interval
  vector[N] age_upper; // high-end of age, if given as interval
  vector[N] age_sd; // std. dev of age, if given
  vector[N] returns; // return value, scaled by adult means
  vector[N] lsd_child; // log-normal sd, where appropriate
  vector[N] lmu_adult; // log-normal mean
  vector[N] lsd_adult; // 
  vector[N] mu_adult; // adult mean, observation scale
}

parameters{
  // Fixed effects 
  // only partially-pool parameters that are scale-free
  matrix[2,2] a_k; // log-scale intercept for growth rate, sex-specific
  matrix[2,2] a_b; // log-scale intercept for growth elasticity
  matrix[2,2] a_eta; // log-scale intercept for skill elasticity
  
  vector[N_outcomes] a_return; // expected returns, intercept
  vector[max(sex_diff)] a_return_d; // expected male returns
  
  vector[N_outcomes] a_p; // prob zero-return, intercept
  vector[max(sex_diff)] a_p_d; // male prob zero-return
  
  vector[max(outcome_var)] sd_outcome; // lognormal sd of outcome
  
  real a_mu; // expected value intercept
  real a_sigma; // sd intercept, log scale
  
  // k random effects
  matrix[2,N_outcomes] outcome_k_z;
  matrix[2,N_studies] study_k_z;
  vector<lower=0>[2] sigma_k_study;
  vector<lower=0>[2] sigma_k_outcome;
  cholesky_factor_corr[2] L_k_outcome;
  cholesky_factor_corr[2] L_k_study;
  
  // b random effects
  matrix[2,N_outcomes] outcome_b_z;
  matrix[2,N_studies] study_b_z;
  vector<lower=0>[2] sigma_b_study;
  vector<lower=0>[2] sigma_b_outcome;
  cholesky_factor_corr[2] L_b_outcome;
  cholesky_factor_corr[2] L_b_study;
  
  // eta random effects
  matrix[2,N_outcomes] outcome_eta_z;
  matrix[2,N_studies] study_eta_z;
  vector<lower=0>[2] sigma_eta_study;
  vector<lower=0>[2] sigma_eta_outcome;
  cholesky_factor_corr[2] L_eta_outcome;
  cholesky_factor_corr[2] L_eta_study;
  
  // alpha random effects
  vector[N_id] id_a_z; // make sure to set outcome-specific sds
  vector<lower=0>[max(id_diff)] sigma_a_id;
  
  // Age meausrement error
  vector<lower=0,upper=1>[N] age_me; // measurement error on age, constraints will be rescaled in transformed parameter block
}

transformed parameters{
  vector[N] age_merged; // all age values
  vector[N] sd_merged; // known and unknown variances
  matrix[N,2] mu_p; // mean vector for prob of non-zero return
  matrix[N,2] mu_r; // mean vector for quantity of returns 
  
  matrix[N_outcomes,2] outcome_k_v;
  matrix[N_studies,2] study_k_v;
  
  matrix[N_outcomes,2] outcome_b_v;
  matrix[N_studies,2] study_b_v;
  
  matrix[N_outcomes,2] outcome_eta_v;
  matrix[N_studies,2] study_eta_v;
  
  matrix[N,2] a_return_merged; // sex-specific means for linear model
  matrix[N,2] a_p_merged; // sex-specific zero return prob for linear model

  // scaling and correlating random effects
  outcome_k_v = (diag_pre_multiply(sigma_k_outcome, L_k_outcome) * outcome_k_z[,])';
  study_k_v = (diag_pre_multiply(sigma_k_study, L_k_study) * study_k_z[,])';
  
    outcome_b_v = (diag_pre_multiply(sigma_b_outcome, L_b_outcome) * outcome_b_z[,])';
  study_b_v = (diag_pre_multiply(sigma_b_study, L_b_study) * study_b_z[,])';
  
  outcome_eta_v = (diag_pre_multiply(sigma_eta_outcome, L_eta_outcome) * outcome_eta_z[,])';
  study_eta_v = (diag_pre_multiply(sigma_eta_study, L_eta_study) * study_eta_z[,])';

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
  
  // Piece together observed and unknown variances
  for (i in 1:N) {
    if (outcome_var[i] > 0) {
      sd_merged[i] = sd_outcome[outcome_var[i]];
    }
    else sd_merged[i] = lsd_child[i];
  }
  
  // Sex-specific means, where appropriate
  a_return_merged[,1] = a_return[outcome];
  a_p_merged[,1] = a_p[outcome];
  
  for (i in 1:N) {
    if (sex_diff[i] > 0) {
      a_return_merged[i,2] = a_return_d[sex_diff[i]];
      a_p_merged[i,2] = a_p_d[sex_diff[i]];
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
    k[q] = exp( a_k[s,q] + study_k_v[outcome[i],s] + study_k_v[outcome[i],s]*(q-1) + outcome_k_v[outcome[i],s] + outcome_k_v[outcome[i],s]*(q-1) );
    
    b[q] = exp( a_k[s,q] + study_b_v[outcome[i],s] + study_b_v[outcome[i],s]*(q-1) + outcome_b_v[outcome[i],s] + outcome_b_v[outcome[i],s]*(q-1) );
    
    eta[q] = exp( a_k[s,q] + study_k_v[outcome[i],s] + study_k_v[outcome[i],1]*(q-1) );
    
    S[q] = pow( 1 - exp(-k[q] * age_merged[i]), b[q] );
    
    p = exp( a_p[outcome[i]] + a_p_d[outcome[i]]*(q-1) );
    alpha = exp( a_return[outcome[i]] + a_return_d[outcome[i]] + id_a_z[id[i]]*sigma_a_id[id_diff[i]] );
    }
    
    mu_p[i,s] = pow(S[1],eta[1]) * p; 
    mu_r[i,s] = pow(S[2],eta[2]) * alpha;
    }
}

model{
  // Priors ///////////////////////////
  to_vector(a_k) ~ std_normal();
  to_vector(a_b) ~ std_normal();
  to_vector(a_eta) ~ std_normal();
  to_vector(a_p) ~ std_normal();
  a_return ~ std_normal();
  a_return_d ~ std_normal();
  sd_outcome ~ exponential(1);
  
  to_vector(outcome_k_z) ~ std_normal();
  to_vector(study_k_z) ~ std_normal();
  to_vector(outcome_b_z) ~ std_normal();
  to_vector(study_b_z) ~ std_normal();
  to_vector(outcome_eta_z) ~ std_normal();
  to_vector(study_eta_z) ~ std_normal();
  id_a_z ~ std_normal();
  
  sigma_k_study ~ exponential(1);
  sigma_k_outcome ~ exponential(1);
  sigma_b_study ~ exponential(1);
  sigma_b_outcome ~ exponential(1);
  sigma_eta_study ~ exponential(1);
  sigma_eta_outcome ~ exponential(1);
  sigma_a_id ~ exponential(1);
  
  L_k_outcome ~ lkj_corr_cholesky(2);
  L_k_study ~ lkj_corr_cholesky(2);
  L_b_outcome ~ lkj_corr_cholesky(2);
  L_b_study ~ lkj_corr_cholesky(2);
  L_eta_outcome ~ lkj_corr_cholesky(2);
  L_eta_study ~ lkj_corr_cholesky(2);
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
    
    lp_r[1] = lognormal_lpdf( returns[i] | mu_r[i,1],   )
  }
  
  }
  
  
  
  
  
  // calculate alpha for each obs (mu/scale)
  mu[i] = a_mu + outcome_z[outcome[i]]*sigma_outcome + study_z[study[i]]*sigma_study;
  if (id[i] != -99) mu[i] = mu[i] + id_z[id[i]]*sigma_id;  // adding individual random effect where appropriate
  
  // likelihood, zero-augmented lognormal
  if (returns[i] == 0) 
  
  // likelihood for the returns
  returns[i] ~ lognormal( mu_r[i], sigma[i] );
  }
}

