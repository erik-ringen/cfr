data{
  int N; // total num obs
  int N_studies; // num studies
  int N_outcomes; // num distinct outcomes
  int N_id; // num unique individuals that appear
  int outcome[N]; // index of which outcome
  int study[N]; // index of study
  int id[N]; // index of individual id
  vector[N] age; // mean age
  vector[N] age_lower; // low-end of age, if given as interval
  vector[N] age_upper; // high-end of age, if given as interval
  vector[N] age_sd; // std. dev of age, if given
  vector[N] RR_mean; // mean returns ratio (child/adult)
  vector[N] RR_sd; // sd of returns ratio
  int N_RR_sd; // number of obs with known sd on RR
  int RR_sd_ind[N]; // index of those obs
}

parameters{
  real a_mu; // expected value intercept, log-scale
  real a_scale; // scale intercept, log-scale
  
  vector[N_outcomes] outcome_z;
  vector[N_studies] study_z;
  vector[N_id] id_z;
  
  real<lower=0> sigma_outcome;
  real<lower=0> sigma_study;
  real<lower=0> sigma_id;
  vector<lower=0,upper=1>[N] age_me; // measurement error on age, constraints will be rescaled in transformed parameter block
  vector<lower=0>[N_RR_sd] RR_me; // measurement error on RR
}

transformed parameters{
  vector[N] age_merged; // all age values
  vector[N] RR_merged; // all RR values
  
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
  
  // add measurement error to RR where appropriate
  RR_merged = RR_mean;
  for (i in 1:N) {
    if (RR_sd_ind[i] > 0) RR_merged[i] = RR_me[ RR_sd_ind[i] ];
  }
}

model{
  vector[N] alpha;
  vector[N] beta;
  
  a_mu ~ normal(0,1);
  a_scale ~ normal(0,1);
  
  outcome_z ~ std_normal();
  study_z ~ std_normal();
  id_z ~ std_normal();
  
  sigma_outcome ~ exponential(1);
  sigma_study ~ exponential(1);
  sigma_id ~ exponential(1);
  
  // priors on age, depending on error structure
  for (i in 1:N) {
    if (age_lower[i] != -99) age_merged[i] ~ uniform(age_lower[i], age_upper[i]);
    else if (age_sd[i] != -99) age_merged[i] ~ normal(age[i], age_sd[i]);
    else age_merged[i] ~ normal(age[i], 0.5/20); // generic age error when no other info available
  }
  
  ////// Model loop for RR outcome /////////////
  for (i in 1:N) {
  
  // beta is fixed when the RR variance is known
  if (RR_sd_ind[i] > 0) {
    beta[i] = pow( RR_mean[i] / square(RR_sd[i]) , 1/3 );
  }
  
  // Otherwise beta estimated
  else beta[i] = 1 / exp(a_scale);
  
  // calculate alpha for each obs (mu/scale)
  {
  real mu;
  mu = a_mu + outcome_z[outcome[i]]*sigma_outcome + study_z[study[i]]*sigma_study;
  if (id[i] != -99) mu = mu + id_z[id[i]]*sigma_id;  
  
  alpha[i] = exp(mu) / beta[i];
  }
  
  // likelihood for the return ratio
  RR_mean[i] ~ gamma(alpha[i],beta[i]);
  }
}

