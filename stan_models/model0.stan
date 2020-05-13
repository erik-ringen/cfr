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
  vector[N] lRR_mean; // mean ln returns ratio (child/adult)
  vector[N] lRR_sd; // sd of ln returns ratio
}

parameters{
  real a_mu; // expected value intercept
  real a_sigma; // sd intercept, log scale
  
  vector[N_outcomes] outcome_z;
  vector[N_studies] study_z;
  vector[N_id] id_z;
  
  real<lower=0> sigma_outcome;
  real<lower=0> sigma_study;
  real<lower=0> sigma_id;
  vector<lower=0,upper=1>[N] age_me; // measurement error on age, constraints will be rescaled in transformed parameter block
}

transformed parameters{
  vector[N] age_merged; // all age values

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
}

model{
  vector[N] mu;
  vector[N] sigma;
  
  a_mu ~ normal(0,1);
  a_sigma ~ normal(0,1);
  
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
  
  // Observation level variance is fixed when sd known
  if (lRR_sd[i] > 0) {
    sigma[i] = lRR_sd[i];
  }
  
  // Otherwise need to parameterize observation-level variance
  else sigma[i] = exp( a_sigma );
  
  // calculate alpha for each obs (mu/scale)
  mu[i] = a_mu + outcome_z[outcome[i]]*sigma_outcome + study_z[study[i]]*sigma_study;
  if (id[i] != -99) mu[i] = mu[i] + id_z[id[i]]*sigma_id;  // adding individual random effect where appropriate
  
  // likelihood for the return ratio
  lRR_mean[i] ~ normal(mu[i],sigma[i]);
  }
}
