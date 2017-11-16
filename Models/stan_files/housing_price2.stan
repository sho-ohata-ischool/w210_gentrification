data {
  int N; //the number of observations
  int N_pred; //number of observations for new prediction
  int J; //the number of zip codes
  int K; //number of columns in the model matrix
  int zip[N]; //vector of group indices
  int zip_pred[N_pred]; //vector of prediction group indices
  int boro[N]; //vector of zip to borough
  int B; //number of boroughs
  int water[N]; //vector of waterfront binary variable
  matrix[N,K] X; //the model matrix
  vector[N] y; //the response variable
  matrix[N_pred,K] X_pred; //data being fed in for data we have
}
parameters {
  vector[K] tau; //the standard deviation of the regression coefficients
  vector[B] alpha1; 
  vector[2] alpha2;
  vector[1] delta;
  vector[K] beta_raw[J];
  real sigma; //standard deviation of the individual observations
  real alpha; //intercept
}
transformed parameters {
  vector[N] gamma; 
  vector[K] beta[J]; 
  for(n in 1:N){
    gamma[n] = alpha1[boro[n]] + alpha2[water[n]];
  }
  
  for(j in 1:J){
    beta[j] = alpha + tau .* beta_raw[j];
  }
}

model {
  vector[N] mu; //linear predictor
  //priors
  alpha ~ normal(0,5); //weakly informative priors on the regression coefficients
  tau ~ cauchy(0,2.5); //weakly informative priors, see section 6.9 in STAN user guide
  gamma ~ normal(0,5); //weakly informative priors on the regression coefficients
  alpha1 ~ cauchy(0,2.5); //weakly informative priors, see section 6.9 in STAN user guide
  alpha2 ~ gamma(2,0.1); //weakly informative priors, see section 6.9 in STAN user guide
  sigma ~ gamma(2,0.1); //weakly informative priors, see section 6.9 in STAN user guide
  for(j in 1:J){
    beta_raw[j] ~ normal(0,5); //fill the matrix of group-level regression coefficients 
  }
  //for(j in 1:J){
   //beta[j] ~ normal(gamma,tau); //fill the matrix of group-level regression coefficients 
  //}
  
  for(n in 1:N){
    mu[n] = X[n] * beta[zip[n]] + gamma[n]; //compute the linear predictor using relevant group-level regression coefficients 
  }

  //likelihood
  y ~ normal(mu,sigma);
}

generated quantities { 
  vector [N_pred] y_sim;
  
  for(n in 1:N_pred) {
    y_sim[n] = normal_rng(X_pred[n] * beta[zip_pred[n]] + gamma[n], sigma);
  }
}