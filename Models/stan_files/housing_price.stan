/*A simple example of an hierarchical model*/
data {
  int N; //the number of observations
  int N_pred; //number of observations for new prediction
  int J; //the number of zip codes
  int K; //number of columns in the model matrix
  int id[N]; //vector of group indices
  int id_pred[N_pred]; //vector of prediction group indices
  matrix[N,K] X; //the model matrix
  vector[N] y; //the response variable
  matrix[n,K] X_pred; //data being fed in for data we have
}
parameters {
  vector[K] gamma; //population-level regression coefficients
  vector[K] tau; //the standard deviation of the regression coefficients

  vector[K] beta_raw[J];
  real sigma; //standard deviation of the individual observations
  real alpha; //intercept
}
transformed parameters {
  vector[K] beta[J]; //matrix of group-level regression coefficients
  //computing the group-level coefficient, based on non-centered parametrization based on section 22.6 STAN (v2.12) user's guide
  for(j in 1:J){
    beta[j] = gamma + tau .* beta_raw[j];
  }
}

model {
  vector[N] mu; //linear predictor
  //priors
  gamma ~ normal(0,5); //weakly informative priors on the regression coefficients
  tau ~ cauchy(0,2.5); //weakly informative priors, see section 6.9 in STAN user guide
  sigma ~ gamma(2,0.1); //weakly informative priors, see section 6.9 in STAN user guide
  for(j in 1:J){
    beta_raw[j] ~ normal(0,1); //fill the matrix of group-level regression coefficients 
  }
  //for(j in 1:J){
   //beta[j] ~ normal(gamma,tau); //fill the matrix of group-level regression coefficients 
  //}
  
  for(n in 1:N){
    mu[n] = X[n] * beta[id[n]] + alpha; //compute the linear predictor using relevant group-level regression coefficients 
  }

  //likelihood
  y ~ normal(mu,sigma);
}

generated quantities { 
  vector y_sim[N_pred]
  for(n in 1:N_pred) {
    y_sim[n] = X_pred[n] * beta[id_pred[n]] + alpha;
  }
}