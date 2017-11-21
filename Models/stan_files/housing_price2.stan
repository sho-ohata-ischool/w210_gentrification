data {
  int N; //the number of observations
  int N_pred; //number of observations for new prediction
  int J; //the number of zip codes
  int K; //number of columns in the model matrix
  int zip[N]; //vector of zip indices
  int zip_pred[N_pred]; //vector of zip prediction indices
  int boro[N]; //vector of borough
  int B; //number of boroughs
  int S; //number of categories for amount of subway stations
  int station[N]; //vector of station categorical variable
  int water[N]; //vector of waterfront binary variable
  matrix[N,K] X; //the model matrix
  vector[N] y; //the response variable e.g. zillow price
  matrix[N_pred,K] X_pred; //data being fed in for prediction
}
parameters {
  vector[K] tau; 
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
    gamma[n] = alpha1[boro[n]] + alpha2[water[n]]; // intercept of boroughs
  }
  
  for(j in 1:J){
    beta[j] = alpha + tau .* beta_raw[j];
  }
}

model {
  vector[N] mu; //linear predictor
  //priors
  alpha ~ normal(0,1); 
  tau ~ normal(0,1);
  gamma ~ gamma(1.5,0.25); // intercept > 0
  alpha1 ~ gamma(0.5,1); //weakly informative priors, see section 6.9 in STAN user guide
  alpha2 ~ gamma(0.5,1); //weakly informative priors, see section 6.9 in STAN user guide
  sigma ~ gamma(2,0.1); //weakly informative priors, see section 6.9 in STAN user guide
  
  for(j in 1:J){
    beta_raw[j] ~ normal(0,1); //fill the matrix of group-level regression coefficients 
  }

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