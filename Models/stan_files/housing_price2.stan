data {
  int N; //the number of observations
  int N_pred; //number of observations for new prediction
  int J; //the number of zip codes
  int K; //number of columns in the model matrix
  int zip[N]; //vector of zip indices
  int zip_pred[N_pred]; //vector of zip prediction indices
  int boro[N]; //vector of borough
  int boro_pred[N_pred];
  int B; //number of boroughs
  int S; //number of categories for amount of subway stations
  int station[N]; //vector of station categorical variable
  int water[N]; //vector of waterfront binary variable
  matrix[N,K] X; //the model matrix
  vector[N] y; //the response variable e.g. zillow price
  matrix[N_pred,K] X_pred; //data being fed in for prediction
}

parameters {
  vector[K] betas; //the regression parameters
  real phi; //the variance parameter
}

transformed parameters {
  vector[N] mu; //the expected values (linear predictor)
  vector[N] alpha; //shape parameter for the gamma distribution
  vector[N] beta; //rate parameter for the gamma distribution
  
  mu = exp(X * betas); //using the log link 
  alpha = mu .* mu / phi; 
  beta = mu / phi;
}

model {  
  betas[1] ~ cauchy(0,10); //prior for the intercept following Gelman 2008

  for(i in 2:K) {
   betas[i] ~ cauchy(0,2.5);//prior for the slopes following Gelman 2008
  }
  y ~ gamma(alpha,beta);
}
generated quantities {
 vector[N_pred] y_sim;
 vector[N_pred] mu_pred;
 vector[N_pred] alpha_pred;
 vector[N_pred] beta_pred;
 
 for(n in 1:N_pred){
  mu_pred[n] = exp(X_pred[n] * betas);
  alpha_pred[n] = mu_pred[n] * mu_pred[n] / phi;
  beta_pred[n] = mu_pred[n] / phi;
  y_sim[n] = gamma_rng(alpha_pred[n],beta_pred[n]); //posterior draws to get posterior predictive checks
 }
}