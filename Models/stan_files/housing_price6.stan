data {
  int N; //the number of observations
  int N_forecast; 
  int J; //the number of neighborhoods
  int K; //number of columns in the model matrix
  int id[N]; //vector of neighborhoods indices
  int id_forecast[N_forecast];
  int boro[N]; //vector of borough
  int boro_forecast[N_forecast];
  int B; //number of boroughs
  //int S; //number of categories for amount of subway stations
  //int station[N]; //vector of station categorical variable
  //int station_pred[N_pred]; 
  //int water[N]; //vector of waterfront binary variable
  matrix[N,K] X; //the model matrix
  vector[N] y; //the response variable e.g. zillow price
  vector[N] predictor_crime; 
  vector[N] prev_zillow;
  vector[175] zillow_2017;//2017 zillow actual for 2018
  matrix[N_forecast,K] X_forecast;
  vector[N_forecast] X_forecast_crime;
  //vector[N_forecast] X_forecast_zillow;
}
parameters {
  vector[K] betas[B]; //the regression parameters
  real gamma[B];
  real tau[J]; 
  real <lower=0, upper=2> phi; //the variance parameter
}
transformed parameters {
  vector[N] mu; //the expected values (linear predictor)
  vector[N] alpha; //shape parameter for the gamma distribution
  vector[N] beta; //rate parameter for the gamma distribution

  for (n in 1:N){
    //mu[n] = exp(X[n] * betas[boro[n]] + gamma[boro[n], station[n]]); //using the log link 
    //#**mu[n] = exp(X[n] * betas[boro[n]] + gamma[boro[n]]); //using the log link 
      //mu[n] = exp(prev_zillow[n] + X[n] * betas[boro[n]] + gamma[boro[n]] + predictor_crime[n] * tau[id[n]]); //using the log link
      mu[n] = prev_zillow[n] + exp(X[n] * betas[boro[n]] + gamma[boro[n]] + predictor_crime[n] * tau[id[n]]); //using the log link
  }
  alpha = mu .* mu / phi; 
  beta = mu / phi;
}
model {
  
  for (j in 1:J){
    //gamma[j] ~ cauchy(0,1); //prior for the intercept following Gelman 2008
    tau[j] ~ cauchy(0,2.5);
  }
  
  //tau ~ cauchy(0,2.5);
  

  for (b in 1:B) {
    gamma[b] ~ cauchy(0,1);
    for(i in 1:K) {
        betas[b, i] ~ cauchy(0,2.5);//prior for the slopes following Gelman 2008
      }
  }
  y ~ gamma(alpha,beta);
}
generated quantities {
  vector[N_forecast] y_sim;
  vector[N_forecast] mu_pred;
  vector[N_forecast] alpha_pred;
  vector[N_forecast] beta_pred;
  
  for(n in 1:175){//2018 data
    mu_pred[n] = zillow_2017[n] + exp(X_forecast[n] * betas[boro_forecast[n]] + gamma[boro_forecast[n]] + X_forecast_crime[n] * tau[id_forecast[n]]);
    //mu_pred[n] = exp(mu_pred[n-175] + X_forecast[n-N_pred] * betas[boro_forecast[n-N_pred]] + gamma[boro_forecast[n-N_pred]] + X_forecast_crime[n-N_pred] * tau[id_forecast[n-N_pred]]);
    alpha_pred[n] = mu_pred[n] * mu_pred[n] / phi;
    beta_pred[n] = mu_pred[n] / phi;
    y_sim[n] = gamma_rng(alpha_pred[n],beta_pred[n]); //posterior draws to get posterior predictive checks
  }
  
  for(n in 176:N_forecast){
    mu_pred[n] = mu_pred[n-175] + exp(X_forecast[n] * betas[boro_forecast[n]] + gamma[boro_forecast[n]] + X_forecast_crime[n] * tau[id_forecast[n]]);
    //mu_pred[n] = exp(mu_pred[n-175] + X_forecast[n-N_pred] * betas[boro_forecast[n-N_pred]] + gamma[boro_forecast[n-N_pred]] + X_forecast_crime[n-N_pred] * tau[id_forecast[n-N_pred]]);
    alpha_pred[n] = mu_pred[n] * mu_pred[n] / phi;
    beta_pred[n] = mu_pred[n] / phi;
    y_sim[n] = gamma_rng(alpha_pred[n],beta_pred[n]); //posterior draws to get posterior predictive checks
  }
}