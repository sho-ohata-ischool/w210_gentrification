data {
  int N; //the number of observations
  int N_pred; //number of observations for new prediction
  int J; //the number of neighborhoods
  int K; //number of columns in the model matrix
  int id[N]; //vector of neighborhoods indices
  int id_pred[N_pred]; //vector of neighborhoods prediction indices
  int boro[N]; //vector of borough
  int boro_pred[N_pred];
  int B; //number of boroughs
  //int S; //number of categories for amount of subway stations
  //int station[N]; //vector of station categorical variable
  //int station_pred[N_pred]; 
  //int water[N]; //vector of waterfront binary variable
  matrix[N,K] X; //the model matrix
  vector[N] y; //the response variable e.g. zillow price
  vector[N] predictor_crime; 
  vector[N] prev_zillow;
  matrix[N_pred,K] X_pred; //data being fed in for prediction
  vector[N_pred] X_pred_crime;
  vector[N_pred] X_prev_zillow;
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
    //mu[n] = exp(X[n] * betas[boro[n]] + gamma[boro[n]] + predictor_crime[n] * tau[id[n]]); //using the log link
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
  
  //for (b in 1:B){
    //gamma[b] ~ cauchy(1,1); //prior for the intercept following Gelman 2008
    //for (s in 1:S){
    //  gamma[b,s] ~ cauchy(0,2.5); //prior for the intercept following Gelman 2008
    //}
  //}
  
  for (b in 1:B) {
    gamma[b] ~ cauchy(0,1);
    for(i in 1:K) {
      betas[b, i] ~ cauchy(0,2.5);//prior for the slopes following Gelman 2008
    }
  }
  y ~ gamma(alpha,beta);
}
generated quantities {
 vector[N_pred] y_sim;
 vector[N_pred] mu_pred;
 vector[N_pred] alpha_pred;
 vector[N_pred] beta_pred;
 
 for(n in 1:N_pred){
  //mu_pred[n] = exp(X_pred[n] * betas[boro_pred[n]] + gamma[boro_pred[n], station_pred[n]]);
  //#**mu_pred[n] = exp(X_pred[n] * betas[boro_pred[n]] + gamma[boro_pred[n]]);
  mu_pred[n] = X_prev_zillow[n] + exp(X_pred[n] * betas[boro_pred[n]] + gamma[boro_pred[n]] + X_pred_crime[n] * tau[id_pred[n]]);
  alpha_pred[n] = mu_pred[n] * mu_pred[n] / phi;
  beta_pred[n] = mu_pred[n] / phi;
  y_sim[n] = gamma_rng(alpha_pred[n],beta_pred[n]); //posterior draws to get posterior predictive checks
 }
}