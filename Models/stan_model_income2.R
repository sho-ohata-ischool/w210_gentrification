library(rstan)
library(RColorBrewer)
library(data.table)

options(mc.cores = parallel::detectCores())

stan_file <- file.path(getwd(),"Models","stan_files","housing_price2.stan") #where the STAN model is saved
df <- data.table(read.csv((file.path(getwd(),"Data","Income_Home_Prices_ZIP_v2.csv"))))
df[,CRIME_COUNTperSqMile:=CRIME_COUNT/LandSqMile]
##df$ZIP <- as.factor(df$ZIP)

## Create input data to stan
#df_mod <- df[,.(ZIP, Year, AGIadj2015, AdjacentIncome, ZillowAdj2, AdjacentZillow2, CRIME_COUNT, Number.of.Subway.Stations.in.ZIP)]
df_mod <- df[,.(ZIP, Borough, Year, AGIadj2015, AdjacentIncome, Bordering.Water, ZillowAdj2, AdjacentZillow2, CRIME_COUNTperSqMile)]
df_mod[,prev_crime:=c(NA,CRIME_COUNTperSqMile),by=ZIP]
df_mod[,prev_AGI:=c(NA,AGIadj2015),by=ZIP]
df_mod[,prev_zillow:=c(NA,ZillowAdj2),by=ZIP]
df_mod <- df_mod[complete.cases(df_mod)] ##for now not back-filling data
#predictors <- as.matrix(df_mod[,.(prev_AGI, AdjacentIncome, prev_zillow, AdjacentZillow2, prev_crime, Number.of.Subway.Stations.in.ZIP)])
#predictors <- as.matrix(df_mod[,.(prev_AGI, AdjacentIncome, prev_zillow, AdjacentZillow2, prev_crime)])
predictors <- as.matrix(df_mod[,.(prev_AGI, prev_zillow, prev_crime)])
response <- df_mod$ZillowAdj2

## Create data for prediction
df_pred <- df[,.(ZIP, Year, AGIadj2015, ZillowAdj2, CRIME_COUNTperSqMile)]
df_pred <- df_pred[Year==2015]
X_pred <- as.matrix(df_pred[,.(AGIadj2015, ZillowAdj2, CRIME_COUNTperSqMile)])

##Parameters to be passed to stan
N <- nrow(df_mod) #number of observations
N_pred <- nrow(df_pred)
J <- length(unique(df_mod$ZIP)) #number of zip-codes
K <- ncol(predictors) #number of regression coefficients
B <- length(unique(df_mod$Borough))
boro <- as.numeric(as.factor(df_mod$Borough))
water <- as.numeric(as.factor(df_mod$Bordering.Water))
id <- as.numeric(as.factor(df_mod$ZIP)) ## each group, i.e. zip code
id_pred <- as.numeric(as.factor(df_pred$ZIP)) ## each group, i.e. zip code
zip_levels <- levels(as.factor(df_mod$ZIP)) ##to map back id to zip

#run the model
stan_data <- list(N=N,N_pred=N_pred,J=J,K=K,zip=id,zip_pred=id_pred,boro=boro,B=B,water=water,X=predictors,X_pred=X_pred,y=response)
m_hier<-stan(file=stan_file, data = stan_data, chains=4)

fit_summary <- summary(m_hier)$summary
pred_out <- data.frame(fit_summary[grep("y_sim", rownames(fit_summary)),])
pred_out$Zip <- zip_levels
