library(rstan)
library(RColorBrewer)
library(data.table)

stan_file <- file.path(getwd(),"Models","stan_files","housing_price.stan") #where the STAN model is saved
df <- data.table(read.csv((file.path(getwd(),"Data","Income_Home_Prices_ZIP_v2.csv"))))
df$ZIP <- as.factor(df$ZIP)

## Create input data to stan
#df_mod <- df[,.(ZIP, Year, AGIadj2015, AdjacentIncome, ZillowAdj2, AdjacentZillow2, CRIME_COUNT, Number.of.Subway.Stations.in.ZIP)]
df_mod <- df[,.(ZIP, Year, AGIadj2015, AdjacentIncome, ZillowAdj2, AdjacentZillow2, CRIME_COUNT)]
df_mod$CRIME_COUNTperSqMile <- df_mod$CRIME_COUNT/df$LandSqMile
df_mod[,prev_crime:=c(NA,CRIME_COUNTperSqMile),by=ZIP]
df_mod[,prev_AGI:=c(NA,AGIadj2015),by=ZIP]
df_mod[,prev_zillow:=c(NA,ZillowAdj2),by=ZIP]
df_mod <- df_mod[complete.cases(df_mod)] ##for now not back-filling data
#predictors <- as.matrix(df_mod[,.(prev_AGI, AdjacentIncome, prev_zillow, AdjacentZillow2, prev_crime, Number.of.Subway.Stations.in.ZIP)])
predictors <- as.matrix(df_mod[,.(prev_AGI, AdjacentIncome, prev_zillow, AdjacentZillow2, prev_crime)])
response <- df_mod$ZillowAdj2

##Parameters to be passed to stan
N <- nrow(df_mod) #number of observations
J <- length(unique(df_mod$ZIP)) #number of zip-codes
K <- ncol(predictors) #number of regression coefficients
id <- as.numeric(as.factor(df_mod$ZIP)) ## each group, i.e. zip code

#run the model
stan_data <- list(N=N,J=J,K=K,id=id,X=predictors,y=response)
model = stan(stan_file, data = stan_data, chains = 0)
m_hier<-stan(fit=model,file=stan_file, data = stan_data)
