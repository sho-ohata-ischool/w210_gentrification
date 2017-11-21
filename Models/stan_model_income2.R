library(rstan)
library(RColorBrewer)
library(data.table)

options(mc.cores = parallel::detectCores())

stan_file <- file.path(getwd(),"Models","stan_files","housing_price2.stan") #where the STAN model is saved
df <- data.table(read.csv((file.path(getwd(),"Data","Income_Home_Prices_ZIP_v2.csv"))))
df[,CRIME_COUNTperSqMile:=CRIME_COUNT/LandSqMile]
df[,log_AGI:=log(AGIadj2015)]
df[,log_crime:=log(CRIME_COUNTperSqMile)]
df[is.na(Number.of.Subway.Stations.in.ZIP),Number.of.Subway.Stations.in.ZIP:=0]
df[is.na(Number.of.Subway.Lines.Serving.ZIP), Number.of.Subway.Lines.Serving.ZIP:=0]
df[,Num_stat_cat:=ifelse(Number.of.Subway.Lines.Serving.ZIP==0, 0, ifelse(Number.of.Subway.Lines.Serving.ZIP < 3, "1-2", "3+"))]

##Other data
#df_zillow <- fread(file.path(getwd(), "Data", "Zillow_by_Zip_year.csv"))
df_crime <- fread(file.path(getwd(), "Data", "NYC violent crime yearly count.csv"))
df_crime <- merge(df_crime,unique(df[,.(ZIP, LandSqMile)]),by.x="Zip_Code", by.y="ZIP")
df_crime <- df_crime[,CRIME_COUNTperSqMile:=CRIME_COUNT/LandSqMile][YEAR==2016] ##for prediction
#df_permit <- fread(file.path(getwd(), "Data", "NYC permit yearly count.csv"))

## Create input data to stan
df_mod <- df[,.(ZIP, Borough, Year, log_AGI, AdjacentIncome, Bordering.Water, ZillowAdj2, AdjacentZillow2, log_crime, Num_stat_cat)]
df_mod[,log_prev_crime:=c(NA,log_crime),by=ZIP]
df_mod[,log_prev_AGI:=c(NA,log_AGI),by=ZIP]
df_mod[,prev_zillow:=c(NA,ZillowAdj2),by=ZIP]
df_mod <- df_mod[complete.cases(df_mod)] ##for now not back-filling data
#predictors <- as.matrix(df_mod[,.(prev_AGI, AdjacentIncome, prev_zillow, AdjacentZillow2, prev_crime, Number.of.Subway.Stations.in.ZIP)])
#predictors <- as.matrix(df_mod[,.(prev_AGI, AdjacentIncome, prev_zillow, AdjacentZillow2, prev_crime)])
predictors <- as.matrix(df_mod[,.(log_prev_AGI, prev_zillow, log_prev_crime)])
response <- df_mod$ZillowAdj2

## Create data for prediction
df_pred <- df[,.(ZIP, Year, log_AGI, ZillowAdj2, log_crime)]
df_pred <- df_pred[Year==2015]
X_pred <- as.matrix(df_pred[,.(log_AGI, ZillowAdj2, log_crime)])

##Parameters to be passed to stan
N <- nrow(df_mod) #number of observations
N_pred <- nrow(df_pred)
J <- length(unique(df_mod$ZIP)) #number of zip-codes
K <- ncol(predictors) #number of regression coefficients
B <- length(unique(df_mod$Borough))
S <- length(unique(df_mod$Num_stat_cat))
boro <- as.numeric(as.factor(df_mod$Borough))
water <- as.numeric(as.factor(df_mod$Bordering.Water))
station <- as.numeric(as.factor(df_mod$Num_stat_cat))
id <- as.numeric(as.factor(df_mod$ZIP)) ## each group, i.e. zip code
id_pred <- as.numeric(as.factor(df_pred$ZIP)) ## each group, i.e. zip code
zip_levels <- levels(as.factor(df_mod$ZIP)) ##to map back id to zip

#run the model
stan_data <- list(N=N,N_pred=N_pred,J=J,K=K,zip=id,zip_pred=id_pred,boro=boro,B=B,water=water,station=station, S=S,X=predictors,X_pred=X_pred,y=response)
m_hier<-stan(file=stan_file, data = stan_data, chains=4)

fit_summary <- summary(m_hier)$summary
pred_out <- data.frame(fit_summary[grep("y_sim", rownames(fit_summary)),])
pred_out$Zip <- zip_levels
