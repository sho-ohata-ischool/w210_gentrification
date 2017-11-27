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

##Other data for prediction
df_zillow <- fread(file.path(getwd(), "Data", "Zillow2016_2017_Yearly_byZipcode.csv"))
df_zillow <- df_zillow[ZIP_Code %in% unique(df[,ZIP])]
colnames(df_zillow)[2] <- "ZIP"
## Crime data for prediction of corresponding year
df_crime <- fread(file.path(getwd(), "Data", "NYC violent crime yearly count.csv"))
df_crime <- merge(df_crime,unique(df[,.(ZIP, LandSqMile)]),by.x="Zip_Code", by.y="ZIP")
df_crime <- df_crime[,log_crime:=log(CRIME_COUNT/LandSqMile)][YEAR==2016 | YEAR==2015] ##for prediction
df_crime[,YEAR:=YEAR+1]
colnames(df_crime)[1:2] <- c("ZIP", "Year")
df_permit <- fread(file.path(getwd(), "Data", "nyc_permit_yearly_count.csv"))
df_permit <- dcast(df_permit, `Zip Code` + `Filing Year` ~ `Job Type`, value.var = c("Count"))
colnames(df_permit)[1:2] <- c("ZIP", "Year")
df_permit[,NB_lag1:=shift(NB,1),by=ZIP]
df_permit[,A1_lag1:=shift(A1,1),by=ZIP]
df_permit[,A2_lag1:=shift(A2,1),by=ZIP]
df_permit[,DM_lag2:=shift(DM,2), by=ZIP]
df_permit <- df_permit[Year==2016 | Year==2017]

## Create input data to stan
df_mod <- df[,.(ZIP, Borough, Year, log_AGI, AdjacentIncome, Bordering.Water, ZillowAdj2, AdjacentZillow2, log_crime, Num_stat_cat, A1, A2, DM, NB)]
df_mod[,log_prev_crime:=shift(log_crime,1),by=ZIP]
df_mod[,log_prev_AGI:=shift(log_AGI,1),by=ZIP]
df_mod[,prev_zillow:=shift(ZillowAdj2,1),by=ZIP]
df_mod[,NB_lag1:=shift(NB,1),by=ZIP]
df_mod[,A1_lag1:=shift(A1,1),by=ZIP]
df_mod[,A2_lag1:=shift(A2,1),by=ZIP]
df_mod[,DM_lag2:=shift(DM,2), by=ZIP]
df_mod <- df_mod[complete.cases(df_mod)] ##for now not back-filling data
predictors <- as.matrix(df_mod[,.(prev_zillow, log_prev_crime, NB_lag1, A1_lag1, A2_lag1, DM_lag2)])
response <- df_mod$ZillowAdj2

## Create data for prediction
df_pred <- df[,.(ZIP, Year, ZillowAdj2, log_crime, A1_lag1=A1, A2_lag1=A2, NB_lag1=NB, DM_lag2=shift(DM, 1))]
df_pred <- df_pred[Year==2015]
merged <- cbind(merge(df_zillow[,.(ZIP, Year, ZillowAdj2)][Year==2016], df_crime, by.x=c("ZIP", "Year"), by.y=c("ZIP", "Year")), df_permit[Year==2017][,.(A1_lag1, A2_lag1, NB_lag1,DM_lag2)])
merged <- merged[,.(ZIP, Year, ZillowAdj2, log_crime, A1_lag1, A2_lag1, DM_lag2, NB_lag1)]
df_pred <- rbind(df_pred, merged)
X_pred <- as.matrix(df_pred[,.(ZillowAdj2, log_crime, A1_lag1, A2_lag1, DM_lag2, NB_lag1)])

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
pred_out <- data.frame(fit_summary[grep("y_sim", rownames(fit_summary)),]) ##stan model prediction for 2016
pred_out$ZIP <- zip_levels
pred_out$Year <- rep(2016:2017,each=175)

shinystan::launch_shinystan(m_hier) ##For parameter diagnostics

## Base line model to predict 2016
ols <- lm(ZillowAdj2 ~ log_prev_AGI + prev_zillow + log_prev_crime, data=df_mod)
ols_coef <- ols$coefficients
pred_out$base_pred <- ols_coef[1] + df_pred$log_AGI * ols_coef[2] + df_pred$ZillowAdj2 * ols_coef[3] + df_pred$log_crime * ols_coef[4]
pred_out <- merge(pred_out, df_zillow[Year==2016 |Year ==2017][,.(ZIP, Year, ZillowAdj2)], by.x=c("ZIP", "Year"), by.y=c("ZIP", "Year"))
MAPE_stan <- sum(abs(pred_out$mean - pred_out$ZillowAdj2)/pred_out$ZillowAdj2)/dim(pred_out)[1]
MAPE_base <- sum(abs(pred_out$base_pred - pred_out$ZillowAdj2)/pred_out$ZillowAdj2)/dim(pred_out)[1]
