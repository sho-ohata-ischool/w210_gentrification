library(rstan)
library(RColorBrewer)
library(data.table)

options(mc.cores = parallel::detectCores())

stan_file <- file.path(getwd(),"Models","stan_files","housing_price4.stan") #where the STAN model is saved
df <- data.table(read.csv((file.path(getwd(),"Data","Income_Home_Prices_ZIP_v3.csv"))))
df[,CRIME_COUNTperSqMile:=CRIME_COUNT/LandSqMile]
df[,log_AGI:=log(AGIadj2015)]
df[,log_crime:=log(CRIME_COUNTperSqMile)]
df[is.na(Number.of.Subway.Stations.in.ZIP),Number.of.Subway.Stations.in.ZIP:=0]
df[is.na(Number.of.Subway.Lines.Serving.ZIP), Number.of.Subway.Lines.Serving.ZIP:=0]
df[,Num_stat_cat:=ifelse(Number.of.Subway.Lines.Serving.ZIP==0, 0, ifelse(Number.of.Subway.Lines.Serving.ZIP < 3, "1-2", "3+"))]

##Other data
df_zillow <- fread(file.path(getwd(), "Data", "Zillow2016_2017_Yearly_byZipcode.csv"))
df_zillow <- df_zillow[`ZIP Code` %in% unique(df[,ZIP])]
colnames(df_zillow)[1:2] <- c("Year", "ZIP")
df_zillow <- rbind(df[,.(Year, ZIP, Zillow_value, ZillowAdj, Zillow_value2, ZillowAdj2)], df_zillow)
df_zillow[,ZillowAdj2_lag:=shift(ZillowAdj2,1),by=ZIP]
df_zillow[,Zillow_change:=ZillowAdj2 - ZillowAdj2_lag]

## Crime data for prediction of corresponding year
df_crime <- fread(file.path(getwd(), "Data", "NYC violent crime yearly count.csv"))
df_crime <- merge(df_crime,unique(df[,.(ZIP, LandSqMile)]),by.x="Zip_Code", by.y="ZIP")
df_crime <- df_crime[,log_crime:=log(CRIME_COUNT/LandSqMile)][YEAR==2016 | YEAR==2015] ##for prediction
df_crime[,YEAR:=YEAR+1]
colnames(df_crime)[1:2] <- c("ZIP", "Year")

df_permit <- fread(file.path(getwd(), "Data", "nyc_permit_yearly_count.csv"))
df_permit <- dcast(df_permit, `Zip Code` + `Filing Year` ~ `Job Type`, value.var = c("Count"))
colnames(df_permit)[1:2] <- c("ZIP", "Year")
df_permit <- merge(df_permit, unique(df[,.(ZIP, LandSqMile)]), by=c("ZIP"))
df_permit[,NB_lag1:=log(shift(as.double(NB),1)+1.001),by=ZIP]
df_permit[,A1_lag1:=log(shift(as.double(A1),1)+1.001),by=ZIP]
df_permit[,A2_lag1:=log(shift(as.double(A2),1)+1.001),by=ZIP]
df_permit[,DM_lag2:=log(shift(as.double(DM/LandSqMile),2)+1.001), by=ZIP]
#df_permit[,DM_lag2:=log(shift(as.double(DM),2)+1.001), by=ZIP]
df_permit <- df_permit[Year==2016 | Year==2017]

## Create input data to stan
df_mod <- df[,.(ZIP, Borough, Neighborhood, Year, log_AGI, AdjacentIncome, Bordering.Water, ZillowAdj2, 
                AdjacentZillow2, log_crime, Num_stat_cat, A1, A2, DM, NB, LandSqMile, Proximity)]
df_mod[,log_prev_crime:=shift(log_crime,1),by=ZIP]
df_mod[,log_prev_AGI:=shift(log_AGI,1),by=ZIP]
df_mod[,prev_zillow:=shift(ZillowAdj2,1),by=ZIP]
df_mod[,NB_lag1:=log(shift(as.double(NB),1)+1.001),by=ZIP]
df_mod[,A1_lag1:=log(shift(as.double(A1),1)+1.001),by=ZIP]
df_mod[,A2_lag1:=log(shift(as.double(A2),1)+1.001),by=ZIP]
df_mod[,DM_lag1:=log(shift(as.double(DM/LandSqMile),1)+1.001), by=ZIP]
df_mod[,DM_lag2:=log(shift(as.double(DM/LandSqMile),2)+1.001), by=ZIP]
#df_mod[,DM_lag2:=log(shift(as.double(DM),2)+1.001), by=ZIP]

df_mod <- df_mod[complete.cases(df_mod)] ##for now not back-filling data
df_mod[,intercept:=rep(1,nrow(df_mod))]
#predictors <- as.matrix(df_mod[,.(intercept,prev_zillow, log_prev_crime, NB_lag1,A1_lag1,A2_lag1, DM_lag2, Proximity)])
#predictors <- as.matrix(df_mod[,.(prev_zillow, log_prev_crime, NB_lag1,A1_lag1,A2_lag1, DM_lag2, Proximity)])
predictors <- as.matrix(df_mod[,.(NB_lag1,A1_lag1,A2_lag1, DM_lag2, Proximity)])
predictor_prev_zillow <- df_mod$prev_zillow
predictor_crime <- df_mod$log_prev_crime
response <- df_mod$ZillowAdj2

## Create data for prediction
##df_pred <- df[,.(ZIP, Year, ZillowAdj2, log_crime, A1_lag1=log(as.double(A1)+1.001), NB_lag1=log(as.double(NB)+1.001), DM_lag2=shift(log(as.double(DM+1.001)), 1))]
#df_pred <- df[,.(ZIP, Year, ZillowAdj2, log_crime)]
##df_pred <- df_pred[Year==2015]
##merged <- cbind(merge(df_zillow[,.(ZIP, Year, ZillowAdj2)][Year==2016], df_crime, by.x=c("ZIP", "Year"), by.y=c("ZIP", "Year")), df_permit[Year==2017][,.(A1_lag1, NB_lag1,DM_lag2)])
#merged <- merge(df_zillow[,.(ZIP, Year, ZillowAdj2)][Year==2016], df_crime, by.x=c("ZIP", "Year"), by.y=c("ZIP", "Year"))
##merged <- merged[,.(ZIP, Year, ZillowAdj2, log_crime, A1_lag1, NB_lag1,DM_lag2)]
##df_pred <- merge(rbind(df_pred, merged), unique(df_mod[,.(ZIP, Borough, Num_stat_cat)]), by=c("ZIP"))
##df_pred$A2_log <- df_permit[,(A2_log)]

df_pred <- merge(merge(df_zillow[,.(ZIP, Year, ZillowAdj2_lag)][Year==2016 | Year==2017], df_crime, by=c("ZIP", "Year")), 
                 df_permit[,.(ZIP, Year, NB_lag1, A1_lag1, A2_lag1, DM_lag2)],by=c("ZIP", "Year"))
df_pred[,intercept:=rep(1,nrow(df_pred))]
df_pred <- merge(df_pred, unique(df[,.(ZIP, Neighborhood, Borough, Num_stat_cat)]), by=c("ZIP"))
prox <- df[df$Year==2015][,.(ZIP, Proximity)]
df_pred <- merge(df_pred, prox, by=c("ZIP"))
#X_pred <- as.matrix(df_pred[,.(ZillowAdj2_lag, log_crime, NB_lag1, A1_lag1, A2_lag1, DM_lag2, Proximity)])
#X_pred <- as.matrix(df_pred[,.(ZillowAdj2_lag, NB_lag1, A1_lag1, A2_lag1, DM_lag2, Proximity)])
X_pred <- as.matrix(df_pred[,.(NB_lag1, A1_lag1, A2_lag1, DM_lag2, Proximity)])
X_pred_crime <- df_pred$log_crime
X_pred_prev_zillow <- df_pred$ZillowAdj2_lag
#X_pred <- as.matrix(df_pred[,.(intercept, ZillowAdj2_lag, log_crime, NB_lag1, A1_lag1, A2_lag1, DM_lag2, Proximity)])

##Parameters to be passed to stan
N <- nrow(df_mod) #number of observations
N_pred <- nrow(df_pred)
J <- length(unique(df_mod$Neighborhood)) #number of neighborhoods
K <- ncol(predictors) #number of regression coefficients
B <- length(unique(df_mod$Borough))
S <- length(unique(df_mod$Num_stat_cat))
boro <- as.numeric(as.factor(df_mod$Borough))
boro_pred <- as.numeric(as.factor(df_pred$Borough))
water <- as.numeric(as.factor(df_mod$Bordering.Water))
station <- as.numeric(as.factor(df_mod$Num_stat_cat))
station_pred <- as.numeric(as.factor(df_pred$Num_stat_cat))
id <- as.numeric(as.factor(df_mod$Neighborhood)) ## each group, i.e.  neighborhoods
id_pred <- as.numeric(as.factor(df_pred$Neighborhood)) ## each group, i.e. neighborhoods
zip_levels <- levels(as.factor(df_mod$ZIP)) ##to map back id to zip

#run the model
stan_data <- list(N=N,N_pred=N_pred,J=J,K=K,id=id,id_pred=id_pred,
                  boro=boro,boro_pred=boro_pred,B=B,
                  ##water=water,
                  station=station, S=S, station_pred=station_pred,
                  X_pred_crime=X_pred_crime, predictor_crime=predictor_crime,
                  prev_zillow=predictor_prev_zillow, X_prev_zillow=X_pred_prev_zillow,
                  X=predictors,X_pred=X_pred,y=response)
if (exists("m_hier")){ rm(m_hier) }
m_hier<-stan(file=stan_file, data = stan_data, chains=4)

fit_summary <- summary(m_hier)$summary
pred_out <- data.frame(fit_summary[grep("y_sim", rownames(fit_summary)),]) ##stan model prediction for 2016
pred_out$ZIP <- rep(zip_levels,each=2)
pred_out$Year <- rep(2016:2017,175)

##shinystan::launch_shinystan(m_hier) ##For parameter diagnostics

## Base line model to predict 2016
ols <- lm(ZillowAdj2 ~ prev_zillow + log_prev_crime, data=df_mod)
ols_coef <- ols$coefficients
pred_out$base_pred <- ols_coef[1] +  df_pred$ZillowAdj2 * ols_coef[2] + df_pred$log_crime * ols_coef[3]
pred_out <- merge(pred_out, df_zillow[Year==2016 |Year ==2017][,.(ZIP, Year, ZillowAdj2)], by.x=c("ZIP", "Year"), by.y=c("ZIP", "Year"))
MAPE_stan <- sum(abs(pred_out$mean - pred_out$ZillowAdj2)/pred_out$ZillowAdj2)/dim(pred_out)[1] ##11.6%
MAPE_base <- sum(abs(pred_out$base_pred - pred_out$ZillowAdj2)/pred_out$ZillowAdj2)/dim(pred_out)[1]
