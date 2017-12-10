df_viz <- fread(file.path(getwd(), "Visualization", "Income_Home_Prices_ZIP_viz.csv"))

zillow_growth <- df_zillow[,.(ZIP, Year, ZillowAdj2)][Year==2016 | Year==2017][,"Growth_16_17":=1-shift(ZillowAdj2,1)/ZillowAdj2,by=ZIP]
zillow_growth <- zillow_growth[complete.cases(zillow_growth),][,.(ZIP, Growth_16_17)]

zillow_overall <- df_zillow[complete.cases(df_zillow),c("Year", "ZIP", "ZillowAdj2")][,.SD[unique(c(1,.N))],by=ZIP][,"overall_growth":=(1-shift(ZillowAdj2,1)/ZillowAdj2)/(Year-shift(Year,1)),by=ZIP]
zillow_overall <- zillow_overall[complete.cases(zillow_overall),][,.(ZIP, overall_growth)]

##R code to pipe into stan
library(rstan)
library(RColorBrewer)
library(data.table)

options(mc.cores = parallel::detectCores())

stan_file <- file.path(getwd(),"Models","stan_files","housing_price5.stan") #where the STAN model is saved
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
df_permit[,DM_lag2:=log(shift(as.double(DM/LandSqMile),2,fill = NA)+1.001), by=ZIP]
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

##Forecast data
df_crime <- fread(file.path(getwd(), "Data", "NYC violent crime yearly count.csv"))
colnames(df_crime)[1:2] <- c("ZIP", "Year")
df_crime <- df_crime[ZIP %in% unique(df[,ZIP])]
crime_growth <- df_crime[,.(ZIP, Year, CRIME_COUNT)][Year==2015 | Year==2016][,"Growth_15_16":=1-shift(CRIME_COUNT,1)/CRIME_COUNT,by=ZIP]
crime_growth <- crime_growth[complete.cases(crime_growth),][,.(ZIP, Growth_15_16)]
crime_growth <- merge(crime_growth, df_crime[Year==2016], by=c("ZIP"))
crime_overall <- df_crime[complete.cases(df_crime),c("Year", "ZIP", "CRIME_COUNT")][,.SD[unique(c(1,.N))],by=ZIP][,"overall_growth":=(1-shift(CRIME_COUNT,1)/CRIME_COUNT)/(Year-shift(Year,1)),by=ZIP]
crime_overall <- crime_overall[complete.cases(crime_overall),][,.(ZIP, overall_growth)]
crime_growth <- merge(crime_growth, crime_overall, by=c("ZIP"))[,.(ZIP, Year, CRIME_COUNT, Growth_15_16, overall_growth)]
rm(crime_overall)

crime_forecast <- data.table(ZIP=integer(), Year=integer(),CRIME_COUNT_15_16=integer(),CRIME_COUNT_o=integer(),
                             Growth_15_16=double(),overall_growth=double())
crime_growth_hold <- crime_growth[Year==2016]
crime_forecast <- crime_growth_hold[,list(ZIP,Year,CRIME_COUNT_15_16=round(Growth_15_16*CRIME_COUNT + CRIME_COUNT,0), 
                                          CRIME_COUNT_o=round(CRIME_COUNT*overall_growth+CRIME_COUNT,0),
                                          Growth_15_16,overall_growth)]
for (n in 2017:2024){
  crime_growth_hold <- crime_forecast[Year==n-1]
  crime_growth_hold <- crime_growth_hold[,list(ZIP,Year=n,CRIME_COUNT_15_16=round(Growth_15_16*CRIME_COUNT_15_16 + CRIME_COUNT_15_16,0), 
                                               CRIME_COUNT_o=round(CRIME_COUNT_o*overall_growth+CRIME_COUNT_o,0),
                                               Growth_15_16,overall_growth)]
  crime_forecast <- rbind(crime_forecast,crime_growth_hold)
}
crime_forecast <- crime_forecast[,Year:=Year+1][Year >= 2018]
crime_forecast <- merge(crime_forecast, unique(df[,.(ZIP, LandSqMile)], by=c("ZIP")))
crime_forecast[,`:=`(CRIME_COUNT_15_16=log(as.double(CRIME_COUNT_15_16)/LandSqMile),CRIME_COUNT_o=log(as.double(CRIME_COUNT_o)/LandSqMile))]

##Permits forecast data
df_permit <- fread(file.path(getwd(), "Data", "nyc_permit_yearly_count.csv"))
df_permit <- dcast(df_permit, `Zip Code` + `Filing Year` ~ `Job Type`, value.var = c("Count"))
colnames(df_permit)[1:2] <- c("ZIP", "Year")
df_permit[Year==2017,`:=`(NB=round(as.double(NB)*(1+1/12),0),A1=round(as.double(A1)*(1+1/12),0),A2=round(as.double(A2)*(1+1/12),0),DM=round(as.double(DM)*(1+1/12),0))]##December data missing

df_permit_agg <- df_permit[Year>=2010][,`:=`(mean_A1=mean(A1), sd_A1=sd(A1), mean_A2=mean(A2), sd_A2=sd(A2), mean_NB=mean(NB), sd_NB=sd(NB), mean_DM=mean(DM), sd_DM=sd(DM)),by=c("ZIP")]
df_permit_agg <- df_permit_agg[,`:=`(lambd_A1=sum(A1)/8,lambd_A2=sum(A2)/8,lambd_NB=sum(NB)/8,lambd_DM=sum(DM)/8),by=c("ZIP")][Year==2017]

for (n in 2018:2025) {
  df_permit <- rbind(df_permit,df_permit[,list(ZIP=unique(ZIP), Year=rep(n,175), A1=rpois(175,df_permit_agg$lambd_A1), 
                                               A2=rpois(175,df_permit_agg$lambd_A2), DM=rpois(175,df_permit_agg$lambd_DM), NB=rpois(175,df_permit_agg$lambd_NB))])
}
df_permit <- merge(df_permit, unique(df[,.(ZIP, LandSqMile)]), by=c("ZIP"))

df_permit[,NB_lag1:=log(shift(as.double(NB),1)+1.001),by=ZIP]
df_permit[,A1_lag1:=log(shift(as.double(A1),1)+1.001),by=ZIP]
df_permit[,A2_lag1:=log(shift(as.double(A2),1)+1.001),by=ZIP]
df_permit[,DM_lag2:=log(shift(as.double(DM/LandSqMile),2)+1.001), by=ZIP]
df_permit <- df_permit[Year>2017][,.(ZIP,Year, NB_lag1,A1_lag1,A2_lag1,DM_lag2)]

df_forecast <- df_mod[Year==2015][,.(ZIP, Borough,Neighborhood, Bordering.Water,Proximity,Num_stat_cat)]
df_forecast <- df_forecast[rep(seq_len(nrow(df_forecast)), 8),]
df_forecast[,Year:=rep(2018:2025,each=175)]
df_forecast <- merge(merge(df_forecast, df_permit, by=c("ZIP", "Year")), crime_forecast[,.(ZIP, Year,CRIME_COUNT_o)], by=c("ZIP", "Year"))

N_forecast <- nrow(df_forecast)
X_forecast <- as.matrix(df_forecast[,.(NB_lag1, A1_lag1, A2_lag1, DM_lag2, Proximity)])
X_forecast_crime <- df_forecast$CRIME_COUNT_o
boro_forecast <- as.numeric(as.factor(df_forecast$Borough))
id_forecast <- as.numeric(as.factor(df_forecast$Neighborhood))

#run the model
stan_data <- list(N=N,N_pred=N_pred,J=J,K=K,id=id,id_pred=id_pred,
                  boro=boro,boro_pred=boro_pred,B=B,
                  ##water=water,
                  station=station, S=S, station_pred=station_pred,
                  X_pred_crime=X_pred_crime, predictor_crime=predictor_crime,
                  prev_zillow=predictor_prev_zillow, X_prev_zillow=X_pred_prev_zillow,
                  N_forecast=N_forecast,X_forecast=X_forecast,boro_forecast=boro_forecast,
                  id_forecast=id_forecast,X_forecast_crime=X_forecast_crime,
                  X=predictors,X_pred=X_pred,y=response)
if (exists("m_hier")){ rm(m_hier) }
m_hier<-stan(file=stan_file, data = stan_data, chains=4)

fit_summary <- summary(m_hier)$summary
pred_out <- data.table(fit_summary[grep("y_sim", rownames(fit_summary)),], keep.rownames = TRUE) ##stan model prediction
pred_out[,`:=`(ZIP=rep(zip_levels,each=5), Year=rep(2016:2025,175))]
colnames(pred_out)[2] <- "y_sim"
alpha_out <- data.table(fit_summary[grep("alpha_pred", rownames(fit_summary)),],  keep.rownames = TRUE)
alpha_out[,`:=`(ZIP=rep(zip_levels,each=5), Year=rep(2016:2025,175))]
colnames(alpha_out)[2] <- "alpha_pred"
beta_out <- data.table(fit_summary[grep("beta_pred", rownames(fit_summary)),], keep.rownames = TRUE)
beta_out[,`:=`(ZIP=rep(zip_levels,each=5), Year=rep(2016:2025,175))]
colnames(beta_out)[2] <- "beta_pred"
pred_out <- merge(pred_out[,.(ZIP, Year, y_sim)], alpha_out[,.(ZIP, Year, alpha_pred)], by=c("ZIP", "Year"))
pred_out <- merge(pred_out[,.(ZIP, Year, y_sim, alpha_pred)], beta_out[,.(ZIP, Year, beta_pred)], by=c("ZIP", "Year"))
pred_forecast <- pred_out[Year > 2017]
write.csv(pred_forecast, 'zillow_forecast.csv', row.names=FALSE)
##shinystan::launch_shinystan(m_hier) ##For parameter diagnostics

## Base line model to predict 2016
ols <- lm(ZillowAdj2 ~ prev_zillow + log_prev_crime, data=df_mod)
ols_coef <- ols$coefficients
pred_out$base_pred <- ols_coef[1] +  df_pred$ZillowAdj2 * ols_coef[2] + df_pred$log_crime * ols_coef[3]
pred_out <- merge(pred_out, df_zillow[Year==2016 |Year ==2017][,.(ZIP, Year, ZillowAdj2)], by.x=c("ZIP", "Year"), by.y=c("ZIP", "Year"))
MAPE_stan <- sum(abs(pred_out$mean - pred_out$ZillowAdj2)/pred_out$ZillowAdj2)/dim(pred_out)[1] ##11.6%
MAPE_base <- sum(abs(pred_out$base_pred - pred_out$ZillowAdj2)/pred_out$ZillowAdj2)/dim(pred_out)[1]
