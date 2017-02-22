###Model Development: Final Models
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Final, reduced models for comparision and performance assessment.  Six final models selected.  Models developed with three methods:
##    - Ordinary least squares (OLS)
##    - Robust regression (M-estimation with Tukey bisquare) using OLS model structure
##    - Robust regression (M-estimation with Tukey bisquare) created from backwards selection
## For each method, two models developed:
##    - First order terms only
##    - First order terms plus two-way interactions with region variable

## Best model selected (1st order OLS) and additional analysis completed (influence diagnostics, standardized terms)

## Summary of model performance functions:
##    modelStats(model, trainDF): assess model performance with training data set [R2, adjusted R2, standard error]
##    predictStats(model, testDF): assess model performance with test data set [predicted values, residuals, R2, MSE, std error]

## Jacey Planteen; R-Day 11/20/15

require(plyr); require(car); require(MASS)

load("popData.Rdata")

# Model Performance Functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
modelStats <- function(model, trainDF){
#Input model and training data frame and output R2, adjusted R2, and standard error
  
  #Calculate R2 and R2 adjusted
  SST.train <- sum(trainDF$Ratio2^2) - sum(trainDF$Ratio2)^2/nrow(trainDF)
  R2 <- 1 - sum(resid(model)^2)/SST.train
  R2adj <- 1 - (sum(resid(model)^2)/(nrow(trainDF) - length(model$coefficients)))/
      (SST.train/(nrow(trainDF) - 1))
      
  #Calculate standard error
  StdE <- sqrt(sum(resid(model)^2)/(nrow(trainDF) - length(model$coefficients)))

  return(list(R2=R2, R2adj=R2adj, StdErr=StdE))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
predictStats <- function(model, testDF){
#Input model and test data set and output predicted values, residuals, R2 for test data,
# MSE for test data, and standard error for test data

  predix <- predict(model, testDF) #Calculate new values
  resids <- testDF$Ratio2 - predix #Calculate residuals
  
  #Calculate R2 Predicted
  SST <- sum(testDF$Ratio2^2) - sum(testDF$Ratio2)^2/nrow(testDF)
  R2pred <- 1 - sum(resids^2)/SST
  
  #Calculate residual mean square error and standard error
  MSE <- sum(resids^2)/(nrow(testDF) - length(model$coefficients))
  StdE <- sqrt(MSE)
  
  return(list(predix=predix, resid=resids, R2pred=R2pred, MSE=MSE, StdErr=StdE))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Model Development
################################################################

#Final, reduced 1st order OLS model
OLS.1st.final <- lm(Ratio2 ~ Region + Male + Age.Under5 + Age.65.Plus + 
	Race.AmerIndian + Race.Asian + Household.Size + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance + Arts + PublicAdmin + Poverty.Indiv + 
	log(Density) + College, data=train.data)
summary(OLS.1st.final)
vif(OLS.1st.final)
plot(OLS.1st.final)
avPlots(OLS.1st.final)
resid.OLS.1st <- resid(OLS.1st.final)

#Final, reduced OLS model with two-way interactions with region
OLS.int.final <- lm(Ratio2 ~ Region + Own.Housing + 
	Construction + Retail + Finance + Arts + PublicAdmin + 
	log(Density) + College + Region:Male + Region:Age.Under5 + 
	Region:Age.65.Plus + Region:Race.Asian + Region:Household.Size + 
	Region:House.Family.Delta + Region:HigherEduc.Grad + 
	Region:NativeBorn + Region:LaborForce + Region:Armed.Forces + 
	Region:Median.Income + 	Region:Agri + Region:Poverty.Indiv, 
	data=train.data)
summary(OLS.int.final)
plot(OLS.int.final)

## Reduced ROBUST REGRESSION using OLS Model Structure
ROLS.1st.final <- rlm(Ratio2 ~ Region + Male + Age.Under5 + Age.65.Plus + 
	Race.AmerIndian + Race.Asian + Household.Size + 
	House.Family.Delta + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance + Arts + PublicAdmin + 
	log(Density) + College, data=train.data, method="M", psi=psi.bisquare)
summary(ROLS.1st.final)
modelStats(ROLS.1st.final, train.data)

##Model with interactions
ROLS.int.final <- rlm(Ratio2 ~ Region + Own.Housing + 
	Construction + Retail + Finance + Arts + PublicAdmin + 
	log(Density) + College + Region:Male + Region:Age.Under5 + 
	Region:Age.65.Plus + Region:Race.Asian + Region:Household.Size + 
	Region:House.Family.Delta + Region:HigherEduc.Grad + 
	Region:NativeBorn + Region:LaborForce + Region:Armed.Forces + 
	Region:Median.Income + 	Region:Agri + Region:Poverty.Indiv, 
	data=train.data, method="M", psi=psi.bisquare)
summary(ROLS.int.final)
modelStats(ROLS.int.final, train.data)

#"ORGANIC" Robust Regression Final Model
Mest.1st.final <- rlm(Ratio2 ~ Region + Male + Median.Age + Age.Under5 +
	Age.65.Plus + Race.AmerIndian + Race.Asian + 
	log(Race.PacIslander+0.001) + Hispanic + Families + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Unemployed + Armed.Forces + Median.Income + 
	Agri + Construction + Mfg + Retail + Finance + Arts + 
	Poverty.Indiv + log(Density) + School, 
	data=train.data, method="M", psi=psi.bisquare)
summary(Mest.1st.final)
vif(Mest.1st.final)
modelStats(Mest.1st.final, train.data)

##"ORGANIC" Robust Regression Model w/ Interactions
Mest.int.final <- rlm(Ratio2 ~ Region + Never.Married + 
	Mfg + Finance + Hispanic + 
	log(Race.PacIslander+0.001) + Arts + School + Median.Income + 
	Unemployed + Region:Male + 
	Region:Median.Age + Region:Age.Under5 +
	Region:Race.Asian + Region:Families + Region:House.Family.Delta + 
	Region:Own.Housing + Region:HigherEduc.Grad + Region:NativeBorn + 
	Region:LaborForce + Region:Armed.Forces + Region:Agri + 
	Region:Construction + Region:Retail,
	data=train.data, method="M", psi=psi.bisquare)
summary(Mest.int.final)
modelStats(Mest.int.final, train.data)


###Calculate comparison statistics
#AIC
AIC(OLS.1st.final)
AIC(ROLS.1st.final)
AIC(Mest.1st.final)
AIC(OLS.int.final)
AIC(ROLS.int.final)
AIC(Mest.int.final)

#BIC
BIC(OLS.1st.final)
BIC(ROLS.1st.final)
BIC(Mest.1st.final)
BIC(OLS.int.final)
BIC(ROLS.int.final)
BIC(Mest.int.final)


####PREDICTION  with test data
################################################################
OLS.1st.pred <- predictStats(OLS.1st.final, test.data)
OLS.int.pred <- predictStats(OLS.int.final, test.data)
ROLS.1st.pred <- predictStats(ROLS.1st.final, test.data)
ROLS.int.pred <- predictStats(ROLS.int.final, test.data)
Mest.1st.pred <- predictStats(Mest.1st.final, test.data)
Mest.int.pred <- predictStats(Mest.int.final, test.data)


##Select BEST model: OLS 1st Order
################################################################
model.data$predix.temp <- predict(OLS.1st.final, model.data)
model.data$predix <- 1/model.data$predix.temp - 1
model.data$resids <- model.data$Change - model.data$predix

##Look at influence diagnosics
OLS.1st.influ <- influence.measures(OLS.1st.final)
which(apply(OLS.1st.influ$is.inf, 1, any))
influ.obs <- summary(OLS.1st.influ)
cov.rats <- sort(influ.obs[, 29])

#Calculate standardized coefficients for chosen model
OLS.1st.std <- lm(scale(Ratio2) ~ scale(as.numeric(Region=="Midwest")) + 
	scale(as.numeric(Region=="South")) + scale(as.numeric(Region=="West")) + 
	scale(Male) + scale(Age.Under5) + scale(Age.65.Plus) + scale(Race.AmerIndian) + 
	scale(Race.Asian) + scale(Household.Size) + scale(House.Family.Delta) + 
	scale(Own.Housing) + scale(HigherEduc.Grad) + scale(Never.Married) + 
	scale(NativeBorn) + scale(LaborForce) + scale(Armed.Forces) + 
	scale(Median.Income) + scale(Agri) + scale(Construction) + 
	scale(Retail) + scale(Finance) + scale(Arts) + scale(PublicAdmin) + 
	scale(Poverty.Indiv) + scale(log(Density)) + scale(College), 
	data=train.data)
summary(OLS.1st.std)

##Pull select observations
select.inds <- c(1151, 1119, 103, 272, 3132, 515, 389, 409, 414, 441, 439, 448)
select.obs <- model.data[select.inds, ]
select.pred <- predict(OLS.1st.final, select.obs, interval="predict")
select.pred <- 1/select.pred - 1
select.obs[, c(2, 9, 10)]

save(model.data, OLS.1st.final, OLS.1st.std, file="FinalModel.Rdata")
