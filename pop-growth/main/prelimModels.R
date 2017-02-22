### Model Development
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Preliminary model development and reduction.  Analysis and elimination of multi-collinearity issues, 
# residuals analysis, and model simplification. 

## Jacey Planteen; R-Day 11/20/15

require(plyr); require(car); require(MASS)
load("popData.R")

#### Model Development and Reduction
################################################################

## 1st order OLS Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fit.orig <- lm(Ratio ~ Region + Metro.Micro + Male + Median.Age + 
	Age.Under5 + Age.18.Plus + Age.65.Plus + Race.White + Race.AfrAmer + Race.AmerIndian +
	Race.Asian + Race.PacIslander + Hispanic + Families + Household.Size + 
	Family.Size + Own.Housing + HighSchool.Grad + HigherEduc.Grad + Never.Married +
	NativeBorn + LaborForce + Unemployed + Armed.Forces + Median.Income + Agri + 
	Construction + Mfg + Wholesale + Retail + Trans + Info + Finance + Professional + 
	Educ + Arts + PublicAdmin + Poverty.Family + Poverty.Indiv + Density + College 
	+ School, data=train.data)
summary(fit.orig)
vif(fit.orig)
boxCox(fit.orig)  #Indicates inverse response may be better

#Graphically examine some potential multi-collinearity issues
plot(train.data$Household.Size, train.data$Family.Size)
plot(train.data$House.Family.Delta, train.data$Family.Size)
cor(train.data$Household.Size, train.data$Family.Size) #Drop Family.Size

##Using transformed response and House.Family.Delta instead of Family.Size
OLS.1st.r0 <- lm(Ratio2 ~ Region + Metro.Micro + Male + Median.Age + Age.Under5 +
	Age.18.Plus + Age.65.Plus + Race.White + Race.AfrAmer + Race.AmerIndian +
	Race.Asian + Race.PacIslander + Hispanic + Families + Household.Size + 
	House.Family.Delta + Own.Housing + HighSchool.Grad + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Unemployed + Armed.Forces + Median.Income + Agri + 
	Construction + Mfg + Wholesale + Retail + Trans + Info + Finance + Professional + 
	Educ + Arts + PublicAdmin + Poverty.Family + Poverty.Indiv + Density + College 
	+ School, data=train.data)
summary(OLS.1st.r0)
vif(OLS.1st.r0)

##Transform regressors where extreme leverage exists
OLS.1st.r1 <- lm(Ratio2 ~ Region + Metro.Micro + Male + Median.Age + Age.Under5 +
	Age.18.Plus + Age.65.Plus + Race.White + Race.AfrAmer2 + Race.AmerIndian +
	Race.Asian + log(Race.PacIslander+0.001) + Hispanic + Families + Household.Size + 
	House.Family.Delta + Own.Housing + HighSchool.Grad + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Unemployed + Armed.Forces + Median.Income + Agri + 
	Construction + Mfg + Wholesale + Retail + Trans + Info + Finance + Professional + 
	Educ + Arts + PublicAdmin + Poverty.Family + Poverty.Indiv + log(Density) + College 
	+ School, data=train.data)
summary(OLS.1st.r1)
vif(OLS.1st.r1)

##Reduced model to resolve the biggest VIF issues
OLS.1st.r2 <- lm(Ratio2 ~ Region + Metro.Micro + Male + Median.Age + Age.Under5 +
	Age.65.Plus + Race.White + Race.AfrAmer2 + Race.AmerIndian +
	Race.Asian + log(Race.PacIslander+0.001) + Hispanic + Household.Size + 
	House.Family.Delta + Own.Housing + HighSchool.Grad + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Unemployed + Armed.Forces + Median.Income + Agri + 
	Construction + Wholesale + Retail + Trans + Info + Finance + Professional + 
	Educ + Arts + PublicAdmin + Poverty.Indiv + log(Density) + College 
	+ School, data=train.data)
summary(OLS.1st.r2)
anova(OLS.1st.r2, OLS.1st.r1)
vif(OLS.1st.r2)


#Continue to reduce model
OLS.1st.r3 <- lm(Ratio2 ~ Region + Male + Median.Age + Age.Under5 +
	Age.65.Plus + Race.AmerIndian + Race.Asian + Household.Size + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance +  
	Arts + PublicAdmin + Poverty.Indiv + log(Density) + College,
	data=train.data)
summary(OLS.1st.r3)
anova(OLS.1st.r3, OLS.1st.r1)
vif(OLS.1st.r3)
plot(OLS.1st.r3)
avPlots(OLS.1st.r3)

plot(train.data$Race.White, train.data$Race.AfrAmer)
plot(train.data$Race.White, train.data$Race.AfrAmer2)
plot(train.data$Median.Age, train.data$Age.65.Plus)
plot(train.data$Race.PacIslander, train.data$Ratio2)
plot(log(train.data$Race.PacIslander+0.001), train.data$Ratio2)

coplot(Ratio2 ~ School | Region, data=train.data)

#Further reduce model
OLS.1st.final <- lm(Ratio2 ~ Region + Male + Age.Under5 + Age.65.Plus + 
	Race.AmerIndian + Race.Asian + Household.Size + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance + Arts + PublicAdmin + Poverty.Indiv + 
	log(Density) + College, data=train.data)
summary(OLS.1st.final)
anova(OLS.1st.final, OLS.1st.r1)
vif(OLS.1st.final)
plot(OLS.1st.final)
avPlots(OLS.1st.final)
resid.OLS.1st <- resid(OLS.1st.final)


## OLS Model with interactions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Try adding interactions with Region
OLS.int.r0 <- lm(Ratio2 ~ Region + Male + Age.Under5 + Age.65.Plus + 
	Race.AmerIndian + Race.Asian + Household.Size + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance + Arts + PublicAdmin + Poverty.Indiv + 
	log(Density) + College + Region:Male + Region:Age.Under5 + 
	Region:Age.65.Plus + Region:Race.AmerIndian + Region:Race.Asian + 
	Region:Household.Size + Region:House.Family.Delta + Region:Own.Housing + 
	Region:HigherEduc.Grad + Region:Never.Married + Region:NativeBorn + 
	Region:LaborForce + Region:Armed.Forces + Region:Median.Income + 
	Region:Agri + Region:Construction + Region:Retail + Region:Finance + 
	Region:Arts + Region:PublicAdmin + Region:Poverty.Indiv + 
	Region:log(Density) + Region:College, data=train.data)
summary(OLS.int.r0)

#Reduce higher order terms
OLS.int.r1 <- lm(Ratio2 ~ Region + Male + Age.Under5 + Age.65.Plus + 
	Race.AmerIndian + Race.Asian + Household.Size + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance + Arts + PublicAdmin + Poverty.Indiv + 
	log(Density) + College + Region:Male + Region:Age.Under5 + 
	Region:Age.65.Plus + Region:Race.Asian + 
	Region:Household.Size + Region:House.Family.Delta + 
	Region:HigherEduc.Grad + Region:NativeBorn + 
	Region:LaborForce + Region:Armed.Forces + Region:Median.Income + 
	Region:Agri + 
	Region:Poverty.Indiv, data=train.data)
summary(OLS.int.r1)
anova(OLS.int.r1, OLS.int.r0)
vif(OLS.int.r1)
plot(OLS.int.r1)

#Reduce lower order terms
OLS.int.r2 <- lm(Ratio2 ~ Region + Male + Age.Under5 + Age.65.Plus + 
	Race.Asian + Household.Size + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance + Arts + PublicAdmin + Poverty.Indiv + 
	log(Density) + College + Region:Male + Region:Age.Under5 + 
	Region:Age.65.Plus + Region:Race.Asian + 
	Region:Household.Size + Region:House.Family.Delta + 
	Region:HigherEduc.Grad + Region:NativeBorn + 
	Region:LaborForce + Region:Armed.Forces + Region:Median.Income + 
	Region:Agri + 
	Region:Poverty.Indiv, data=train.data)
summary(OLS.int.r2)
anova(OLS.int.r2, OLS.int.r0)

OLS.int.final <- lm(Ratio2 ~ Region + Own.Housing + 
	Construction + Retail + Finance + Arts + PublicAdmin + 
	log(Density) + College + Region:Male + Region:Age.Under5 + 
	Region:Age.65.Plus + Region:Race.Asian + Region:Household.Size + 
	Region:House.Family.Delta + Region:HigherEduc.Grad + 
	Region:NativeBorn + Region:LaborForce + Region:Armed.Forces + 
	Region:Median.Income + 	Region:Agri + Region:Poverty.Indiv, 
	data=train.data)
summary(OLS.int.final)
anova(OLS.int.final, OLS.int.r0)
plot(OLS.int.final)

## 1st order robust regression using OLS structure
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ROLS.1st.r0 <- rlm(Ratio2 ~ Region + Male + Age.Under5 + Age.65.Plus + 
	Race.AmerIndian + Race.Asian + Household.Size + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance + Arts + PublicAdmin + Poverty.Indiv + 
	log(Density) + College, data=train.data, method="M", psi=psi.bisquare)
summary(ROLS.1st.r0)

##Reduce further based off t-values
ROLS.1st.final <- rlm(Ratio2 ~ Region + Male + Age.Under5 + Age.65.Plus + 
	Race.AmerIndian + Race.Asian + Household.Size + 
	House.Family.Delta + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Armed.Forces + Median.Income + Agri + 
	Construction + Retail + Finance + Arts + PublicAdmin + 
	log(Density) + College, data=train.data, method="M", psi=psi.bisquare)
summary(ROLS.1st.final)


## Robust regression with interactions using OLS structure
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


## "Organic" 1st order robust regression 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Mest.1st.r0 <- rlm(Ratio2 ~ Region + Metro.Micro + Male + Median.Age + Age.Under5 +
	Age.18.Plus + Age.65.Plus + Race.White + Race.AfrAmer2 + Race.AmerIndian +
	Race.Asian + log(Race.PacIslander+0.001) + Hispanic + Families + Household.Size + 
	House.Family.Delta + Own.Housing + HighSchool.Grad + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Unemployed + Armed.Forces + Median.Income + Agri + 
	Construction + Mfg + Wholesale + Retail + Trans + Info + Finance + Professional + 
	Educ + Arts + PublicAdmin + Poverty.Family + Poverty.Indiv + Density + College 
	+ School, data=train.data, method="M", psi=psi.bisquare)
summary(Mest.1st.r0)

##"ORGANIC" Robust Regression Model
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


## "Organic" robust regression model with interactions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Mest.int.r0 <- rlm(Ratio2 ~ Region + Male + Median.Age + Age.Under5 +
	Age.65.Plus + Race.AmerIndian + Race.Asian + 
	log(Race.PacIslander+0.001) + Hispanic + Families + 
	House.Family.Delta + Own.Housing + HigherEduc.Grad + Never.Married + 
	NativeBorn + LaborForce + Unemployed + Armed.Forces + Median.Income + 
	Agri + Construction + Mfg + Retail + Finance + Arts + 
	Poverty.Indiv + log(Density) + School + Region:Male + 
	Region:Median.Age + Region:Age.Under5 + Region:Age.65.Plus + 
	Region:Race.AmerIndian + Region:Race.Asian + 
	Region:log(Race.PacIslander+0.001) + Region:Hispanic + Region:Families + 
	Region:House.Family.Delta + Region:Own.Housing + 
	Region:HigherEduc.Grad + Region:Never.Married + Region:NativeBorn + 
	Region:LaborForce + Region:Unemployed + Region:Armed.Forces + 
	Region:Median.Income + Region:Agri + Region:Construction + Region:Mfg + 
	Region:Retail + Region:Finance + Region:Arts + 
	Region:Poverty.Indiv + Region:log(Density) + Region:School,
	data=train.data, method="M", psi=psi.bisquare)
summary(Mest.int.r0)

##Reduced model
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
