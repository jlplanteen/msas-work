##############################################################
### ANALYSIS of SALES RANKS
##############################################################

# Product data were scraped from Amazon in 8 waves over the course of 2 weeks in June 2016.
# Read in and merge the 8 individual files with raw data and perform preliminary analysis.
# Condense readings to average values per product ID (longitudinal analysis not fruitful).  Create two models
# using linear regression: one model including review volume, and one excluding.  Thought process for excluding
# reviews volume is that products that have higher sales volumes naturally have more oppportunities for reviews.

require(sqldf); require(ggplot2); require(car)

## Data Compilation & Sample Statistics / Plots
##############################################################

#Read in all raw data files and compile
#Create vector of file names
days <- seq(15, 29, by=2)
filenames <- paste("jun", days, ".RData", sep="")

#Combine data from all days into one file
for (i in 1:length(filenames)){
	load(filenames[i])
	battery.details$Wave <- i
	if (i==1){
		raw.data <- battery.details
	} else {
		raw.data <- rbind(raw.data, battery.details)
	}
}

#Assign brands
raw.data$Brand[grep("Odyssey", raw.data$Name, ignore.case=TRUE)] <- "Odyssey"
raw.data$Brand[grep("Exide", raw.data$Name, ignore.case=TRUE)] <- "Exide"
raw.data$Brand[grep("Optima", raw.data$Name, ignore.case=TRUE)] <- "Optima"
raw.data$Brand[grep("Hawker", raw.data$Name, ignore.case=TRUE)] <- "Odyssey"
raw.data$Brand[grep("Odyssey", raw.data$URL, ignore.case=TRUE)] <- "Odyssey"

#Assign Product IDs
raw.data$ID <- as.numeric(raw.data$Name)

#Write compiled data to output file
write.csv(raw.data, file="AmazonSales.csv", na="",row.names=FALSE)

## Creation of summary statistics by brand
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
brand.summary <- sqldf('select Brand, count(ID)/8 as N, avg(Price) as AvgPrice, 
				avg(Rating)	as AvgRating, avg(ReviewQty) as AvgReviewQty,
				avg(RankBatteries) as AvgRank, min(RankBatteries) as
				MinRank, max(RankBatteries) as MaxRank from "raw.data" 
				group by Brand')

## Creation of individual level graphics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
big.changes <- raw.data[raw.data$ID %in% c(3, 26, 79, 80, 95),] #Product IDs with big changes in sales rank over time
stable.IDs <- raw.data[raw.data$ID %in% c(6, 23, 43, 66, 107),] #Product IDs with small changes in sales rank over time

#Plot individuals with large swings in sales rank over time
rank.BIG <- ggplot(big.changes, aes(x=Wave, y=RankBatteries, group=as.factor(ID))) +
		coord_cartesian() + geom_line(aes(color=as.factor(ID)), size=1.1) + theme_bw() +
		labs(x="Date", y="Sales Rank in Batteries") + scale_x_continuous(
		breaks=c(2,4,6,8), labels=c('Jun 17', 'Jun 21', 'Jun 25', 'Jun 29')) +
		theme(legend.position="bottom", legend.key=element_blank()) + 
		scale_y_continuous(limits = c(0,6000)) + scale_color_brewer(
		name="Battery ID", type="qual", palette=3)

#Plot individuals with relatively stable sales rank over time
rank.stable <- ggplot(stable.IDs, aes(x=Wave, y=RankBatteries, group=as.factor(ID))) +
		coord_cartesian() + geom_line(aes(color=as.factor(ID)), size=1.1) + theme_bw() +
		labs(x="Date", y="Sales Rank in Batteries") + scale_x_continuous(
		breaks=c(2,4,6,8), labels=c('Jun 17', 'Jun 21', 'Jun 25', 'Jun 29')) +
		theme(legend.position="bottom", legend.key=element_blank()) + 
		scale_y_continuous(limits = c(0,6000)) + scale_color_brewer(
		name="Battery ID", type="qual", palette=3)


## Data Preparation for Modeling
##############################################################

#Create an average summary by battery ID (condense 8 readings over time to a single reading per ID)
raw.summary <- sqldf('select ID, Brand, avg(Price) as Price, 
				avg(Rating)	as Rating, avg(ReviewQty) as ReviewQty,
				avg(RankAuto) as RankAuto, avg(RankBatteries) as 
				RankBatteries from "raw.data" group by ID, Brand')

#Create additional variables
raw.summary$LRank <- log(raw.summary$RankBatteries) #Log rank response
raw.summary$Rating[is.na(raw.summary$Rating)] <- 0 #Set no reviews to 0
raw.summary$NoReviews <- as.numeric(raw.summary$ReviewQty==0) #Indicator
raw.summary$LReviewQty <- (raw.summary$ReviewQty)^(1/4) #4th root volume
raw.summary$Rating2 <- raw.summary$Rating #Create a copy of rating
raw.summary$Rating2[raw.summary$Rating==0] <- 3 #Set no reviews to rating of 3

##Split into test and training data sets; 25% of data to test set; 75% to training set
set.seed(152983)
test.inds <- sample(1:nrow(raw.summary), 0.25*nrow(raw.summary))
train.data <- raw.summary[-test.inds,]
test.data <- raw.summary[test.inds,]


## MODEL A DEVELOPMENT & ANALYSIS - Review Volume Included
##############################################################
##Fit full model, including reviews volume as a predictor
full.model <- lm(RankBatteries ~ Brand + Price + LReviewQty + Rating +
			Brand:Price + Brand:LReviewQty + Brand:Rating + 
			Price:LReviewQty + Price:Rating + LReviewQty:Rating + 
			Brand:Price:LReviewQty + Brand:Price:Rating + 
			Brand:LReviewQty:Rating + Price:LReviewQty:Rating, 
			data=train.data)
#Check for appropriate response transformation
boxCox(full.model)#Indicates log transformation appropriate

#Fit full model with log transformation
full.model.A <- lm(LRank ~ Brand + Price + LReviewQty + Rating2 + NoReviews +
			Brand:Price + Brand:LReviewQty + Brand:Rating2 + Brand:NoReviews +
			Price:LReviewQty + Price:Rating2 + Price:NoReviews + LReviewQty:Rating + 
			Brand:Price:LReviewQty + Brand:Price:Rating2 + 
			Brand:LReviewQty:Rating2 + Price:LReviewQty:Rating2, 
			data=train.data)
summary(full.model.A)

#Reduce model as possible
reduced.model.A <- lm(LRank ~ LReviewQty + Rating2 + NoReviews, data=train.data)
summary(reduced.model.A)
vif(reduced.model.A)
avPlots(reduced.model.A)
anova(reduced.model.A) #Type I Effects Analysis
anova(reduced.model.A, full.model.A) #Likelihood ratio test

mean(reduced.model.A$residuals^2) #Calculate MSE
AIC(reduced.model.A)
par(mfrow=c(2,2))
plot(reduced.model.A) #Residuals plots

#Influence diagnositics
infl.A <- influence.measures(reduced.model.A)
summary(infl.A)
par(mfrow=c(1,3))
cov.A <- infl.A$infmat[,"cov.r"]
plot(cov.A, ylab="Covariance Ratio")
abline(h=1, lty=2)
dffit.A <- infl.A$infmat[,"dffit"]
plot(dffit.A, ylab="DFFITs")
abline(h=0, lty=2)
cookD.A <- infl.A$infmat[,"cook.d"]
plot(cookD.A, ylab="Cook's D")


## MODEL B DEVELOPMENT & ANALYSIS - Review Volume Excluded
##############################################################

#Fit full model
full.model.B <- lm(LRank ~ Brand + Price + Rating2 + NoReviews +
			Brand:Price + Brand:Rating2 + Brand:NoReviews +
			Price:Rating2 + Price:NoReviews + Brand:Price:Rating2 + 
			Brand:Price:NoReviews, 
			data=train.data)
summary(full.model.B)

#Reduce model as possible
reduced.model.B <- lm(LRank ~ Brand + Price + Rating2 + NoReviews +
			Brand:Price + Price:Rating2, data=train.data)
summary(reduced.model.B)
vif(reduced.model.B)
summary(aov(reduced.model.B)) #Type I analysis of effects
anova(reduced.model.B, full.model.B) #Likelihood ratio test
avPlots(reduced.model.B)

MSE.B <- mean(reduced.model.B$residuals^2) #MSE calculation
AIC(reduced.model.B)
par(mfrow=c(2,2))
plot(reduced.model.B) #residuals plots

#Influence diagnositics
infl.B <- influence.measures(reduced.model.B)
summary(infl.B)
par(mfrow=c(1,3))
cov.B <- infl.B$infmat[,"cov.r"]
plot(cov.B, ylab="Covariance Ratio")
abline(h=1, lty=2)
dffit.B <- infl.B$infmat[,"dffit"]
plot(dffit.B, ylab="DFFITs")
abline(h=0, lty=2)
cookD.B <- infl.B$infmat[,"cook.d"]
plot(cookD.B, ylab="Cook's D")

#Residuals versus reviews volume
par(mfrow=c(1,1))
plot(train.data$LReviewQty, reduced.model.B$residuals, ylab="Standardized 
	Residuals", xlab="Reviews Volume ^ (1/4)")
abline(h=0, lty=2)


## Validation of Models with Test Data
##############################################################

#Calculate predicted values
test.A <- predict(reduced.model.A, newdata=test.data)
test.B <- predict(reduced.model.B, newdata=test.data)

#Calculate residuals
resid.A <- test.data$LRank - test.A
resid.B <- test.data$LRank - test.B

#Calculate MSE on new observations
MSE.test.A <- mean(resid.A^2)
MSE.test.B <- mean(resid.B^2)

#Calculate approximate R^2 on new data
SST <- sum(test.data$LRank^2)-sum(test.data$LRank)^2/
		nrow(test.data) #Sum of squares total
R2.A <- 1 - sum(resid.A^2)/SST
R2.B <- 1 - sum(resid.B^2)/SST


## Predicted Value Plots
##############################################################

#Model A: Predicted Sales Rank vs. Rating & Reviews Volume
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create new observations for predictions
ReviewQty <- rep(seq(0, 400, by=2), times=3)
Rating <- rep(seq(3, 5, by=1), each = 201)
new.vals.A <- data.frame(ReviewQty=ReviewQty, Rating=Rating)
new.vals.A$LReviewQty <- (new.vals.A$ReviewQty)^(1/4)
new.vals.A$NoReviews <- as.numeric(new.vals.A$ReviewQty==0)
new.vals.A$Rating2 <- new.vals.A$Rating
new.vals.A$Rating2[new.vals.A$NoReviews==1] <- 3

#Predict sales ranks
temp <- predict(reduced.model.A, newdata=new.vals.A)
new.vals.A$Pred.LRank <- temp
new.vals.A$Pred.Rank <- exp(new.vals.A$Pred.LRank)

#Plot sales rank versus reviews volume, grouped by rating
modelA.pred <- ggplot(new.vals.A, aes(x=ReviewQty, y=Pred.Rank, group=Rating)) + 
		geom_line(aes(color=as.factor(Rating)), size=1) + theme_bw() +
		labs(x="Customer Reviews Volume", y="Sales Rank in Batteries") +
		theme(legend.position="bottom", legend.key=element_blank()) +
		scale_colour_discrete(name="Customer Review Rating")


#Model B: Predicted Sales Rank vs. Price by Brand & Rating
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create new observations for predictions
Price <- rep(seq(125, 250, by=5),times=9)
Brand <- rep(rep(c("Odyssey", "Optima", "Exide"), each=26), times=3)
Rating <- rep(c(3,3,5), each=78)
NoReviews <- rep(c(1,0,0),each=78)
new.vals.B <- data.frame(Brand=Brand, Price=Price, Rating2=Rating, NoReviews=NoReviews)

#Predict new values
temp <- predict(reduced.modelB, newdata=new.vals.B)
new.vals.B$Pred.LRank <- temp
new.vals.B$Pred.Rank <- exp(new.vals.B$Pred.LRank)

#Plot sales rank versus price, grouped by brand for NO REVIEWS
modelB.pred.0 <- ggplot(new.vals.B[new.vals.B$NoReviews==1,], aes(x=Price, y=Pred.Rank, 
		group=Brand)) + geom_line(aes(color=Brand), size=1) + theme_bw() + 
		labs(x="Price ($)", y="Sales Rank in Batteries") +
		theme(legend.position="bottom", legend.key=element_blank()) + 
		scale_color_brewer(name="Brand", type="qual", palette=3) + 
		scale_y_continuous(limits = c(0,5000))

#Plot sales rank versus price, grouped by brand for 3 STAR Rating
modelB.pred.3 <- ggplot(new.vals.B[new.vals.B$Rating==3 & new.vals.B$NoReviews==0,], 
		aes(x=Price, y=Pred.Rank, group=Brand)) + geom_line(aes(color=Brand), 
		size=1) + theme_bw() + labs(x="Price ($)", y="Sales Rank in Batteries") +
		theme(legend.position="bottom", legend.key=element_blank()) + 
		scale_color_brewer(name="Brand", type="qual", palette=3) + 
		scale_y_continuous(limits = c(0,5000))

#Plot sales rank versus price, grouped by brand for 5 STAR Rating
modelB.pred.5 <- ggplot(new.vals.B[new.vals.B$Rating==5,], aes(x=Price, y=Pred.Rank, 
		group=Brand)) + geom_line(aes(color=Brand), size=1) + theme_bw() + 
		labs(x="Price ($)", y="Sales Rank in Batteries") +
		theme(legend.position="bottom", legend.key=element_blank()) + 
		scale_color_brewer(name="Brand", type="qual", palette=3) + 
		scale_y_continuous(limits = c(0,5000))
 
