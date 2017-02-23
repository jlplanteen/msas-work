##########################################################################
## Customer Reviews Analysis
#########################################################################

## Gain insights from customer reviews posted to Amazon.  Process / clean customer reviews.
# Determine most frequent terms (single words, bi-grams, and tri-grams).  Visualizes frequent terms
# in word clouds both by brand and by review rating.
# Creates linear model for durations (in years) mentioned in reviews.
# Tests for association of certain chosen key terms (whether or not they appear in review) with brand / rating.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summary of Text Processing Functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# getWordList(dtm): takes in document term matrix and returns sorted list of word frequency
# customStemmer(Texts, dictionary): word stemming function using custom dictionary
# numToText(Texts): replaces written integer values with digits in supplied texts (i.e. "one" becomes "1")
# gram2Tokenizer(Texts): creates two word tokens
# gram3Tokenizer(Texts): creates three word tokens
# NGramFixer(Texts, NGrams): takes in array of NGrams and converts those NGrams to single tokens in Texts
# wordFlagger(DF, Texts, Flag): indicate if flag term found in texts and add Boolean value to data frame

load("Comments.RData")
require(tm); require(wordcloud); require(sqldf); require(ggplot2); require(Deducer)
require(RWeka); require(car)

## Text Cleaning & Analysis Functions
##############################################################

##Function yields sorted list of word frequencies in a doc term matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getWordList <- function(dtm){
#Function that takes in a document text matrix and returns
#a sorted list of word frequency

	words <- colnames(dtm)
	freqs <- apply(dtm, 2, sum)

	word.list <- data.frame(Word=words, Freq=freqs)

	w.order <- sort.int(word.list$Freq, decreasing=TRUE, 
				index.return=TRUE)

	word.list <- word.list[w.order$ix,]
	rownames(word.list) <- 1:nrow(word.list)

	return(word.list)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Word stemming function that uses custom dictionary
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
customStemmer <- function(Texts, Dictionary){
	for (i in 1:nrow(Dictionary)){
		Texts <- gsub(Dictionary$Original[i], Dictionary$Replacement[i],
				Texts, ignore.case=TRUE)
	}
	return(Texts)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##Number substitutions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
numsToText <- function(Texts){
	integers <- c(' 1 ',' 2 ',' 3 ',' 4 ',' 5 ',' 6 ',' 7 ',' 8 ',
			' 9 ',' 10 ',' 11 ',' 12 ')
	numbers <- c(' one ',' two ',' three ',' four ',' five ',' six ',
			' seven ',' eight ',' nine ',' ten ',' eleven ',' twelve ')
	for (i in 1:length(integers)){
		Texts <- gsub(integers[i], numbers[i], Texts,)
	}

	return(Texts)

}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gram2Tokenizer <- function(x){
	NGramTokenizer(x, Weka_control(min=2,max=2))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gram3Tokenizer <- function(x){
	NGramTokenizer(x, Weka_control(min=3,max=3))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##Convert N-grams into single token
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NGramFixer <- function(Texts, NGrams){
	NGrams.new <- gsub(' ', 'XXX', NGrams)
	for (i in 1:length(NGrams)){
		Texts <- gsub(NGrams[i], NGrams.new[i], Texts, 
		ignore.case=TRUE)
	}
	return(Texts)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wordFlagger <- function(data, text, Flag){
#Indicate whether "Flag" term is found in texts, add Boolean to data
  if (nrow(data) != length(text)){
    return(-1)
  } else {
	  C <- ncol(data) + 1
	  inds <- grep(Flag, text)
	  data[,C] <- 0
	  data[inds, C] <- 1
	  names(data)[C] <- gsub(" ", ".", Flag)
	  return(data)
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Basic Data Preparation & Overall Analysis
##############################################################

##Assign brands
all.comments[grep("Odyssey", all.comments$Prod.Name),"Brand"] <- "Odyssey"
all.comments[grep("Optima", all.comments$Prod.Name),"Brand"] <- "Optima"
all.comments[grep("Exide", all.comments$Prod.Name),"Brand"] <- "Exide"
all.comments[grep("Hawker", all.comments$Prod.Name),"Brand"] <- "Odyssey"

#Convert date to POSIXt
all.comments$Date2 <- as.Date(all.comments$Date, "%B %d, %Y")

#Plot comment frequencies over time
date.hist <- ggplot(all.comments, aes(x=Date2, fill=Brand)) + 
		geom_histogram(bins=50) + scale_fill_brewer(
		name="Brand", type="qual", palette=3) + theme_bw() +
		labs(x="Date", y="Frequency of Customer Reviews") + 
		theme(legend.position=c(0.12,0.8))

##Reduce to only more recent reviews
comments.recent <- all.comments[all.comments$Date2 >= "2013-01-01",]

##Summarize selected comments by brand and rating
table(comments.recent$Brand)
table(comments.recent$Rating)
table(comments.recent$Rating, comments.recent$Brand)

##Test for association between brand and rating
pearson <- chisq.test(comments.recent$Rating,comments.recent$Brand)
pearson$observed
pearson$expected
pearson$residuals

G2.likelihood <- likelihood.test(comments.recent$Brand,comments.recent$Rating)

#Test for association between only Odyssey & Optima
pearson2 <- chisq.test(comments.recent$Rating[comments.recent$Brand!="Exide"],
		comments.recent$Brand[comments.recent$Brand!="Exide"])

#Create variable for whether comment was found helpful by anyone
comments.recent$Helpful <- as.numeric(comments.recent$Votes>0)
table(comments.recent$Rating, comments.recent$Helpful)

#Test for relationship between rating and helpfulness
cor.test(comments.recent$Rating, comments.recent$Votes, method="kendall")
cor.test(comments.recent$Rating, comments.recent$Votes, method="spearman")


## Text Processing & Cleaning
##############################################################
#Read-in custom dictionary for stemming
doc.stems <- read.csv("CustomStems.csv")

#Replace punctuation with spaces
comments.recent$Text2 <- gsub("[.!;,?/]", " ", comments.recent$Text)

#Stem reviews
comments.recent$Text2 <- customStemmer(comments.recent$Text2, doc.stems)

#Replace integers
comments.recent$Text2 <- numsToText(comments.recent$Text2)

#Find frequent N-grams
corpus <- VCorpus(DataframeSource(data.frame(comments.recent$Text2)))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

#Remove brand names & other custom stop words
corpus <- tm_map(corpus, removeWords, c("exide","optima","hawker","odyssey",
			"optimas", "battery", "batteries", "batterys"))
corpus <- tm_map(corpus, stripWhitespace)
dtm.3gram <- DocumentTermMatrix(corpus, control=list(tokenize=gram3Tokenizer))
freq.3grams <- removeSparseTerms(dtm.3gram, 0.998)
trigrams <- getWordList(freq.3grams)

dtm.2gram <- DocumentTermMatrix(corpus, control=list(tokenize=gram2Tokenizer))
freq.2grams <- removeSparseTerms(dtm.2gram, 0.996)
bigrams <- getWordList(freq.2grams)

#Output to .csv file for later use
write.csv(trigrams, file="Common3Grams.csv", row.names=FALSE)
write.csv(bigrams, file="Common2Grams.csv", row.names=FALSE)

##Read in modified N-gram lists (nonsense removed) and replace in reviews
list.3grams <- read.csv("Common3Grams.csv")
list.2grams <- read.csv("Common2Grams.csv")

comments.modified <- data.frame(Text=unlist(sapply(corpus, '[', "content")),
				stringsAsFactors=FALSE)
comments.modified$Text3 <- NGramFixer(comments.modified$Text, list.2grams$Word)

#Create new corpus
corpus2 <- VCorpus(DataframeSource(data.frame(comments.modified$Text3)))

#Remove specific product types & other custom stop words
corpus2 <- tm_map(corpus2, removeWords, c("redXXXtop", "yellowXXXtop", "redtop",
			"yellowtop", "amazon", "one", "car", "will", "two", "three",
			"four","buy","get","just","can","put","five", "com", "amazons",
			"use", "also", "say", "said","purchase","vehicle"))

#Create document term matrices
dtm.tf <- DocumentTermMatrix(corpus2)
dtm.tf <- removeSparseTerms(dtm.tf, 0.9995)
total.words <- getWordList(dtm.tf)
plot(total.words$Freq, type='l')
abline(h=20,col="red")

#Keep only the most common words (appear in at least 20 times)
dtm.tf <- dtm.tf[,(colnames(dtm.tf)%in% total.words[
				total.words$Freq>=20,"Word"])]
        
## Macro Analysis of cleaned texts / dtm
##############################################################

Brand.summary <- sqldf('select Brand, count(Brand) as N,
				sum(Votes) as Voted, avg(Rating) as "Avg.Rating" from
				"comments.recent" group by Brand')

##Make some word clouds
low.ratings <- dtm.tf[comments.recent$Rating<=2, ]
high.ratings <- dtm.tf[comments.recent$Rating>=4, ]
mid.ratings <- dtm.tf[comments.recent$Rating==3, ]

low.words <- getWordList(low.ratings)
low.words$Word <- gsub("xxx", "_", low.words$Word)
mid.words <- getWordList(mid.ratings)
mid.words$Word <- gsub("xxx", "_", mid.words$Word)
high.words <- getWordList(high.ratings)
high.words$Word <- gsub("xxx", "_", high.words$Word)

low.cols <- brewer.pal(9, "YlOrRd")[-(1:4)]
high.cols <- brewer.pal(9, "PuBuGn")[-(1:4)]
brand.cols <- brewer.pal(9, "BuPu")[-(1:4)]
wordcloud(words=low.words$Word, freq=low.words$Freq, max.words=75,
		random.order=FALSE, random.color=FALSE, colors=low.cols)
wordcloud(words=mid.words$Word, freq=mid.words$Freq, max.words=75,
		random.order=FALSE, random.color=FALSE, colors=brand.cols)
wordcloud(words=high.words$Word, freq=high.words$Freq, max.words=75,
		random.order=FALSE, random.color=FALSE, colors=high.cols)


##By Brand
Exide <- dtm.tf[comments.recent$Brand=="Exide",]
Exide.words <- getWordList(Exide)
Exide.words$Word <- gsub("xxx", "_", Exide.words$Word)
wordcloud(words=Exide.words$Word, freq=Exide.words$Freq, max.words=100,
		random.order=FALSE, random.color=FALSE, colors=brand.cols)

Optima <- dtm.tf[comments.recent$Brand=="Optima",]
Optima.words <- getWordList(Optima)
Optima.words$Word <- gsub("xxx", "_", Optima.words$Word)
wordcloud(words=Optima.words$Word, freq=Optima.words$Freq, max.words=100,
		random.order=FALSE, random.color=FALSE, colors=brand.cols)

Odyssey <- dtm.tf[comments.recent$Brand=="Odyssey",]
Odyssey.words <- getWordList(Odyssey)
Odyssey.words$Word <- gsub("xxx", "_", Odyssey.words$Word)
wordcloud(words=Odyssey.words$Word, freq=Odyssey.words$Freq, max.words=100,
		random.order=FALSE, random.color=FALSE, colors=brand.cols)


# Analyze durations of time mentioned in reviews
#################################################################
#Create new data frame
time.comments <- comments.recent[,c(2:7,9:12)]
time.words <- c('one years','two years','three years','four years',
		'five years', 'six years', 'seven years','eight years',
		'nine years', 'ten years', 'eleven years','twelve years')
time.comments$Years <- 0

for (i in 1:length(time.words)){
	inds <- grep(time.words[i], comments.modified$Text)
	time.comments$Years[inds] <- i
}
time.comments$Mention <- as.numeric(time.comments$Years>0)

table(time.comments$Years)
table(time.comments$Years, time.comments$Rating)
table(time.comments$Years, time.comments$Brand)
table(time.comments$Mention)
table(time.comments$Mention, time.comments$Rating)
table(time.comments$Mention, time.comments$Brand)
time.comments2 <- time.comments[time.comments$Mention==1,]
boxplot(Years ~ Brand, data=time.comments2)

#Test for correlation between time duration and rating
cor.test(time.comments2$Years, time.comments2$Rating, method="kendall")
cor.test(time.comments2$Years, time.comments2$Rating, method="spearman")

#Model years in review by brand and rating
time.comments2$Years2 <- sqrt(time.comments2$Years)
x <- aov(Years ~ Brand, data=time.comments2)
y <- lm(Years2 ~ Brand + Rating, data=time.comments2)
summary(x)
summary(y)
vif(y)
anova(y)
TukeyHSD(x, "Brand")
plot(x)
par(mfrow=c(2,2))
plot(y)
boxCox(y)

# Predict average time mentioned in review, given rating and brand
Rating <- rep(1:5, times=3)
Brand <- rep(c("Exide", "Odyssey", "Optima"), each=5)
predix <- data.frame(Brand=Brand, Rating=Rating)
temp <- predict(y, newdata=predix)
predix$Years <- temp^2

#Plot predicted durations
pred.duration <- ggplot(predix, aes(x=Rating, y=Years, group=Brand)) + 
		geom_line(aes(color=Brand),size=1) + 
		theme_bw() + labs(x="Rating (stars)", y="Duration (Years)") + 
		theme(legend.position="bottom", legend.key=element_blank()) + 
		scale_colour_brewer(name="Brand", type="qual", palette=3)

#Plot densities of durations by brand
brand.duration <- ggplot(time.comments2, aes(x=Years, group=Brand)) + 
		geom_density(kernel="gaussian", aes(fill=Brand),alpha=0.3) + 
		theme_bw() + labs(x="Years", y="Density") + 
		theme(legend.position=c(0.8,0.8)) + scale_fill_brewer(
		name="Brand", type="qual", palette=3)
brand.duration <- ggplot(time.comments2, aes(y=Years, x=Brand)) + 
		geom_boxplot(aes(fill=Brand),alpha=0.5) + 
		theme_bw() + labs(x="Brand", y="Years") + 
		theme(legend.position="bottom", legend.key=element_blank()) + 
		scale_fill_brewer(name="Brand", type="qual", palette=3)
    
#Plot densities of duration by rating
rating.duration <- ggplot(time.comments2, aes(x=Years, group=Rating)) + 
		geom_density(kernel="gaussian", aes(fill=as.factor(Rating),
		color=as.factor(Rating)),size=1,
		alpha=0.2) + theme_bw() + labs(x="Years", y="Density") + 
		theme(legend.position=c(0.8,0.8)) + scale_fill_discrete(
		name="Review Rating") + scale_color_discrete(name="Review Rating")
rating.duration <- ggplot(time.comments2, aes(y=Years, x=as.factor(Rating))) + 
		geom_boxplot(aes(fill=as.factor(Rating))) + theme_bw() + 
		labs(x="Rating (stars)", y="Years") + theme(legend.position="bottom") + 
		scale_fill_discrete(name="Review Rating")

#Grab terms of interest and analyze presence for association with brand and rating
################################################################################
#Create new data frame
key.comments <- comments.recent[,c(2:7,9:12)]
key.words <- c("prius", "dead", "great", "price", "warranty", "great price",
			"fit perfect", "perfect fit")
for (j in 1:length(key.words)){
	key.comments <- wordFlagger(key.comments,comments.modified$Text,
				key.words[j])
}

#Combine fit perfect and perfect fit variables
key.comments$fit.perfect <- key.comments$fit.perfect + key.comments$perfect.fit
key.comments$fit.perfect[key.comments$fit.perfect>1] <- 1
key.comments <- key.comments[,1:(ncol(key.comments)-1)]


#Determine number of reviews containing each of the key words
apply(key.comments[,11:17], 2, sum)

#Determine average customer review rating for each of the key words
key.words <- gsub(" ", ".", key.words)
m.ratings <- c()
for (j in 1:(length(key.words)-1)){
	m.ratings <- c(m.ratings, mean(key.comments$Rating[key.comments[,
				key.words[j]]==1]))
}


##Analysis for "Prius"
prius.tab <- sqldf('select Brand, Rating, prius, count(prius) as N 
			from "key.comments" group by Brand, Rating, prius')
prius.tab$Rating2 <- as.factor(prius.tab$Rating)
prius.fit <- glm(N ~ Brand + Rating2 + prius + Brand:Rating + Rating:prius + 
			Brand:prius, data=prius.tab, family=poisson())
summary(prius.fit)
Anova(prius.fit)
prius.fit2 <- glm(N ~ Brand + Rating2 + prius + Brand:Rating + 
			Brand:prius, data=prius.tab, family=poisson())
summary(prius.fit2)
pchisq(prius.fit2$deviance, df=prius.fit2$df.residual, lower.tail=FALSE)
Anova(prius.fit2)
anova(prius.fit2, prius.fit)
table(key.comments$prius, key.comments$Brand)
table(key.comments$prius, key.comments$Prod.Name)


##Analysis for "Great"
great.tab <- sqldf('select Brand, Rating, great, count(great) as N
			 from "key.comments" group by Brand, Rating, great')
great.tab$Rating2 <- as.factor(great.tab$Rating)
great.fit <- glm(N ~ Brand + Rating2 + great + Brand:Rating + 
			Rating:great + Brand:great, data=great.tab, 
			family=poisson())
summary(great.fit)
Anova(great.fit)
great.fit2 <- glm(N ~ Brand + Rating2 + great + Brand:Rating + 
			Rating:great, data=great.tab, family=poisson())
summary(great.fit2)
Anova(great.fit2)
pchisq(great.fit2$deviance, df=great.fit2$df.residual, lower.tail=FALSE)
anova(great.fit2, great.fit)
great.tab$fits <- great.fit2$fitted.values

#Odds of "great" in 5 stars vs 1 star
temp <- great.tab$fits[great.tab$Brand =='Optima' & great.tab$Rating %in% c(1,5)]
great.OR.stars <- temp[1]*temp[4]/(temp[2]*temp[3])


##Analysis for "Price"
price.tab <- sqldf('select Brand, Rating, price, count(price) as N
			 from "key.comments" group by Brand, Rating, price')
price.tab$Rating2 <- as.factor(price.tab$Rating)
price.fit <- glm(N ~ Brand + Rating2 + price + Brand:Rating + 
			Rating:price + Brand:price, data=price.tab, 
			family=poisson())
summary(price.fit)
Anova(price.fit)
pchisq(price.fit$deviance, df=price.fit$df.residual, lower.tail=FALSE)
price.tab$fits <- price.fit$fitted.values

##Calculate some odds ratios
#Odds of price =  1 in Optima vs Odyssey
temp <- price.tab$fits[price.tab$Brand %in% c('Optima','Odyssey') & price.tab$Rating == 5]
price.OR.brand <- temp[1]*temp[4]/(temp[2]*temp[3])

#Odds of price in 5 stars vs 1 star
temp <- price.tab$fits[price.tab$Brand =='Optima' & price.tab$Rating %in% c(1,5)]
price.OR.stars <- temp[1]*temp[4]/(temp[2]*temp[3])


##Analysis for "Great Price"
gp.tab <- sqldf('select Brand, Rating, "great.price", count("great.price")
			as N from "key.comments" group by Brand, Rating, 
			"great.price"')
gp.tab$Rating2 <- as.factor(gp.tab$Rating)
gp.fit <- glm(N ~ Brand + Rating2 + great.price + Brand:Rating + 
			Rating:great.price + Brand:great.price, data=gp.tab, 
			family=poisson())
summary(gp.fit)
Anova(gp.fit)
gp.fit2 <- glm(N ~ Brand + Rating2 + great.price + Brand:Rating + 
			Rating:great.price, data=gp.tab, family=poisson())
summary(gp.fit2)
Anova(gp.fit2)
pchisq(gp.fit2$deviance, df=gp.fit2$df.residual, lower.tail=FALSE)
anova(gp.fit2, gp.fit)
table(key.comments$great.price, key.comments$Rating)


##Analysis for "Fit Perfect"
fit.tab <- sqldf('select Brand, Rating, "fit.perfect", count("fit.perfect")
			as N from "key.comments" group by Brand, Rating, 
			"fit.perfect"')
fit.tab$Rating2 <- as.factor(fit.tab$Rating)
fit.fit <- glm(N ~ Brand + Rating2 + fit.perfect + Brand:Rating + 
			Rating:fit.perfect + Brand:fit.perfect, data=fit.tab, 
			family=poisson())
summary(fit.fit)
Anova(fit.fit)
fit.fit2 <- glm(N ~ Brand + Rating2 + fit.perfect + Brand:Rating + 
			Brand:fit.perfect, data=fit.tab, family=poisson())
summary(fit.fit2)
Anova(fit.fit2)
pchisq(fit.fit2$deviance, df=fit.fit2$df.residual, lower.tail=FALSE)
anova(fit.fit2, fit.fit)


##Analysis for "Dead"
dead.tab <- sqldf('select Brand, Rating, dead, count(dead) as N 
			from "key.comments" group by Brand, Rating, dead')
dead.tab$Rating2 <- as.factor(dead.tab$Rating)
dead.fit <- glm(N ~ Brand + Rating2 + dead + Brand:Rating + Rating:dead + 
			Brand:dead, data=dead.tab, family=poisson())
summary(dead.fit)
Anova(dead.fit)
dead.fit2 <- glm(N ~ Brand + Rating2 + dead + Brand:Rating + Rating:dead, 
			data=dead.tab, family=poisson())
summary(dead.fit2)
Anova(dead.fit2)
pchisq(dead.fit2$deviance, df=dead.fit2$df.residual, lower.tail=FALSE)
anova(dead.fit2, dead.fit)
table(key.comments$dead, key.comments$Rating)
dead.tab$fits <- dead.fit2$fitted.values

#Odds of dead = 1 in 1 stars vs 5 star
temp <- dead.tab$fits[dead.tab$Brand=='Optima' & dead.tab$Rating %in% c(1,5)]
dead.OR.stars <- temp[2]*temp[3]/(temp[1]*temp[4])

##Analysis for "Warranty"
warr.tab <- sqldf('select Brand, Rating, warranty, count(warranty) as N 
			from "key.comments" group by Brand, Rating, warranty')
warr.tab$Rating2 <- as.factor(warr.tab$Rating)
warr.fit <- glm(N ~ Brand + Rating2 + warranty + Brand:Rating + Rating:warranty + 
			Brand:warranty, data=warr.tab, family=poisson())
summary(warr.fit)
Anova(warr.fit)
warr.fit2 <- glm(N ~ Brand + Rating2 + warranty + Brand:Rating + 
			Rating:warranty, data=warr.tab, family=poisson())
summary(warr.fit2)
Anova(warr.fit2)
pchisq(warr.fit2$deviance, df=warr.fit2$df.residual, lower.tail=FALSE)
anova(warr.fit2, warr.fit)
table(key.comments$warranty, key.comments$Rating)
warr.tab$fits <- warr.fit2$fitted.values

#Odds of warranty = 1 in 1 stars vs 5 star
temp <- warr.tab$fits[warr.tab$Brand=='Optima' & warr.tab$Rating %in% c(1,5)]
warr.OR.stars <- temp[2]*temp[3]/(temp[1]*temp[4])


##Analysis of "Prius" and "Perfect Fit"
priusfit.tab <- sqldf('select Brand, prius, "fit.perfect", count("fit.perfect")
			as N from "key.comments" group by Brand, prius, 
			"fit.perfect"')
priusfit.fit <- glm(N ~ Brand + prius + fit.perfect + Brand:prius + 
			prius:fit.perfect + Brand:fit.perfect, data=priusfit.tab, 
			family=poisson())
summary(priusfit.fit)
Anova(priusfit.fit)
priusfit.fit2 <- glm(N ~ Brand + prius + fit.perfect + Brand:prius + 
			prius:fit.perfect, data=priusfit.tab, 
			family=poisson())
summary(priusfit.fit2)
Anova(priusfit.fit2)
pchisq(priusfit.fit2$deviance, df=priusfit.fit2$df.residual, 
		lower.tail=FALSE)

#Make data frame with dates of reviews with key terms and the associated key term
for (i in 1:length(key.words)){
	temp <- key.comments[key.comments[,names(key.comments)==
			key.words[i]]==1, c("Rating","Date2")]
	temp$Term <- key.words[i]
	if (i==1){
		test <- temp
	} else {
		test <- rbind(test, temp)
	}
}


#Plot review frequencies containing key terms over time
term.density <- ggplot(test, aes(x=Date2, group=Term)) + 
		geom_density(kernel="gaussian", aes(y=..count..,
		color=Term),size=1) + 
		theme_bw() + labs(x="Date", y="Density") + 
		theme(legend.position=c(0.12,0.8))
