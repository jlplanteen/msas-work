##############################################################
### AMAZON WEB SCRAPING CODE
##############################################################

# Code to extract product info and customer review details for three brands of lead-acid batteries.  Search result pages for each
# battery type supplied, then custom functions complete scraping to gather list of individual product types, product details, and 
# customer review details.


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Summary of web-scraping functions:
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## getURLs(searchURL, startInd): takes in search results list (for particular battery brand) and returns array of links 
##          to individual product pages.

## productDetails(URL): takes in URL for individual product page and returns key product info, including price, sales rank,
##          quantity of reviews, average review rating, URL for complete customer reviews, and time stamp of scraping

## grabComments(commentsURL): takes in URL of complete customer reviews and returns dataframe with key info from each review

##      commentDetails(HTMLguts): helper function to grabComments.  Takes in raw HTML of individual page of customer review and 
##            pulls key info for each review: ID, title, author, date, text, rating, verified purchase, and number of people who 
##            voted helpful
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Scraping Functions
##############################################################

##Function to scrape product listing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getURLs <- function(searchURL, start=1){
#Gather URLs for individual products by web scraping search results

	urlcontents <- readLines(searchURL)

	#Initialize links vector (24 links per page)
	links <- rep("", times=24)

	#Iterate through each search result
	for (i in start:(start+23)){
		#Find line with i-th search result
		keyline <- grep(paste('result', i, sep='_'),urlcontents)

		#Determine if i-th search result was found
		if (length(keyline)!=0){
			#Extract relevant line
			line <- urlcontents[keyline[1]]
			#Split before and after links "href=" 
			templinks <- strsplit(line, 'href="')[[1]]
			#Determine which segment contains "result#" & grab previous
			reslink <- grep(paste('result', i, sep='_'),templinks)
			links[i+1-start] <- strsplit(templinks[reslink+1], 'ref=')[[1]][1]
		}	
	}
	links <- links[links!=""]

	#Find next page
	nextpage1 <- grep('title="Next Page"', urlcontents)
	#Does next page exist?
	if (length(nextpage1)==0){
		return(links)
	} else {
		nextarea <- urlcontents[nextpage1:(nextpage1+10)]
		nextpage2 <- grep('href=', nextarea)
		p2.address <- nextarea[nextpage2]
		#Clean up and format address
		p2.address <- gsub(" ", "", p2.address) #remove spaces
		p2.address <- gsub("&amp;", "&", p2.address) #replace ampersands
		p2.address <- gsub('href="', "", p2.address) #remove beginning
		p2.address <- gsub('">', "", p2.address) #remove end
		nextlink <- paste('https://www.amazon.com', p2.address, sep="")
		return(c(links, getURLs(nextlink, start=i+1)))
	}
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##Function to scrape product details
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
productDetails <- function(URL){
#Scrape product name, price, rating, ranking, and customer review info from URL

	urlcontents <- readLines(URL, warn=FALSE)
	visit.time <- Sys.time()
	 
	#Find title
		nameline <- grep('meta name="title"', urlcontents)
		lines <- strsplit(urlcontents[nameline], ":")[[1]]
		Name <- lines[2]

	#Find price
		priceline <- grep('"priceblock_ourprice"', urlcontents)
		if (length(priceline)==0){
			Price <- NA
		} else {
			lines <- strsplit(urlcontents[priceline], ">")[[1]]
			Price <- as.numeric(gsub("[^0-9.]", "", lines[2]))
		}

	#Average rating
		ratinglines <- urlcontents[grep('out of 5 stars', urlcontents)]
		ratingline <- grep('Histogram', ratinglines)
		if (length(ratingline)==0){
			Rating <- NA
		} else {
			lines <- strsplit(ratinglines[ratingline[1]], "out of")[[1]]
			Rating <- as.numeric(gsub("[^0-9.]", "", lines[1]))
		}


	#Ranking
		bestline <- grep('Best Sellers Rank', urlcontents)
		if (length(bestline)==0){
			RankAuto <- NA
			RankBatteries <- NA
		} else {
			bestlines <- urlcontents[bestline:(bestline+15)]
			ranklines <- bestlines[grep('#', bestlines)]
			autoline <- ranklines[grep('See top 100', ranklines)]
			if (length(autoline) == 0){
				RankAuto <- NA
			} else {
				lines <- strsplit(autoline, "in")[[1]]
				RankAuto <- as.numeric(gsub("[^0-9]", "", lines[1]))
			}
			battline <- ranklines[grep('Replacement Parts', ranklines)]
			lines <- strsplit(battline, "in")[[1]]
			RankBatteries <- as.numeric(gsub("[^0-9]", "", lines[1]))
		}

	#Customer Reviews
		if (length(grep("Be the first to review this item", urlcontents))>0){
			ReviewLink <- ""
			ReviewQty <- 0
		} else {
			reviewline <- grep("see_all_summary", urlcontents)
			lines <- strsplit(urlcontents[reviewline],'"')[[1]]
			ReviewLink <- lines[4]
			ReviewQty <- as.numeric(gsub("[^0-9]", "", lines[5]))
			if (is.na(ReviewQty)){
				if (length(grep("both", lines[5]))>0){
					ReviewQty <- 2
				}else if (length(grep("See the customer review", lines[5]))>0){
					ReviewQty <- 1
				}
			}
		}
	
	#Combine into data frame and return
		product <- data.frame(URL, Name, Price, Rating, RankAuto, RankBatteries, 
			ReviewLink, ReviewQty, visit.time)
		return(product)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##Function to scrape product reviews
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grabComments <- function(commentsURL){
#Grab all comments for a given product
	guts <- readLines(commentsURL, warn=FALSE)

	#Find next page of comments
	next.inds <- grep(">Next<", guts)
	if (length(next.inds)==0){
		return(commentDetails(guts))		
	} else {
		next.line <- guts[next.inds]
		link.pages <- strsplit(next.line, '<li')[[1]]
		next.link <- link.pages[grep(">Next<", link.pages)]
		if (length(grep("a-disabled", next.link))>0){
			return(commentDetails(guts))
		} else {
			mywait()
			temp <- strsplit(next.link, '">')[[1]]
			temp <- temp[grep('href="', temp)][1]
			next.URL <- paste("https://amazon.com", gsub('<a href="', "", temp), sep="")
			return(rbind(commentDetails(guts), grabComments(next.URL)))
		}
	}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##Helper function to scrape review details
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
commentDetails <- function(URLcontents){
#Get comment details such as ID, title, author, date, text, rating, verified
#purchase, and number of people who voted helpful

	commentlines <- grep("review-title", URLcontents)
	top.review <- grep('>Top positive review<', URLcontents)
	critical.review <- grep('>Top critical review<', URLcontents)
	commentlines <- commentlines[!(commentlines %in% c(top.review, 
				critical.review))]
	temp <- rep(NA, length(commentlines))

	comments <- data.frame(ID=temp, Title=temp, Author=temp, Date=temp,
			Rating=temp, Text=temp, Verified=temp, Votes=temp)

	for (i in 1:length(commentlines)){
		templine <- paste(URLcontents[commentlines[i]],
				URLcontents[commentlines[i]+1], 
				URLcontents[commentlines[i]+2], sep="")
		templine <- gsub("<br />","",templine)
		segments <- strsplit(templine, "><")[[1]]

		#Review ID
			pos.segs <- segments[grep("div id=", segments)]
			ID.seg <- pos.segs[grep("a-section review", pos.segs)]
			temp <- strsplit(ID.seg, '=\"')[[1]][2]
			temp <- gsub('\" ', "", temp)
			comments[i,"ID"] <- gsub("class", "", temp)

		#Review title
			title.seg <- segments[grep("review-title", segments)]
			temp <- strsplit(title.seg, ">")[[1]][2]
			comments[i,"Title"] <- gsub("</a", "", temp)

		#Review author
			author.seg <- segments[grep("author", segments)]
			temp <- strsplit(author.seg, '">')[[1]][2]
			comments[i, "Author"] <- gsub("</a", "", temp)

		#Get date
			date.seg <- segments[grep("review-date", segments)]
			temp <- strsplit(date.seg, ">on ")[[1]][2]
			comments[i,"Date"] <- gsub("</span","",temp)

		#Get rating
			rating.seg <- segments[grep("out of 5 stars", segments)]
			temp <- strsplit(rating.seg, "out of")[[1]][1]
			comments[i,"Rating"] <- as.numeric(gsub("[^0-9.]", "", temp))

		#Main text
			text.seg <- segments[grep("review-text", segments)]
			temp <- strsplit(text.seg, 'review-text\">')[[1]][2]
			comments[i,"Text"] <- gsub("</span","",temp)

		#Verified Purchase
			verify <- grep("Verified Purchase", segments)
			if (length(verify)>0){
				comments[i,"Verified"] <- 1
			} else {
				comments[i,"Verified"] <- 0
			}

		#Found helpful
			vote.loc <- grep("review.votes", segments)
			if (length(vote.loc)>0){
				vote.seg <- segments[vote.loc]
				temp <- strsplit(vote.seg, 'review-votes\">')[[1]][2]
				if (length(grep("One", temp))>0){
					comments[i,"Votes"] <- 1
				} else {
					comments[i,"Votes"] <- as.numeric(gsub("[^0-9]", "", temp))	
				}
			} else {
				vote.lines <- grep("review.votes", URLcontents)
				not.used <- vote.lines[!(vote.lines %in% c(commentlines, top.review, 
						critical.review))]
				if (i == nrow(comments)){
					vote.line <- not.used[not.used > commentlines[i]]
				} else {
					vote.line <- not.used[not.used > commentlines[i] & not.used 
							< commentlines[i+1]]
				}		
				if (length(vote.line==1)){
					templine <- URLcontents[vote.line]
					templine <- gsub("<br />","",templine)
					segments <- strsplit(templine, "><")[[1]]
					vote.seg <- segments[grep("review.votes", segments)]
					temp <- strsplit(vote.seg, 'review-votes\">')[[1]][2]
					if (length(grep("One", temp))>0){
						comments[i,"Votes"] <- 1
					} else {
						comments[i,"Votes"] <- as.numeric(gsub("[^0-9]", "", temp))	
					}
				} else {
					comments[i, "Votes"] <- 0
				}
		
			}
	}
	return(comments)
}	

##############################################################



## Scrape product list
##############################################################

##URLs to brand specific product listings
optima <- "https://www.amazon.com/s/ref=lp_15719921_nr_p_89_2?fst=as%3Aoff&rh=n%3A15684181%2Cn%3A!15690151%2Cn%3A15719731%2Cn%3A15719911%2Cn%3A15719921%2Cp_89%3AOptima&bbn=15719921&ie=UTF8&qid=1465767410&rnid=2528832011"
odyssey <- "https://www.amazon.com/s/ref=sr_in_-2_p_89_34?fst=as%3Aoff&rh=n%3A15684181%2Cn%3A!15690151%2Cn%3A15719731%2Cn%3A15719911%2Cn%3A15719921%2Cp_89%3AOdyssey&bbn=15719921&ie=UTF8&qid=1465767448&rnid=2528832011"
exide <- "https://www.amazon.com/s/ref=sr_in_-2_p_89_21?fst=as%3Aoff&rh=n%3A15684181%2Cn%3A!15690151%2Cn%3A15719731%2Cn%3A15719911%2Cn%3A15719921%2Cp_89%3AExide&bbn=15719921&ie=UTF8&qid=1465767484&rnid=2528832011"

##Get individual product URLs for each brand
optima.links <- getURLs(optima)
odyssey.links <- getURLs(odyssey)
exide.links <- getURLs(exide)

##Concatenate all three groups of URLs into one vector
battery.links <- c(optima.links, odyssey.links, exide.links)

#Remove items that are not batteries
battery.links <- battery.links[-grep("Maintainer", battery.links)]
battery.links <- battery.links[-grep("Protectors", battery.links)]
battery.links <- battery.links[-grep("HK", battery.links)]
battery.links <- battery.links[-grep("Gauge", battery.links)]
battery.links <- battery.links[-grep("EHM327BK", battery.links)]

#Save battery URL vector
save(battery.links, file="links.RData")


## Scrape details for each product
##############################################################

##Randmize order of battery URLs (since scraping done multiple times, want randomized order)
temp.links <- battery.links[sample(1:length(battery.links), length(battery.links))]

#Iterate through each battery URL and extract details
for (i in 1:length(temp.links)){
	temp <- productDetails(temp.links[i])
	if (i == 1){
		battery.details <- temp
	} else {
		battery.details <- rbind(battery.details, temp)
	}
}

#Eliminate batteries not currently being sold
battery.details <- battery.details[!is.na(battery.details$Price), ]

#Eliminate batteries which have no sales rank
battery.details <- battery.details[!is.na(battery.details$RankAuto), ]

#Save results
save(battery.details, file="batteryDetails.RData")


## Scrape customer reviews for each battery
##############################################################

#Extract only batteries that have product reviews
comment.types <- battery.details[battery.details$ReviewQty > 0, ]

#Determine total number of reviews to be extracted
tot.comments <- sum(comment.types$ReviewQty)

#Convert factor classes to strings
comment.types$URL <- as.character(comment.types$URL)
comment.types$Name <- as.character(comment.types$Name)

#Initiate blank data frame
temp <- rep(NA, tot.comments)
all.comments <- data.frame(Prod.URL=temp, Prod.Name=temp, ID=temp, Title=temp,
			Author=temp, Date=temp, Rating=temp, Text=temp, Verified=temp,
			Votes=temp)

#Iterate through each battery type's comments
k <- 1
for (i in 1:nrow(comment.types)){
	#Extract all comments for a given battery
	temp.comments <- grabComments(as.character(comment.types[i, 
			"ReviewLink"]))
	#Determine number of comments extracted and properly advance index
	#for comments data frame
	N <- nrow(temp.comments)
	all.comments[k:(k+N-1), 1] <- as.character(comment.types[i, 1])
	all.comments[k:(k+N-1), 2] <- as.character(comment.types[i, 2])
	all.comments[k:(k+N-1), 3:10] <- temp.comments
	k <- k+N
}

#Save product reviews
save(all.comments, file="Comments.RData")


