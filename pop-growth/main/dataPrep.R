#### RAW DATA PROCESSING

## Read in raw census data files from 2000 and 2010 as well as geographic and demographic details.
## Combine data into one data frame with one county per observation.
## Fix data issues, such as geographic boundaries which have changed over the 10 years.  

## Split data into test and training data sets using duplex algorithm.

require(plyr); require(prospectr)

###Read in raw csv files
DP1.2000 <- read.csv("DEC_00_SF1_DP1.csv")
DP2.2000 <- read.csv("DEC_00_SF3_DP2.csv")
DP3.2000 <- read.csv("DEC_00_SF3_DP3.csv")
DP1.2010 <- read.csv("DEC_10_SF1_SF1DP1.csv")
G001 <- read.csv("DEC_10_DP_G001.csv")

#Define the variables to keep in each file
DP1.00.keep <- c("GEO.id2", "HC01_VC01", "HC01_VC03", "HC01_VC18", 
			"HC02_VC05", "HC02_VC19", "HC02_VC24", "HC01_VC29", 
			"HC01_VC30", "HC01_VC31", "HC01_VC32", "HC01_VC40", 
			"HC01_VC56", "HC02_VC79", "HC01_VC90", "HC01_VC91", 
			"HC02_VC101")

DP2.00.keep <- c("GEO.id2", "HC01_VC02", "HC01_VC07", "HC01_VC17",
			"HC01_VC18", "HC02_VC21", "HC02_VC55")

DP3.00.keep <- c("GEO.id2", "HC02_VC03", "HC02_VC06", "HC01_VC08", 
			"HC01_VC24", "HC02_VC34","HC02_VC35", "HC02_VC36",
			"HC02_VC37", "HC02_VC38", "HC02_VC39", "HC02_VC40",
			"HC02_VC41", "HC02_VC42", "HC02_VC43", "HC02_VC44",
			"HC02_VC46","HC02_VC93", "HC02_VC105")

DP1.10.keep <- c("GEO.id2", "HD01_S001")

G001.keep <- c("GEO.id2", "GEO.display.label", "VD010", "VD011",
			"VD012", "VD013", "VD067", "VD102")

##Reduce data frames to only desired variables
DP1.2000 <- DP1.2000[ ,DP1.00.keep]
DP2.2000 <- DP2.2000[ ,DP2.00.keep]
DP3.2000 <- DP3.2000[ ,DP3.00.keep]
DP1.2010 <- DP1.2010[ ,DP1.10.keep]
G001 <- G001[ , G001.keep]

#Define new (descriptive) variable names
names(DP1.2000) <- c("ID1", "Total.2000", "Male","Median.Age","Age.Under5", 
			"Age.18.Plus", "Age.65.Plus", "Race.White", "Race.AfrAmer",
			"Race.AmerIndian", "Race.Asian", "Race.PacIslander", 
			"Hispanic",	"Families", "Household.Size", "Family.Size",
			"Own.Housing")
names(DP2.2000) <- c("ID1", "School", "College", "HighSchool.Grad",
			"HigherEduc.Grad", "Never.Married", "NativeBorn")
names(DP3.2000) <- c("ID1", "LaborForce", "Unemployed", "Armed.Forces", 
			"Median.Income", "Agri", "Construction", "Mfg", "Wholesale",
			"Retail", "Trans", "Info", "Finance", "Professional", "Educ",
			"Arts", "PublicAdmin", "Poverty.Family", "Poverty.Indiv")
names(DP1.2010) <- c("ID1", "Total.2010")
names(G001) <- c("ID1", "County.Name", "Region", "Division", "FIPS.State",
		"FIPS.County", "Land.Area","Metro.Micro")

#Merge the separate data frames into one main data frame
temp1 <- merge(G001, DP1.2010, by="ID1", all=TRUE)
temp2 <- merge(temp1, DP1.2000, by="ID1", all=TRUE)
temp3 <- merge(temp2, DP2.2000, by="ID1", all=TRUE)
pop.data <- merge(temp3, DP3.2000, by="ID1", all=TRUE)


##Fix issues in pop.2010 data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
#Find population totals with annotation - will have an "r" character after population total
pop.data$Total.2010 <- as.character(pop.data$Total.2010)
temp <- as.numeric(gregexpr("r", pop.data$Total.2010))
#Remove annotation
pop.data$Total.2010[temp>0&!is.na(temp)] <- substr(pop.data$Total.2010[temp>0&
	!is.na(temp)], 1, temp[temp>0  & !is.na(temp)]-2)

##Convert classes for variables as necessary
lapply(pop.data, class)
pop.data$Total.2010 <- as.numeric(pop.data$Total.2010)
pop.data$Region <- as.factor(pop.data$Region)
pop.data$Metro.Micro <- as.factor(pop.data$Metro.Micro)
pop.data$FIPS.State <- as.character(pop.data$FIPS.State)
pop.data$FIPS.County <- as.character(pop.data$FIPS.County)

##Fix special cases of boundary changes
	#Broomfield County, CO (ID1=8014)split from Boulder County in 2001 
	#(ID1=8013).  Combine 2010 population total with Boulder County total
	pop.data$Total.2010[pop.data$ID1==8013] <- pop.data$Total.2010[pop.data$ID1
			==8013] + pop.data$Total.2010[pop.data$ID1==8014]
	pop.data <- pop.data[pop.data$ID1!=8014,]

	#Clifton Forge City, VA - independent city in 2000, part of 
	#Alleghany County in 2010.  Combine 2000 city data into Alleghany Co.
	pop.data[pop.data$ID1==51005, c(12:15,22:25,28:33,35:49)] <- (
			pop.data$Total.2000[pop.data$ID1==51005]*
			pop.data[pop.data$ID1==51005, c(12:15,22:25,28:33,35:49)] + 
			pop.data$Total.2000[pop.data$ID1==51560]*
			pop.data[pop.data$ID1==51560, c(12:15,22:25,28:33,35:49)])/
			(pop.data$Total.2000[pop.data$ID1==51005] + 
			pop.data$Total.2000[pop.data$ID1==51560])
	pop.data[pop.data$ID1==51005, c(10,11,16:21,26,27,34)] <-
			pop.data[pop.data$ID1==51005, c(10,11,16:21,26,27,34)] + 
			pop.data[pop.data$ID1==51560, c(10,11,16:21,26,27,34)]
	pop.data <- pop.data[pop.data$ID1!=51560,]



#Fix FIPS codes
pop.data$FIPS.State[nchar(pop.data$FIPS.State)==1] <- paste("0", 
	pop.data$FIPS.State[nchar(pop.data$FIPS.State)==1], sep="")
pop.data$FIPS.County[nchar(pop.data$FIPS.County)==1] <- paste("00", 
	pop.data$FIPS.County[nchar(pop.data$FIPS.County)==1], sep="")
pop.data$FIPS.County[nchar(pop.data$FIPS.County)==2] <- paste("0", 
	pop.data$FIPS.County[nchar(pop.data$FIPS.County)==2], sep="")

pop.data$FIPS <- paste(pop.data$FIPS.State, pop.data$FIPS.County)


##Rename factor levels to actual meaning
pop.data$Region <- mapvalues(pop.data$Region, c("1", "2", 
	"3", "4"), c("Northeast", "Midwest", "South", "West"))
pop.data$Division <- mapvalues(pop.data$Division, c("1", "2", 
	"3", "4", "5", "6", "7", "8", "9"), c("New England", 
	"Middle Atlantic", "East North Central", "West North Central",
	"South Atlantic", "East South Central", "West South Central", 
	"Mountain", "Pacific"))
pop.data$Metro.Micro <- mapvalues(pop.data$Metro.Micro, c("1", 
	"2", "9"), c("Metropolitan", "Micropolitan", "Neither"))


##Create new variables and modify some existing variables
#Ratio of 2010 to 2000 population
pop.data$Ratio <- pop.data$Total.2010/pop.data$Total.2000

#Ratio of 2000 to 2010 population 
pop.data$Ratio2 <- pop.data$Total.2000/pop.data$Total.2010

#Actual % change in population
pop.data$Change <- (pop.data$Total.2010 - pop.data$Total.2000)/
			pop.data$Total.2000

#Convert land area from square meters to square miles
pop.data$Land.Area <- pop.data$Land.Area/2589975

#Calculate population density
pop.data$Density <- pop.data$Total.2000/pop.data$Land.Area

#Calculate difference between difference between household/family sizes
pop.data$House.Family.Delta <- pop.data$Family.Size - 
			pop.data$Household.Size

#Calculate percent of population enrolled in college & school
pop.data$School <- (pop.data$School-pop.data$College)/
			pop.data$Total.2000
pop.data$College <- pop.data$College/pop.data$Total.2000

#Calculate percentages for variables which are totals
pop.data[, c(11,16:21,34)] <- pop.data[ ,c(11,16:21,34)]/pop.data$Total.2000

#Reduce other percents by 100 to make comparable
other.percs <- c(13:15,22,26:33,36:49)
pop.data[, other.percs] <- pop.data[ ,other.percs]/100

##Create alternate racial variables to reduce multi-collinearity issues
model.data <- pop.data[complete.cases(pop.data), ]
model.data$Race.AfrAmer2 <- 0
model.data$Race.AfrAmer2[model.data$Race.AfrAmer > 0] <- 
	model.data$Race.AfrAmer[model.data$Race.AfrAmer > 0]/
	(1-model.data$Race.White[model.data$Race.AfrAmer > 0])
row.names(model.data) <- 1:nrow(model.data)

##Split data into training and test sets
##Create design matrix to use in duplex function
fit.total <- lm(Ratio ~ Region + Metro.Micro + Male + Median.Age + 
	Age.Under5 + Age.18.Plus + Age.65.Plus + Race.White + Race.AfrAmer + Race.AmerIndian +
	Race.Asian + Race.PacIslander + Hispanic + Families + Household.Size + 
	Family.Size + Own.Housing + HighSchool.Grad + HigherEduc.Grad + Never.Married +
	NativeBorn + LaborForce + Unemployed + Armed.Forces + Median.Income + Agri + 
	Construction + Mfg + Wholesale + Retail + Trans + Info + Finance + Professional + 
	Educ + Arts + PublicAdmin + Poverty.Family + Poverty.Indiv + Density + College 
	+ School, data=model.data)
test.inds <- duplex(model.matrix(fit.total), 600)[[2]]

test.data <- model.data[test.inds, ]
train.data <- model.data[-test.inds, ]

save(model.data, test.data, train.data, file="PopData.RData")

