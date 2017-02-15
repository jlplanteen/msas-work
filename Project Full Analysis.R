#################################################################
##   FULL TRIAL OF CODE ON TOY DATA
#################################################################
##  STAT 7900 - Machine Learning - Summer 2016
##  Final Project: Dimension Reduction to 3D Space
##  Jacey Planteen; last modified: 7/26/16
#################################################################

setwd("C:/Users/Jaceybot/Documents/STAT 7900 - Machine Learning/Project")

## READ IN TOY DATA SETS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Can data set
can.data <- read.csv("can_orig_2clustered.csv", header=FALSE)

##Ecoli data set
ecoli.data <- read.csv("ecoli_orig_clustered.csv", header=FALSE)

##Iono data set
iono.data <- read.csv("iono_2.csv", header=FALSE)

##KDD data set
KDD.data <- read.csv("kdd-after_normalization.csv", header=TRUE)

##Shuttle data set
shuttle.data <- read.csv("shuttle4_orig_clustered.csv", header=FALSE)

##Wine data set
wine.data <- read.csv("wine_clustered_3.csv", header=FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## REDUCE TO 3 DIMENSIONS - SCALE INPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(236834267)
system.time(can.scaled <- reduce.3D(can.data, scale=TRUE))
system.time(ecoli.scaled <- reduce.3D(ecoli.data, scale=TRUE))
system.time(iono.scaled <- reduce.3D(iono.data, scale=TRUE))
system.time(KDD.scaled <- reduce.3D(KDD.data, scale=TRUE))
system.time(shuttle.scaled <- reduce.3D(shuttle.data, scale=TRUE))
system.time(wine.scaled <- reduce.3D(wine.data, scale=TRUE))

save(can.scaled, ecoli.scaled, iono.scaled, KDD.scaled, 
		shuttle.scaled, wine.scaled, file="Scaled.RData")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## REDUCE TO 3 DIMENSIONS - DO NOT SCALE INPUT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(1192386)
system.time(can.noscale <- reduce.3D(can.data, scale=FALSE))
system.time(ecoli.noscale <- reduce.3D(ecoli.data, scale=FALSE))
system.time(iono.noscale <- reduce.3D(iono.data, scale=FALSE))
system.time(KDD.noscale <- reduce.3D(KDD.data, scale=FALSE))
system.time(shuttle.noscale <- reduce.3D(shuttle.data, scale=FALSE))
system.time(wine.noscale <- reduce.3D(wine.data, scale=FALSE))

save(can.noscale, ecoli.noscale, iono.noscale, KDD.noscale, 
		shuttle.noscale, wine.noscale, file="Unscaled.RData")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Read in saved data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("scaled.Rdata")
load("unscaled.Rdata")

## Compare and visualize results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Combine all the visualizability
scaled <- list(can=can.scaled, ecoli=ecoli.scaled, iono=iono.scaled,
		KDD=KDD.scaled, shuttle=shuttle.scaled, wine=wine.scaled)
no.scale <- list(can=can.noscale, ecoli=ecoli.noscale, iono=iono.noscale,
		KDD=KDD.noscale, shuttle=shuttle.noscale, wine=wine.noscale)
temp <- rep(NA, length(scaled)*2)
vis.results <- data.frame(Dataset=temp, Scaling=temp, PCA=temp, Gauss.KPCA=temp,
			Gauss.KECA=temp, Poly.KPCA=temp, Poly.KECA=temp)
for (i in 1:length(scaled)){
	vis.results$Dataset[i] <- names(scaled)[i]
	vis.results$Scaling[i] <- "Yes"
	vis.results[i, 3:7] <- scaled[[i]][[2]]
}
for (i in 1:length(scaled)){
	vis.results$Dataset[i+length(scaled)] <- names(no.scale)[i]
	vis.results$Scaling[i+length(scaled)] <- "No"
	vis.results[i+length(scaled), 3:7] <- no.scale[[i]][[2]]
}


#Plot maximum visualizability for scaled and unscaled input
vis.results$MaxVis <- apply(vis.results[,3:7],1,max)
ggplot(vis.results) + aes(x=Dataset,y=MaxVis, group=Scaling, 
	colour=Scaling) + geom_line(size=1) + theme_gray() + scale_y_continuous(
	limits=c(min(0, vis.results$MaxVis),1))+ labs(y=
	"Visualizability in 3D", x= "Data Set", title=
	"Maximum Visualizability for Scaled and Unscaled Input") +
	theme(legend.key=element_blank())

#Plot maximum visualizability for Gaussian KPCA / KECA - unscaled input
gauss.long <- melt(vis.results, id = c("Dataset", "Scaling"), measure = 
		c("Gauss.KPCA", "Gauss.KECA"))
ggplot(gauss.long[gauss.long$Scaling=="No",]) + aes(x=Dataset,y=value, 
	group=variable, colour=variable) + geom_line(size=1) + theme_gray() +
	scale_y_continuous(limits=c(0,1)) + labs(y = "Visualizability in 3D",
	x = "Data Set", title = 
	"Max Visualizability for Unscaled KECA & KPCA with Gaussian Kernel") + 
	theme(legend.key=element_blank(), legend.title=element_blank())

#Plot maximum visualizability for Polynomial KPCA / KECA - unscaled input
poly.long <- melt(vis.results, id = c("Dataset", "Scaling"), measure = 
		c("Poly.KPCA", "Poly.KECA"))
ggplot(poly.long[poly.long$Scaling=="No",]) + aes(x=Dataset,y=value, 
	group=variable, colour=variable) + geom_line(size=1) + theme_gray() +
		scale_y_continuous(limits=c(0,1)) + labs(y = "Visualizability in 3D",
	x = "Data Set", title = 
	"Max Visualizability for Unscaled KECA & KPCA with Polynomial Kernel") + 
	theme(legend.key=element_blank(), legend.title=element_blank())

#Plot visualizability from each method for unscaled data
method.long <- melt(vis.results, id = c("Dataset", "Scaling"), measure = 
		c("PCA", "Gauss.KPCA", "Gauss.KECA", "Poly.KPCA", "Poly.KECA"))
ggplot(method.long[method.long$Scaling=="No",]) + aes(x=Dataset,y=value, 
	group=variable, colour=variable) + geom_line(size=1) + theme_gray() +
		scale_y_continuous(limits=c(0.75,1)) + labs(y = "Visualizability in 3D",
	x = "Data Set", title = 
	"Visualizability in 3 Dimensions for Each Method with Unscaled Input") + 
	theme(legend.key=element_blank(), legend.title=element_blank())
