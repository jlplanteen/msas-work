# This code contains functions to complete feature reduction down to 3 features and visualize results using five different methods:
#	Principal components analysis, Gaussian kernel principal components analysis (KPCA), Polynomical KPCA,
#	Gaussian kernel entropy components analysis (KECA), and Polynomial KECA
# Input should be in a data frame and contain at least 3 features.  The final column in the data frame should contain original 
# cluster assigments (chosen via kmeans).
# User can choose to scale or not to scale input (scaling centers each feature to mean 0 and scales to a variance of 1).
# Functions tune kernel parameters using grid search type techniques.  Optimal parameters are chosen to maximize a metric
# called visualizability in 3D.  This metric is a measure of the entropy in new clusters assigned via kmeans using the reduced
# features vs. the original cluster assignments (same number of clusters used).  More agreement equates to lower entropy and higher
# visualizability.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created July, 2016 by J. Planteen for STAT 7900 - Machine Learning at Kennesaw State University
# Last modified February 15, 2016 by J. Planteen

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dimension reduction functions and descriptions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# reduce.3D(raw, scale=TRUE) >> master function for reduction to 3 dimensions
# 	fix.data(data.frame) >> takes raw data frame and standardizes formatting for further processing
#	PCA.3D(data, scale) >> completes principal components analysis to reduce to 3 dimensions
#	RBF.KPCA(data) >> completes Gaussian KPCA and KECA to reduce to 3 dimensions using optimized sigma
#		RBF.3D(data, sigma) >> reduces to 3 dimensions using KPCA / KECA and Gaussian kernel with given sigma
#		RBF.optimize(data) >> completes grid search to optimize sigma to maximize visualizability in 3D
#	Poly.KPCA(data) >> completes Polynomial KPCA and KECA to reduce to 3 dimensions using optimized degree, scale, and offset
#		Poly.3D(data, degree, scale, offset) >> 3D reduction using KPCA / KECA and poly kernel with given degree, scale, & offset
#		optimize.poly(data) >> completes grid search of various degrees and offsets to maximize visualizability in 3D
#	keca(kernel, data) >> complete KECA using a given kernel and data set and returns transformed data (full dimensionality)
#		Renyi(E.vecs, E.vals) >> calculates Renyi quadratic entropy of supplied eigenvectors and eigenvalues
#	visualize.3D(features, clusters) >> takes in reduced 3D data & original clusters.  Assigns new clusters and calcs visualizability
#		total.entropy(matrix) >> calculates total entropy for all clusters
#		individual.entropy(vector) >> calculates entropy of a single cluster

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Graphing / Visualization functions and descriptions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot.Gauss.opt(data) >> takes output from reduce.3D function and plots visualizability vs. sigma for KPCA/KECA using Gaussian kernel
# plot.Poly.opt(data) >> takes output from reduce.3D; plots visualizability vs. offset, grouped by degree for KPCA/KECA w/ poly kernel
# visualize.plot(data) >> takes output from reduce.3D; produces 5 (one for each redux method) 3D scatterplots (grouped by orig clusters)
#	make.3D.plot(data, method, method.name, vis) >> makes individual 3D scatterplot for given data and reduction method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Example using built-in iris data set:
# iris2 <- iris[,1:4]
# cluster0 <- kmeans(iris2, 3)
# iris2$Orig.Cluster <- cluster0[[1]]
# iris.3D <- reduce.3D(iris2)
# plot.Gauss.opt(iris.3D)
# plot.Poly.opt(iris.3D)
# visualize.plot(iris.3D)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(kernlab)
library(ggplot2); library(reshape2); library(cowplot); 
library(scatterplot3d)

## MASTER FUNCTION
#################################################################
reduce.3D <- function(raw.data, scale=TRUE){
# reduce.3D(raw.data, scale=TRUE)
#	raw.data -> data frame with original cluster assignments in last column and at least 3 dimensions
#			in remaining columns (and possibly an ID column which will be excluded)
#	scale -> Boolean value indicating whether each variable should be scaled to mean 0 and variance 1.  Default is to scale
# Dimension reduction (to three dimensions) is completed with 5 methods: principal components analysis (PCA),  
# kernel principal components analysis (KPCA) with Gaussian kernel, KPCA with polynomial kernel, kernel entropy components analysis 
# (KECA) with Gaussian kernel, and KECA with polynomial kernel.
# Output: list with five components
#	result: data frame with the optimal 3 dimensions from each method
#	visualizability: a list of the visualizability obtained for each of the five methods (with optimal kernel tuning)
#	KECA.vec: two vectors indicating the Eigen vectors used from the kernel matrix for the two optimal KECA reductions
#	kernel.params: a list of the optimal kernel parameters used for the four kernel methods	
#	optimization: a list containing data frames of the parameters used to tune the kernels and the resulting visualizability for each

	data <- fix.data(raw.data) #Standardize data format
	
	#Principal components analysis
	data.PCA <- PCA.3D(data, scale=scale)

	#Scale inputs (as needed)
	if (scale) {
	data[,1:(ncol(data)-1)] <- scale(data[,1:(ncol(data)-1)], center = 
			FALSE, scale = apply(data[,1:(ncol(data)-1)], 2, sd, 
			na.rm = TRUE))}

	#Kernel components analysis
	data.RBF <- RBF.KPCA(data)
	data.poly <- Poly.KPCA(data)

	#Combine results and calculate visualizability in 3D
	final.3D <- cbind(data.PCA[,1:3], data.RBF$result[,1:6],
				data.poly$result)
	vis.PCA <- visualize.3D(final.3D[,1:3], final.3D[,16])
	vis.RBF.KPCA <- visualize.3D(final.3D[,4:6], final.3D[,16])
	vis.RBF.KECA <- visualize.3D(final.3D[,7:9], final.3D[,16])
	vis.Poly.KPCA <- visualize.3D(final.3D[,10:12], final.3D[,16])
	vis.Poly.KECA <- visualize.3D(final.3D[,13:15], final.3D[,16])
	
	return(list(result=final.3D, visualizability=list(PCA=vis.PCA,
		Gaussian.KPCA=vis.RBF.KPCA, Gaussian.KECA=vis.RBF.KECA,
		Polynomial.KPCA=vis.Poly.KPCA, Polynomial.KECA=vis.Poly.KECA),
		KECA.vec=list(Gaussian=data.RBF$KECA.vec, Polynomial=
		data.poly$KECA.vec), kernel.params=list(Gaussian=data.RBF$params,
		Polynomial=data.poly$params), optimization=list(Gaussian=
		data.RBF$optimization, Polynomial=data.poly$optimization)))
}


#Format incoming data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fix.data <- function(data.frame){
#Function to standardize naming in toy data sets & remove any constants
	#remove ID column, if exists
	data.frame <- data.frame[,!(names(data.frame) %in% c("id"))]
	#make last column cluster id	
	new.names <- c(names(data.frame)[1:(ncol(data.frame)-1)],
			"cluster")
	names(data.frame) <- new.names

	#find any constant features and remove
	vars <- apply(data.frame, 2, var)
	data.frame <- data.frame[,vars!=0]
	return(data.frame)
}


# Regular Principal Components Analysis (PCA)
#################################################################

#Principal Components analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
PCA.3D <- function(raw, scale=TRUE){
##Function to take in a data frame of features and vector of clusters and return data frame reduced to 3 features by PCA and 
# cluster label
	features <- raw[,1:(ncol(raw)-1)]
	if (scale){
		full.PCA <- prcomp(~., data=features, scale.=TRUE)
	} else {
		full.PCA <- prcomp(~., data=features, scale.=FALSE)
	}
	data.3D <- data.frame(PCA.F1=full.PCA$x[,1], PCA.F2=full.PCA$x[,2],
				PCA.F3=full.PCA$x[,3], Cluster.Orig=raw$cluster)
	return(data.3D)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Gaussian Kernel Principal Components analysis
#################################################################

#Main function for Guassian KPCA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RBF.KPCA <- function(data){
#Return data frame with 3 principal components from Gaussian kernel PCA & kernel ECA & optimize kernel
	
	#Estimate optimal sigma for analysis
	sig.table <- RBF.optimize(data)
	#Optimal sigma for KPCA
	sig.KP.opt <- sig.table$sigma[sig.table$KPCA.vis == max(
				sig.table$KPCA.vis)]
	#If more than one "optimal" sigma...
	if (sigest(cluster~.,data=data)[2] %in% sig.KP.opt){
		sig.KP.opt <- sigest(cluster~.,data=data)[2]
	} else {
		sig.KP.opt <- max(sig.KP.opt)
	}
	
	#Optimal sigma for KECA
	sig.KE.opt <- sig.table$sigma[sig.table$KECA.vis == max(
				sig.table$KECA.vis)]
	#If more than one "optimal" sigma...
	if (sigest(cluster~.,data=data)[2] %in% sig.KE.opt){
		sig.KE.opt <- sigest(cluster~.,data=data)[2]
	} else {
		sig.KE.opt <- max(sig.KE.opt)
	}

	#Calculate and return final reduced dimensions
	KPCA.3D <- RBF.3D(data, sig.KP.opt)
	KECA.3D <- RBF.3D(data, sig.KE.opt)
	final.3D.DF <- cbind(KPCA.3D$x.prime[,1:3], KECA.3D$x.prime[,4:7])
	KECA.vec <- KECA.3D$KECA.vecs
	sigmas <- list(RBF.KPCA.sig=sig.KP.opt, RBF.KECA.sig=sig.KE.opt)
	return(list(result=final.3D.DF, KECA.vec=KECA.vec,
		params=sigmas, optimization=sig.table))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Gaussian Kernel Principal Components analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RBF.3D <- function(data, sigma.test){
#Return data frame with 3 principal components from Gaussian kernel PCA given a sigma value

	#Determine optima eigenvectors for KECA
	keca.out <- keca(rbfdot(sigma=sigma.test), as.matrix(data[,
			1:(ncol(data)-1)]))
	keca.x <- keca.out$PCs
	keca.order <- keca.out$order

	#Perform RBF KPCA & extract features for KPCA & KECA
	temp <- kpca(~., data=data[,1:(ncol(data)-1)], kernel="rbfdot",
			kpar=list(sigma=sigma.test), features=max(keca.order))
	PCs <- rotated(temp)
	data.3D <- data.frame(KPCA.RBF.F1=PCs[,1], KPCA.RBF.F2=PCs[,2],
				KPCA.RBF.F3=PCs[,3], KECA.RBF.F1=keca.x[,1], 
				KECA.RBF.F2=keca.x[,2],KECA.RBF.F3=keca.x[,3],
				Cluster.Orig=data$cluster)
	return(list(x.prime=data.3D, KECA.vecs=keca.order, sigma=sigma.test))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Optimization of Gaussian Kernel Parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RBF.optimize <- function(data){
#Optimize Gaussian kernel parameters for maximum visualizability
	
	#Estimate optimal sigma for analysis
	sigma.opt <- sigest(cluster~.,data=data)

	#Develop vector of sigmas to trial
	pt.1 <- exp(seq(log(sigma.opt[1])-2, log(sigma.opt[2]), length.out=11))
	pt.2 <- exp(seq(log(sigma.opt[2]), log(sigma.opt[3])+2, length.out=11))
	sigmas <- c(pt.1, pt.2[-1])

	#Initialize variables
	KPCA.vis <- rep(NA, length(sigmas))
	KECA.vis <- rep(NA, length(sigmas))
	
	for (i in 1:length(sigmas)){
		temp <- RBF.3D(data, sigmas[i])
		KPCA.vis[i] <- visualize.3D(temp$x.prime[,1:3], temp$x.prime[,7])
		KECA.vis[i] <- visualize.3D(temp$x.prime[,4:6], temp$x.prime[,7])
	}

	return(data.frame(sigma=sigmas, KPCA.vis=KPCA.vis, KECA.vis=KECA.vis))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Polynomial Kernel Principal Components analysis
#################################################################

#Main function for Polynomial KPCA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Poly.KPCA <- function(data){
#Return data frame with 3 principal components from Polynomial kernel PCA and optimize parameters
	
	#Estimate optimal kernel parameters
	params <- optimize.poly(data)
	#Find optimal parameters for KPCA
	opt.KP.row <- params[params$KPCA.vis == max(params$KPCA.vis),]
	opt.KP.row <- opt.KP.row[1,]

	#Optimal parameters for KECA
	opt.KE.row <- params[params$KECA.vis == max(params$KECA.vis),]
	opt.KE.row <- opt.KE.row[1,]

	#Determine final optimal eigenvectors for KECA
	KPCA.3D <- Poly.3D(data, degree=opt.KP.row$degree[1], scale=
				opt.KP.row$scale[1], offset=opt.KP.row$offset[1])
	KECA.3D <- Poly.3D(data, degree=opt.KE.row$degree[1], scale=
				opt.KE.row$scale[1], offset=opt.KE.row$offset[1])

	final.3D.DF <- cbind(KPCA.3D$x.prime[,1:3], KECA.3D$x.prime[,4:7])
	KECA.vec <- KECA.3D$KECA.vecs
	final.parms <- list(poly.KPCA=KPCA.3D$parms, poly.KECA=KECA.3D$parms)
	return(list(result=final.3D.DF, KECA.vec=KECA.vec,
		params=final.parms, optimization=params))

	return(list(result=final.3D, optimization=params))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Polynomial Kernel Principal Components analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Poly.3D <- function(data, degree.test, scale.test, offset.test){
#Return data frame with 3 principal components from Polynomial kernel PCA

	#Use scale if offset sufficiently large
	if (offset.test > 1) {
		scale.test <- scale.test/offset.test
		offset.test <- offset.test/offset.test
	}

	#Determine optimal eigenvectors for KECA
	keca.out <- keca(polydot(degree=degree.test, scale=scale.test,
				offset=offset.test), as.matrix(data[,
				1:(ncol(data)-1)]))
	keca.x <- keca.out$PCs
	keca.order <- keca.out$order

	#Perform Polynomial KPCA & extract features for KPCA & KECA
	temp <- kpca(~., data=data[,1:(ncol(data)-1)], kernel="polydot",
			kpar=list(degree=degree.test, scale=scale.test, 
			offset=offset.test), features=3)
	PCs <- rotated(temp)
	data.3D <- data.frame(KPCA.Poly.F1=PCs[,1], KPCA.Poly.F2=PCs[,2],
				KPCA.Poly.F3=PCs[,3], KECA.Poly.F1=keca.x[,1], 
				KECA.Poly.F2=keca.x[,2],KECA.Poly.F3=keca.x[,3], 
				Cluster.Orig=data$cluster)
	if (scale.test < 1) {
		offset.test <- offset.test/scale.test
		scale.test <- scale.test/scale.test
	}
	return(list(x.prime=data.3D, KECA.vecs=keca.order, parms=list(
			degree=degree.test, scale=scale.test, offset=offset.test)))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Optimize Polynomial Kernel Parameters
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
optimize.poly <- function(data){
#Optimize degree and offset for polynomial kernel
	
	#Define parameters to try
	degrees <- rep(1:5, each=30)
	offsets <- rep(c(0,10^seq(-2,6, length.out=29)), times=5)

	#Initialize parameters
	opt.df <- data.frame(degree=degrees, offset=offsets)
	opt.df$scale <- 1
	opt.df$KPCA.vis <- NA
	opt.df$KECA.vis <- NA

	#Perform grid search through parameters
	for (i in 1:nrow(opt.df)){
		temp <- Poly.3D(data, degree=opt.df$degree[i],	
					scale=opt.df$scale[i],
					offset=opt.df$offset[i])
		opt.df$KPCA.vis[i] <- visualize.3D(temp$x.prime[,1:3], 
						temp$x.prime[,7])
		opt.df$KECA.vis[i] <- visualize.3D(temp$x.prime[,4:6], 
						temp$x.prime[,7])
	}
	return(opt.df)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Kernel Entropy Components Analysis Functions
#################################################################

#Calculate KECA for a given data set and return transformed data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
keca <- function(kernel, data){
	K.matrix <- kernelMatrix(kernel, data)
	eig.K <- eigen(K.matrix)
	R.entropy <- Renyi(eig.K$vectors, eig.K$values)
	order.eca <- order(R.entropy, decreasing=TRUE)
	keca.order <- order.eca[1:3]

	#Transform data
	D <- diag(1/sqrt(eig.K$values[keca.order])) 
	E <- t(eig.K$vectors[,keca.order])
	return(list(PCs=t(D %*% E %*% K.matrix), order=keca.order))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Renyi entropy of Eigenvectors and Eigenvalues
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Renyi <- function(E.vecs, E.vals){

	N <- length(E.vals)
	entropies <- rep(NA, times=N)

	for (i in 1:N){
		c.val <- E.vals[i]
		c.vec <- E.vecs[,i]/sqrt(sum(E.vecs[,i]*E.vecs[,i]))
		entropies[i] <- c.val*sum(c.vec)^2
	}
	return(entropies)
}

#Visualizability in 3D
########################################################################

##Function to calculate visualizability in 3D space
##for dimension reduction tactics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
visualize.3D <- function(features, cluster.orig){
#Function takes in a data set with 3 attributes and an original cluster ID
#and returns a measure of the visualizability in 3-dimensional space
	N.clusters <- length(unique(cluster.orig))
	new <- kmeans(features, centers=N.clusters, algorithm="MacQueen",
			iter.max=100, nstart=500)
	c.table <- table(cluster.orig, new$cluster)
	vis.3D <- 1 - total.entropy(as.matrix(c.table))
	return(vis.3D)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
total.entropy <- function(Matrix){
##Helper function to calculate the total entropy for new cluster assignments (matrix input of contingency table:
##columns new cluster assignments, rows original cluster assigments
	cluster.sizes <- apply(Matrix,2,sum) #Size of each new cluster
	#Get entropy of each cluster
	cluster.entropy <- apply(Matrix,2,individual.entropy)
	entropy.clusters <- sum(cluster.sizes*cluster.entropy)/
					sum(cluster.sizes)
	return(entropy.clusters)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
individual.entropy <- function(vec){
##Helper function tocalculate the entropy for an individual cluster (in vector form)
	N.cluster <- sum(vec) #Total individuals in new cluster
	p.vec <- vec/N.cluster #proportion in each original cluster
	cell.entropy <- p.vec*log2(p.vec) #entropy for each individual cell
	cell.entropy[p.vec==0] <- 0 #In case perfect match, log(0) returns NaN
	cluster.entropy <- -sum(cell.entropy) #sum up cell entropies for total
	return(cluster.entropy)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

########################################################################


#Back End Functions to visualize results
########################################################################

#Gaussian Kernel Optimization Plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.Gauss.opt <- function(data){
#Plot Gaussian kernel optimization data for KPCA and KECA, given output from reduce.3D fucntion
	opt.set <- data$optimization$Gaussian 
	opt.long <- melt(opt.set, id = "sigma", measure = c("KPCA.vis", "KECA.vis"))
	opt.long$variable <- gsub(".vis", "", opt.long$variable)
	ggplot(opt.long, aes(log(sigma), value, colour = variable)) + 
		geom_line(size=1) + theme_gray() + labs(y="Visualizability in 3D", 
		title=
		"Visualizability in 3D for Dimension Reduction using Gaussian Kernel") +
		theme(legend.position="bottom", legend.key=element_blank(), 
		legend.title=element_blank()) + scale_y_continuous(limits = c(min(c(
		0, min(opt.long$value))),1))
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Polynomial Kernel Optimization Plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.Poly.opt <- function(data){
#Plot polynomial kernel optimization data for KPCA and KECA, given output from reduce.3D fucntion
	opt.set <- data$optimization$Polynomial 
	min.val <- min(c(0, opt.set$KPCA.vis, opt.set$KECA.vis))
	KPCA <- ggplot(opt.set) + aes(x = log10(offset), y = KPCA.vis, colour = 
		as.factor(degree)) + geom_line() + theme_gray() + 
		scale_y_continuous(limits=c(min.val,1)) + labs(y = 
		"Visualizability in 3D", title=
		"Visualizability in 3D using Polynomial KPCA") + 
		theme(legend.position="bottom", legend.key=element_blank()) +
		scale_colour_hue(name="Degree")
	KECA <- ggplot(opt.set) + aes(x = log10(offset), y = KECA.vis, colour = 
		as.factor(degree)) + geom_line() + theme_gray() + 
		scale_y_continuous(limits=c(min.val,1)) + labs(y = 
		"Visualizability in 3D", title=
		"Visualizability in 3D using Polynomial KECA") + 
		theme(legend.position="bottom", legend.key=element_blank())+
		scale_colour_hue(name="Degree")
	plot_grid(KPCA, KECA, ncol=2, nrow=1)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Create 3D scatterplots of all 5 reduction methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
visualize.plot <- function(data){
#Create scatterplots of all reduction methods given output from
#reduce.3D
	par(mfrow=c(2,3))
	methods <- c("PCA","KPCA.RBF","KECA.RBF","KPCA.Poly","KECA.Poly")
	m.names <- c("PCA","Gaussian KPCA","Gaussian KECA",
			"Polynomial KPCA","Polynomial KECA")
	for (i in 1:5){
		make.3D.plot(data, methods[i], m.names[i],data$visualizability[[i]])
	}
} 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make.3D.plot <- function(data, method, method.name, vis){
	cols <- grep(paste(method, ".F", sep=""), names(data$result))
	scatterplot3d(data$result[,cols[1]], 
		data$result[,cols[2]], data$result[,cols[3]],
		color=as.numeric(as.factor(data$result[,16])),
		xlab="Feature 1", ylab="Feature 2", zlab="Feature 3",
		main=paste(method.name, ", Visualizability =", round(vis,
		digits=4)))	
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
