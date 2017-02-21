# Visualization of High Dimension Data

## Project Summary
Reduce various high dimensional data sets to three dimensions while minimizing structural information loss.  Three different dimension reduction methods were implemented:
  - Principal Components Analysis (PCA)
  - Kernel Principal Components Analysis (KPCA)
  - Kernel Entropy Components Analysis (KECA)

The kernel methods utilized both Gaussian (also called RBF, radial basis function) and Polynomial kernels.  Kernel performance was assessed using a measure called visualizability in 3 dimensions.  This method compared cluster assignments on the original, high dimension data versus new cluster assignments only using the reduced 3 dimensions.  Higher visualizability corresponds to dimension reduction which better preserves the original clusters.

All coding was completed in R.  The kernlab package was used for KPCA reduction methods.  The KECA funtions were created, however, using some of the functionality from the kernlab package.  The algorithm for KECA was derived from:

_Jenssen, R. (2010).  Kernel entropy component analysis.  IEEE Transactions on Pattern Analysis and Machine Intelligence, 32(5), 847-860._

## Publication
This code was originally completed as part of a project assignment for a course on support vector machines (svm).  Pieces of this code, particularly the functions for graphing and the KECA functions were utilized for a publication on dimension reduction of high dimension big data.  The resulting paper was presented at the IEEE / ACM 2016 International Conference on Big Data Computing.  Publication details as follows:

_Ying Xie, Pooja Chenna, Jing (Selena) He, Linh Le, and Jacey Planteen, “Visualization of Big High Dimensional Data in a Three Dimensional Space“, IEEE/ACM International Conference on Big Data Computing, Applications and Technologies (BDCAT’16), Shanghai, China, December 6 – 9, 2016._

http://dl.acm.org/citation.cfm?id=3006299.3006340

## Structure
graphics >> contains png files of graphical output from the visualization functions for the example data sets.

main >> key functions and example script
