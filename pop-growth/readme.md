# Modeling Population Growth in the United States

__Language: R__

Packages: plyr, prospectr, car, MASS, ggplot2, maps, mapproj, rgdal, RColorBrewer


Modeling of US population growth between 2000 and 2010 by county using US census data. OLS and robust regression utilized to develop 
various models. This research was submitted to Kennesaw State University's 2015 R Day poster competition and won 1st place in the graduate 
competition.

## Abstract

This research modeled population change between 2000 and 2010 by county in the United States utilizing various demographic, social, 
and economic variables from decennial census data. A useful population change model could help communities ensure infrastructure keeps 
pace with a growing – or declining – population size. The population change model was developed using a variety of techniques, including 
ordinary least squares regression and robust regression. Census observations were split into training and test data sets using the DUPLEX 
algorithm in order to both develop and validate the model. Ultimately, the final model only explains approximately 58% of the overall 
variation in population change; clearly not all important factors for population growth and decline may be captured in the census data 
alone. However, numerous regressors are significant and could be useful indicators. Some of the strongest indicators are the percent of 
the population under five years old, the percent enrolled in college, the percent of the population in the labor force, geographic region, 
and the difference between the average family size and average household size. Thus, though this model has substantial error, it does have 
some utility and value for understanding population growth and decline.

## Files and Structure

main: contains scripts to clean and process data, develop models, and visualize data with choropleths.

PopulationChangePoster.pdf: poster as presented at 2015 R Day competition
