# Gaining Insight into Online Sales of a Traditionally In-Store Purchase
__Language: R__

Packages: sqldf, car, ggplot2, tm, Deducer, RWeka, wordcloud

Analysis of premium lead-acid battery sales on Amazon.com to gain insights into the strengths and weaknesses of different brand names 
and other factors, such as volume of customer reviews and review ratings.  

Techniques utilized include:
   -  Web scraping to gather data
   -  Text mining of customer reviews
   -  Loglinear regression to test for association among categorical variables
   -  Modeling of sales rank (surrogate measure for sales volume) using linear regression

## Executive Summary
In today’s age of smartphones, social media, and online connectivity, an ever-growing list of products can be purchased online, which now 
includes lead-acid batteries. These very traditional products can be purchased from online retailers like Amazon.com, where customers can 
freely rate and review their new items. For now, most people who have a dead car battery are not going to order a replacement online and 
wait patiently for delivery; however, these people may consult online reviews as they make purchasing decisions in-store. Additionally, 
battery manufacturers may be able to gain valuable insight into customer preferences and opinions on their products as well as their 
competitors’ from online reviews. 

This study sought to determine whether meaningful insights could be gathered from data readily available on Amazon for three different 
battery brands: Exide, Optima, and Odyssey. Two different groups of data were extracted from Amazon and were analyzed with different 
techniques: 
  -  Sales rank, price, number of customer reviews, and average reviews rating were tracked for 108 different products for a period of two 
  weeks. Ordinary least squares (OLS) regression was used to understand the relationship of brand, price, and customer reviews on sales 
  rank, which was used as a surrogate measure for sales volume. 
  -  Individual customer reviews for the 108 battery types were also gathered. Term frequencies for individual words and two word phrases 
  were used to identify key terms of interest for further analysis. Various categorical techniques were used to analyze relationships in 
  the reviews, including the Pearson chi-square test for independence and log linear models for contingency tables. 

Many insights were indeed gained from this research. Some of the key findings included: 
  -  The average customer review rating for a given battery type has the most impact on sales volume for products with few customer 
  reviews.
  -  More Amazon users find negative customer reviews to be helpful than positive reviews, making minimization of negative feedback all 
  the more important. 
  -  Exide branded products had more negative customer reviews than the other two brands. Negative reviews have high occurrences of 
  words like “warranty” and “dead”. Additionally, shorter periods of time are mentioned in Exide reviews versus reviews for the other 
  brands. These findings suggest that Exide batteries may have shorter battery life or more premature battery failures than the 
  competition. 
  -  Toyota Prius batteries are relatively popular on Amazon.com, and this market segment appears to be dominated by Optima today. 
  
This study did reveal that online retailer websites can provide a wealth of information to manufacturers. Approaches of this type could 
provide a direct customer feedback pipeline to understand not only their own products’ performance but also that of other brands. 

## Structure
analysis >> code for web scraping, modeling, and text mining

output >> final report and additional graphics generated from analysis
