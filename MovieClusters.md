MovieClusters
================
Carlton Hencey
December 10, 2018

One of the biggest businesses in America is the movie industry. Billions of dollars are spent every year in movie production. However, not all of these movies are able to make their money back. Some movies thrive, while others lose millions of dollars. In order to better determine what factors are associated with movies, several qualtitative variables were clustered with gross revenue of over 6800 movies over the past 32 years. This data was scraped from IMBD and contains information on the movie's budget, runtime, IMDB score, number of IMDB votes, and how long ago the movie was released.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

The first step was to enter the data and include only the impactful variables moving forward. The code for the data cleaning process is included below.
-------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
library(fpc)
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
Movies<-read.csv("movies.csv")
Movies$released<-as.Date(Movies$released, format="%m/%d/%Y")

MovieData<-Movies[,c(1,6,10,11,13,15)]
MovieData$age<-(2018-MovieData$year)
MovieData<-MovieData[,-6]
head(MovieData)
```

    ##     budget     gross runtime score  votes age
    ## 1  8000000  52287414      89   8.1 299174  32
    ## 2  6000000  70136369     103   7.8 264740  32
    ## 3 15000000 179800601     110   6.9 236909  32
    ## 4 18500000  85160248     137   8.4 540152  32
    ## 5  9000000  18564613      90   6.9  36636  32
    ## 6  6000000 138530565     120   8.1 317585  32

The next step in the process is to understand how these clusters will work. First, all of the data was scaled so that the columns with larger values would not skew the clusters. Next, the Ward.D2 cluster was determined as the optimal clustering method, and 5 distinct clusters were determined based on the Cluster Dendrogram. The code and visuals for this process are shown below.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
ClusterData<-scale(MovieData[,c(1:6)])

d <- dist(ClusterData, method = "euclidean") 
fit <- hclust(d, method="ward.D2")
plot(fit)
rect.hclust(fit, k=5, border="red")
```

![](MovieClusters_files/figure-markdown_github-ascii_identifiers/Clustering-1.png)

Lastly, the data was broken down into the five clusters, as stated above, and the average values for all of the variables for each one of these clusters were calculated and put into a data frame. The calculations for the clusters and the ending data frame is provided below.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
groups<-cutree(fit, k=5)

DataGroups<-as.data.frame(cbind(ClusterData,groups))
GroupMeans<- DataGroups %>%
  select(budget,runtime,score,votes,age,groups,gross) %>%
  group_by(groups) %>%
  summarize(budget=mean(budget),runtime=mean(runtime),score=mean(score),votes=mean(votes),
            age=mean(age),AvgGross=mean(gross))

as.data.frame(GroupMeans)
```

    ##   groups     budget      runtime      score       votes        age
    ## 1      1  0.9630844  0.212775981  0.2377467  0.81866964 -0.3840527
    ## 2      2 -0.3697459  0.272893355  0.4920399 -0.33231073  0.6083654
    ## 3      3 -0.3823389 -0.642749359 -1.0134362 -0.44171502  0.6354705
    ## 4      4  3.0552605  1.157523152  0.8196596  2.83640621 -0.7441019
    ## 5      5 -0.2440083  0.008713604  0.1682204 -0.09869582 -1.1039717
    ##     AvgGross
    ## 1  0.7522461
    ## 2 -0.3168621
    ## 3 -0.3594653
    ## 4  3.3847772
    ## 5 -0.2593668

Analysis: There were some interesting insights from the clustering the movie details. The cluster with the highest Average Gross had a much higher Gross value than all of the other clusters. That cluster also had significantly larger budget and longer runtime. The larger budget makes sense, as more money put into a movie is expected to increase revenue, but the longer runtime is not as obviously connected to gross revenue. In addition, the highest grossing movie cluster had the highest movie score, while receiving the most votes, which indicates that these movies were not only better made, but also seen by a larger number of people. Lastly, that cluster had the second lowest age, meaning that newer movies are more likely to make more money, most likely due to the growing movie industry.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Another interesting insight comes from the three lowest grossing clusters, which have similar values of gross revenue. All of these clusters had low values of budget and votes, which indicates that the movies may not have been marketed well enough to reach a large number of viewers. One interesting insight from the low grossing clusters is that two of the clusters have the highest age, but the other cluster has the newest age. That indicates that age may not be as influential as a factor in overall gross as some other variables.
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

In conclusion, the most important factors in estimating the gross revenue of a movie are how much money is spent on the production of the movie and how long the movie lasts. On the other hand, all of the lower grossing movies had the least amount of votes, indicatng that they did not get enough exposure to reach a large audience. By investing more money in the movie up front for better quality, longer runtime, and marketing for more exposure, a movie company will give itself a better chance at receiving larger revenue.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
