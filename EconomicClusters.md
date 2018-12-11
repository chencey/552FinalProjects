552EconomicClusters
================
Carlton Hencey
December 7, 2018

The goal of this project is to analyze different economic groupings of countries and the how different factors are associated with these groupings. The first part of this process was to enter the data, clean it by choosing only the important variables, and then scale the variables. The code and results are given below.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
library(fpc)
library(cluster)
Data<-read.csv("economic2018data.csv")
EconomicData<-Data[,c(2,5,7:24,26:34)]
colnames(EconomicData)<-c("Name","Rank","Score","Property","Judicial","Gov","Tax","GovSpending",
                          "FiscalHealth","BusinessFreedom","LaborFreedom","MonetaryFreedom","TradeFreedom",
                          "InvestmentFreedom","FinancialFreedom","Tariff","IncomeTax","CorporateTax",
                          "TaxBurdernOfGDP","GovExpendOfGDP","Population","GDP","GDPGrowth","5YearGrowth",
                          "GDPperCapita","Unemployment","Inflation","FDIinflow","PublicDebt")
for (i in 2:29){
  EconomicData[,i]<-as.numeric(as.character(EconomicData[,i]))
}
```

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

    ## Warning: NAs introduced by coercion

``` r
EconomicData<-EconomicData[complete.cases(EconomicData),]
EconomicData[,c(2:29)]<-scale(EconomicData[,c(2:29)])
head(EconomicData)
```

    ##          Name       Rank      Score    Property    Judicial        Gov
    ## 1 Afghanistan  1.2533020 -1.0049203 -1.77930627 -1.01282861 -0.8765723
    ## 2     Albania -0.4662748  0.2807021  0.06789939 -1.15463691 -0.1515962
    ## 3     Algeria  1.6010816 -1.6477316 -1.27413124 -0.65830786 -0.7284020
    ## 4      Angola  1.4465129 -1.2678886 -0.85570344 -1.15463691 -1.2628734
    ## 5   Argentina  1.0600911 -0.9075247 -0.61077009 -0.18730172 -0.5378973
    ## 6     Armenia -0.8720177  0.6897638  0.12913273 -0.04042883 -0.1198454
    ##          Tax GovSpending  FiscalHealth BusinessFreedom LaborFreedom
    ## 1  1.2223156   0.6602205  1.0266723107      -0.8902328   -0.1467856
    ## 2  0.6657310   0.3503809  0.0105457174       0.2532145   -0.6356749
    ## 3 -0.2563717  -0.9077557 -1.5829255312       0.1821929   -0.7561259
    ## 4  0.4414357   0.1813775 -0.4084415467      -0.5138185   -0.6002481
    ## 5 -0.9458720  -0.4523854 -0.4810220177      -0.6629638   -1.1387349
    ## 6  0.6325021   0.6930823  0.0006483805       0.9350216    0.7459689
    ##   MonetaryFreedom TradeFreedom InvestmentFreedom FinancialFreedom
    ## 1      0.12234182   -1.0253072        -2.1894809       -2.0876743
    ## 2      0.59041694    1.0939757         0.5160919        1.0758380
    ## 3     -0.68152630   -1.2683443        -1.5130877       -1.0331702
    ## 4     -1.84153854   -1.6572035        -1.2876233       -0.5059182
    ## 5     -2.57417785   -0.6072835        -0.1603013        0.5485860
    ## 6     -0.08116909    0.3357002         0.7415563        1.0758380
    ##       Tariff  IncomeTax CorporateTax TaxBurdernOfGDP GovExpendOfGDP
    ## 1  0.3578482 -0.6579281   -0.4433530     -1.85930466    -0.58046350
    ## 2 -0.9418843 -0.4284491   -1.0002483      0.36976454    -0.21358043
    ## 3  0.6327493  0.4894666   -0.1092159      0.56054073     0.96044541
    ## 4  1.0659934 -0.8874070    0.6704375      0.04845727    -0.02966242
    ## 5  0.4304220  0.4894666    1.2273328      0.78143948     0.58117408
    ## 6 -0.6317959 -0.1989702   -0.4433530     -0.22264574    -0.62715771
    ##     Population         GDP    GDPGrowth 5YearGrowth GDPperCapita
    ## 1 -0.055401373 -0.26797160 -0.277610703   0.5057079  -0.83966885
    ## 2 -0.260159477 -0.28083331  0.006112803  -0.4473679  -0.38886744
    ## 3 -0.006007088 -0.03221799  0.289836309   0.1757971  -0.24409717
    ## 4 -0.095936901 -0.21502021  0.109284987   0.3305701  -0.61585773
    ## 5  0.013052663  0.08021881 -0.354989841  -1.3474950  -0.01596052
    ## 6 -0.259488359 -0.28443562  0.109284987   0.1228484  -0.53513800
    ##   Unemployment   Inflation  FDIinflow PublicDebt
    ## 1   0.01628595 -0.06455105 -0.2575763 -1.4066208
    ## 2   1.26121453 -0.21538861 -0.2312404  0.4582880
    ## 3   0.44457974  0.03373665 -0.2204043 -1.0502631
    ## 4  -0.29694383  1.29785270  0.1089957  0.4683096
    ## 5  -0.30014005  2.56391504 -0.1124891 -0.1391784
    ## 6   1.32993330 -0.34627668 -0.2514481 -0.1232617

In order to determine how different factors are affecting economic indicators, they will each be included in clustering with Score, GDP, GDPGrowth, and Unemployment. The factors in question are Business Freedom, Labor Freedom, Monetary Freedom, Trade Freedom, Investment Freedom, and Financial Freedom. The code and results of each one of these clusters are provided below.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

First, Business Freedom was clustered with Score, GDP, GDPGrowth, and Unemployment. By using the ward.D2 method, two clearly defined clusters were found. Next, the cluster centers for these two clusters. As shown below, one cluster has a much higher Business Freedom value, as well as a high economic Score, high GDP, and low Unemployment. However, the cluster with high Business Freedom had a low value for GDP Growth. This is interesting because more Business Freedom would be exected to increase growth within a country.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
set.seed(1)

BusinessFreedomClusterData<-EconomicData[c(10,3,22,23,26)]

d <- dist(BusinessFreedomClusterData, method = "euclidean")
fit <- hclust(d, method="ward.D2")
groups <- cutree(fit, k=2)
plot(fit)
rect.hclust(fit, k=2, border="red") #two clearly defined groups
```

![](EconomicClusters_files/figure-markdown_github-ascii_identifiers/Business%20Freedom%20Clustering-1.png)

``` r
Clusters<-cbind(BusinessFreedomClusterData,groups)
Cluster1<-Clusters[which(Clusters$groups==1),]
Cluster2<-Clusters[which(Clusters$groups==2),]
(Cluster1centroid <- colMeans(Cluster1[,c(1:5)])) #cluster 1 center
```

    ## BusinessFreedom           Score             GDP       GDPGrowth 
    ##     -0.55847066     -0.53375687     -0.19366806      0.06459407 
    ##    Unemployment 
    ##      0.23810694

``` r
(Cluster2centroid <- colMeans(Cluster2[,c(1:5)])) #cluster 2 center
```

    ## BusinessFreedom           Score             GDP       GDPGrowth 
    ##      0.78341023      0.74874228      0.27167325     -0.09061112 
    ##    Unemployment 
    ##     -0.33401113

The next factor compared with Score, GDP, GDPGrowth, and Unemployment is Labor Freedom. This clustering was also done using the ward.D2 method, which created two clusters. Of the two clusters, one had a much higher Labor Freedom value, and that cluster is also associated with a higher economic Score, higher GDP, and lower Unemployment. However, this cluster was also associated with a lower GDP Growth, which is the same as Business Freedom.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
set.seed(1)

LaborFreedomClusterData<-EconomicData[c(11,3,22,23,26)]

d <- dist(LaborFreedomClusterData, method = "euclidean")
fit <- hclust(d, method="ward.D2")
groups <- cutree(fit, k=2)
plot(fit)
rect.hclust(fit, k=2, border="red") #two clearly defined groups
```

![](EconomicClusters_files/figure-markdown_github-ascii_identifiers/Labor%20Freedom%20Clustering-1.png)

``` r
Clusters<-cbind(LaborFreedomClusterData,groups)
Cluster1<-Clusters[which(Clusters$groups==1),]
Cluster2<-Clusters[which(Clusters$groups==2),]
(Cluster1centroid <- colMeans(Cluster1[,c(1:5)])) #cluster 1 center
```

    ## LaborFreedom        Score          GDP    GDPGrowth Unemployment 
    ##  -0.42869483  -0.47872531  -0.10563575   0.02391685   0.25727546

``` r
(Cluster2centroid <- colMeans(Cluster2[,c(1:5)])) #cluster 2 center
```

    ## LaborFreedom        Score          GDP    GDPGrowth Unemployment 
    ##   0.80737526   0.90159933   0.19894734  -0.04504341  -0.48453544

Next, Monetary Freedom was clustered with Score, GDP, GDPGrowth, and Unemployment. For this factor, the ward.D method produced the two most clearly defined clusters. However, the cluster with the higher Monetary Freedom value also has a low GDP, low GDP growth, and high Unemployment. This would appear to indicate that Monetary Freedom is not associated with economic health of a country.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
set.seed(1)

MonetaryFreedomClusterData<-EconomicData[c(12,3,22,23,26)]

d <- dist(MonetaryFreedomClusterData, method = "euclidean")
fit <- hclust(d, method="ward.D")
groups <- cutree(fit, k=2)
plot(fit)
rect.hclust(fit, k=2, border="red") #two clearly defined groups
```

![](EconomicClusters_files/figure-markdown_github-ascii_identifiers/Monetary%20Freedom%20Clustering-1.png)

``` r
Clusters<-cbind(MonetaryFreedomClusterData,groups)
Cluster1<-Clusters[which(Clusters$groups==1),]
Cluster2<-Clusters[which(Clusters$groups==2),]
(Cluster1centroid <- colMeans(Cluster1[,c(1:5)])) #cluster 1 center
```

    ## MonetaryFreedom           Score             GDP       GDPGrowth 
    ##    -0.488720908    -0.480708593     0.013934649     0.001276607 
    ##    Unemployment 
    ##    -0.408312198

``` r
(Cluster2centroid <- colMeans(Cluster2[,c(1:5)])) #cluster 2 center
```

    ## MonetaryFreedom           Score             GDP       GDPGrowth 
    ##     0.609314379     0.599325000    -0.017373068    -0.001591614 
    ##    Unemployment 
    ##     0.509064559

The next factor included in clustering is Trade Freedom. Ward.D2 method was used for this clustering as well, and resulted in two clusters, with one having a much higher value of Trade Freedom. Although the cluster with Trade Freedom has a higher Score, GDP, and GDP Growth, it also has a higher Unemployment value, which indicates that increasing trade decreases the number of jobs needed at home.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
set.seed(1)

TradeFreedomClusterData<-EconomicData[c(13,3,22,23,26)]

d <- dist(TradeFreedomClusterData, method = "euclidean")
fit <- hclust(d, method="ward.D2")
groups <- cutree(fit, k=2)
plot(fit)
rect.hclust(fit, k=2, border="red") #two clearly defined groups
```

![](EconomicClusters_files/figure-markdown_github-ascii_identifiers/Trade%20Freedom%20Clustering-1.png)

``` r
Clusters<-cbind(TradeFreedomClusterData,groups)
Cluster1<-Clusters[which(Clusters$groups==1),]
Cluster2<-Clusters[which(Clusters$groups==2),]
(Cluster1centroid <- colMeans(Cluster1[,c(1:5)])) #cluster 1 center
```

    ## TradeFreedom        Score          GDP    GDPGrowth Unemployment 
    ##   -0.9306658   -0.8278894   -0.1856211   -0.1945419   -0.1577672

``` r
(Cluster2centroid <- colMeans(Cluster2[,c(1:5)])) #cluster 2 center
```

    ## TradeFreedom        Score          GDP    GDPGrowth Unemployment 
    ##    0.6027169    0.5361570    0.1202118    0.1259891    0.1021730

The next factor included in clustering is Investment Freedom. the method used for this clustering was ward.D2, and that method resulted in two clusters. The cluster with a higher value of Investment Freedom also had a high Score, high GDP, low GDP Growth, and high Unemployment. Therefore, Investment Freedom does not have the best association to economic prosperity of all the factors used for clustering.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
set.seed(1)

InvestmentFreedomClusterData<-EconomicData[c(14,3,22,23,26)]

d <- dist(InvestmentFreedomClusterData, method = "euclidean")
fit <- hclust(d, method="ward.D2")
groups <- cutree(fit, k=2)
plot(fit)
rect.hclust(fit, k=2, border="red") #two clearly defined groups
```

![](EconomicClusters_files/figure-markdown_github-ascii_identifiers/Investment%20Freedom%20Clustering-1.png)

``` r
Clusters<-cbind(InvestmentFreedomClusterData,groups)
Cluster1<-Clusters[which(Clusters$groups==1),]
Cluster2<-Clusters[which(Clusters$groups==2),]
(Cluster1centroid <- colMeans(Cluster1[,c(1:5)])) #cluster 1 center
```

    ## InvestmentFreedom             Score               GDP         GDPGrowth 
    ##       -0.65222361       -0.72323191       -0.11392355        0.02453641 
    ##      Unemployment 
    ##       -0.41420783

``` r
(Cluster2centroid <- colMeans(Cluster2[,c(1:5)])) #cluster 2 center
```

    ## InvestmentFreedom             Score               GDP         GDPGrowth 
    ##        0.52313769        0.58009226        0.09137618       -0.01968024 
    ##      Unemployment 
    ##        0.33222919

The last factor analyzed by itself was Financial Freedom. By using ward.D2, two clearly defined clusters were created. The cluster with a high Financial Freedom score also had a high Score, high GDP, and low unemployment. However, it also was associated with low GDP Growth.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
set.seed(1)

FinancialFreedomClusterData<-EconomicData[c(15,3,22,23,26)]

d <- dist(FinancialFreedomClusterData, method = "euclidean")
fit <- hclust(d, method="ward.D2")
groups <- cutree(fit, k=2)
plot(fit)
rect.hclust(fit, k=2, border="red") #two clearly defined groups
```

![](EconomicClusters_files/figure-markdown_github-ascii_identifiers/Financial%20Freedom%20Clustering-1.png)

``` r
Clusters<-cbind(FinancialFreedomClusterData,groups)
Cluster1<-Clusters[which(Clusters$groups==1),]
Cluster2<-Clusters[which(Clusters$groups==2),]
(Cluster1centroid <- colMeans(Cluster1[,c(1:5)])) #cluster 1 center
```

    ## FinancialFreedom            Score              GDP        GDPGrowth 
    ##      -0.66858103      -0.64248422      -0.14946046       0.02669236 
    ##     Unemployment 
    ##       0.13388313

``` r
(Cluster2centroid <- colMeans(Cluster2[,c(1:5)])) #cluster 2 center
```

    ## FinancialFreedom            Score              GDP        GDPGrowth 
    ##       0.79552679       0.76447490       0.17783903      -0.03176053 
    ##     Unemployment 
    ##      -0.15930398

Finally, each of the six factors analyzed individually were all included together with each of the four economic indicators into new clusters. The Ward.D2 method combined these factors into two clusters. One cluster had high values of all six factors (Business Freedom, Labor Freedom, Monetary Freedom, Trade Freedom, Investment Freedom, Financial Freedom). This cluster also had a very high economic Score, a high GDP, high GDP Growth, and low unemployment. This indicates that when all of these factors have high values, the economic health of the country as a whole isexpected to be very good.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
set.seed(1)

AllClusterData<-EconomicData[c(10:15,3,22,23,26)]

d <- dist(AllClusterData, method = "euclidean")
fit <- hclust(d, method="ward.D2")
groups <- cutree(fit, k=2)
plot(fit)
rect.hclust(fit, k=2, border="red") #two clearly defined groups
```

![](EconomicClusters_files/figure-markdown_github-ascii_identifiers/All%20Factors%20Clustering-1.png)

``` r
Clusters<-cbind(AllClusterData,groups)
Cluster1<-Clusters[which(Clusters$groups==1),]
Cluster2<-Clusters[which(Clusters$groups==2),]
(Cluster1centroid <- colMeans(Cluster1[,c(1:10)])) #cluster 1 center
```

    ##   BusinessFreedom      LaborFreedom   MonetaryFreedom      TradeFreedom 
    ##       -0.32404224       -0.20962002       -0.22843843       -0.32814890 
    ## InvestmentFreedom  FinancialFreedom             Score               GDP 
    ##       -0.28439409       -0.35060360       -0.40929692       -0.13932107 
    ##         GDPGrowth      Unemployment 
    ##       -0.01248218        0.16819323

``` r
(Cluster2centroid <- colMeans(Cluster2[,c(1:10)])) #cluster 2 center
```

    ##   BusinessFreedom      LaborFreedom   MonetaryFreedom      TradeFreedom 
    ##        0.95003293        0.61456778        0.66973994        0.96207291 
    ## InvestmentFreedom  FinancialFreedom             Score               GDP 
    ##        0.83379176        1.02790601        1.19998415        0.40846404 
    ##         GDPGrowth      Unemployment 
    ##        0.03659549       -0.49311197

In conclusion, all of these factors have positive associations with economic success within a company. All of these factors not only had associations with good economic indicators when grouped together, but many of them were associated with economic success individually as well. All six factors were associated with high economic Scores when clustered individually. Also, five out of the six factors were associated with a high GDP. The only unexpected result came from the GDP Growth indicator. All of the factors except for Trade Freedom were associated with low GDP Growth scores. However, the low growth may be a factor of an already successful economy without as much room to grow as a smaller economy. The least impactful factor is Monetary Freedom, which was the only factor with low GDP, low GDP Growth, and high unemployment.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
