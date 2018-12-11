MarketBasketAnalysis
================
Carlton Hencey
December 11, 2018

For this project, the purchase history at a grocery store was analyzed to determine associations between differet products. This dataset contained 9,825 different transactions, each with a collection of different products. The data was read in using the read.transactions command, and each item in the basket was separated by a comma.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
library(arules)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
Orders<-read.transactions("groceries.csv",sep=",")
```

The first part of this process was to determine the details of all of the purchases. First, the summary of all the purchases was obtained. Through the summary, it can be seen that the distribution of the number of items per transactionis centered mostly around values under 10, with the most frequent number of items purchased of just 1 and a median number of items purchased of 3. Also, the most frequent items purchased are provided, which shows that the most frequent item purchased is Whole Milk, followed by Other Vegetables, Rolls/Buns, Soda, and Yogurt. For further visibility on the most frequent items, a plot is included to show the top 20 most frequent items.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
summary(Orders)
```

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55 
    ##   16   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   46   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##             labels
    ## 1 abrasive cleaner
    ## 2 artif. sweetener
    ## 3   baby cosmetics

``` r
itemFrequencyPlot(Orders, topN = 20)
```

![](MarketBasketAnalysis_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-1-1.png)

The next step was to determine the basket rules. The interpretation of the basket rules are given through the support, confidence, and lift. The support is the percentage of transactions that contain all of the items in an itemset, the confidence is the probability that a transaction that contains the items on the left hand side of the rule also contains the item on the right hand side, and the lift is the probability of all of the items in a rule occurring together.
=======================================================================================================================================================================================================================================================================================================================================================================================================================================================================================

``` r
Rules <- apriori(Orders, parameter = list(support =
                          0.005, confidence = 0.2, minlen = 2))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.2    0.1    1 none FALSE            TRUE       5   0.005      2
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 49 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [120 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.02s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [872 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.02s].

The first rules that were inspected were the 5 rules with the highest support. Since the support is centered around the all items appearing together the most, it makes sense that the most frequently occuring items (whole milk, other vegetables, and rolls) also have the highest support together.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
inspect(sort(Rules, by = "support")[1:5])
```

    ##     lhs                   rhs                support    confidence
    ## [1] {other vegetables} => {whole milk}       0.07483477 0.3867578 
    ## [2] {whole milk}       => {other vegetables} 0.07483477 0.2928770 
    ## [3] {rolls/buns}       => {whole milk}       0.05663447 0.3079049 
    ## [4] {whole milk}       => {rolls/buns}       0.05663447 0.2216474 
    ## [5] {yogurt}           => {whole milk}       0.05602440 0.4016035 
    ##     lift     count
    ## [1] 1.513634 736  
    ## [2] 1.513634 736  
    ## [3] 1.205032 557  
    ## [4] 1.205032 557  
    ## [5] 1.571735 551

The next rules that were examined were the 5 rules with the highest confidence. Each of these rules contains Whole Milk on the right hand side, which indicates that each of the item groups on the left hand side has a very strong association with milk.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
inspect(sort(Rules, by = "confidence")[1:5])
```

    ##     lhs                     rhs              support confidence     lift count
    ## [1] {root vegetables,                                                         
    ##      tropical fruit,                                                          
    ##      yogurt}             => {whole milk} 0.005693950  0.7000000 2.739554    56
    ## [2] {other vegetables,                                                        
    ##      pip fruit,                                                               
    ##      root vegetables}    => {whole milk} 0.005490595  0.6750000 2.641713    54
    ## [3] {butter,                                                                  
    ##      whipped/sour cream} => {whole milk} 0.006710727  0.6600000 2.583008    66
    ## [4] {pip fruit,                                                               
    ##      whipped/sour cream} => {whole milk} 0.005998983  0.6483516 2.537421    59
    ## [5] {butter,                                                                  
    ##      yogurt}             => {whole milk} 0.009354347  0.6388889 2.500387    92

The top 5 rules for lift were examined next. The main takeway from these association is that root vegetables have a much higher chance of being in a basket if herbs or citrus fruit, other vegetables, and whole milk are in the basket, while whipped/sour cream has a much higher chance of being in a basket if berries or butter and other vegeatbles are in the basket.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
inspect(sort(Rules, by = "lift")[1:5])
```

    ##     lhs                   rhs                      support confidence     lift count
    ## [1] {citrus fruit,                                                                  
    ##      other vegetables,                                                              
    ##      whole milk}       => {root vegetables}    0.005795628  0.4453125 4.085493    57
    ## [2] {butter,                                                                        
    ##      other vegetables} => {whipped/sour cream} 0.005795628  0.2893401 4.036397    57
    ## [3] {herbs}            => {root vegetables}    0.007015760  0.4312500 3.956477    69
    ## [4] {citrus fruit,                                                                  
    ##      pip fruit}        => {tropical fruit}     0.005592272  0.4044118 3.854060    55
    ## [5] {berries}          => {whipped/sour cream} 0.009049314  0.2721713 3.796886    89

Lastly, since Whole Milk was the most frequently purchased item, a closer look was taken at the lift of whole milk. Through the top 5 lifts involving Whole Milk, it can be seen that the baskets involving milk also have a much higher likelihood of having root vegetables, butter, and tropical fruit.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
milkrules <- subset(Rules, items %in% "whole milk")
inspect(sort(milkrules, by = "lift")[1:5])
```

    ##     lhs                     rhs                   support confidence     lift count
    ## [1] {citrus fruit,                                                                 
    ##      other vegetables,                                                             
    ##      whole milk}         => {root vegetables} 0.005795628  0.4453125 4.085493    57
    ## [2] {other vegetables,                                                             
    ##      tropical fruit,                                                               
    ##      whole milk}         => {root vegetables} 0.007015760  0.4107143 3.768074    69
    ## [3] {whipped/sour cream,                                                           
    ##      whole milk}         => {butter}          0.006710727  0.2082019 3.757185    66
    ## [4] {root vegetables,                                                              
    ##      whole milk,                                                                   
    ##      yogurt}             => {tropical fruit}  0.005693950  0.3916084 3.732043    56
    ## [5] {other vegetables,                                                             
    ##      pip fruit,                                                                    
    ##      whole milk}         => {root vegetables} 0.005490595  0.4060150 3.724961    54

In conclusion, these association rules can be used by the grocery store to optimize their marketing and coupon strategies. By aligning coupon strategies with lift associations, the grocery can capitalize off of customer behaviors that are already occuring.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
