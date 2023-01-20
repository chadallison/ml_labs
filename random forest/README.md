random forest
================
chad allison \| 20 january 2023

### setup

``` r
library(randomForest)
```

### creating model

``` r
little_forest = randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                             data = iris)

print(little_forest)
```

    ## 
    ## Call:
    ##  randomForest(formula = Species ~ Sepal.Length + Sepal.Width +      Petal.Length + Petal.Width, data = iris) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 2
    ## 
    ##         OOB estimate of  error rate: 4.67%
    ## Confusion matrix:
    ##            setosa versicolor virginica class.error
    ## setosa         50          0         0        0.00
    ## versicolor      0         47         3        0.06
    ## virginica       0          4        46        0.08

### variable importance

``` r
importance(little_forest, type = 2)
```

    ##              MeanDecreaseGini
    ## Sepal.Length         9.100400
    ## Sepal.Width          2.321789
    ## Petal.Length        43.337536
    ## Petal.Width         44.512248
