ST558 Project2
================
Shan Luo, Chengxi Zhou
2022-07-03

``` r
rmarkdown::render("ST558_Project2_Group10.Rmd", output_file = "lifestyle.html", params = list(Channels = 'data_channel_is_bus'))
```

# Introduction

``` r
# Read in data and subset data
News <- read_csv("OnlineNewsPopularity.csv", show_col_types = FALSE) 
News <- News %>% 
  filter(!!rlang::sym(params$Channels) == 1) %>%
  select(shares, n_tokens_content, num_imgs, num_videos,
         global_rate_positive_words, global_subjectivity, is_weekend)
News$is_weekend <- factor(News$is_weekend)
```

``` r
# Split train and test data
set.seed(1)
trainIndex <- createDataPartition(News$shares, p = 0.7, list = FALSE)
train <- News[trainIndex, ]
test <- News[-trainIndex, ]
```

``` r
# Check correlation of all interested variables
cor(select(News ,shares, n_tokens_content, num_imgs, num_videos,
           global_rate_positive_words, global_subjectivity))
```

    ##                                 shares n_tokens_content
    ## shares                      1.00000000      -0.02011013
    ## n_tokens_content           -0.02011013       1.00000000
    ## num_imgs                    0.08035302       0.24321580
    ## num_videos                  0.03300678       0.05790071
    ## global_rate_positive_words  0.02152962       0.13704763
    ## global_subjectivity         0.02232124       0.18432991
    ##                               num_imgs  num_videos
    ## shares                      0.08035302  0.03300678
    ## n_tokens_content            0.24321580  0.05790071
    ## num_imgs                    1.00000000 -0.03302750
    ## num_videos                 -0.03302750  1.00000000
    ## global_rate_positive_words -0.12834892  0.00208748
    ## global_subjectivity        -0.17540318  0.04783557
    ##                            global_rate_positive_words
    ## shares                                     0.02152962
    ## n_tokens_content                           0.13704763
    ## num_imgs                                  -0.12834892
    ## num_videos                                 0.00208748
    ## global_rate_positive_words                 1.00000000
    ## global_subjectivity                        0.53016953
    ##                            global_subjectivity
    ## shares                              0.02232124
    ## n_tokens_content                    0.18432991
    ## num_imgs                           -0.17540318
    ## num_videos                          0.04783557
    ## global_rate_positive_words          0.53016953
    ## global_subjectivity                 1.00000000

If two variables have high correlation, we may think about removing one
of them.

# EDA

## Numeric Summary Table

``` r
# Compute the summary statistics
apply(X = select(train, shares:global_subjectivity), MARGIN = 2,
      FUN = function(x) {
        summaries <- c(min(x), mean(x), median(x), max(x), sd(x), IQR(x))
        names(summaries) <- c("Min", "Mean", "Median", "Max", "Sd", "IQR")
        summaries
      })
```

    ##            shares n_tokens_content   num_imgs num_videos
    ## Min        41.000           0.0000   0.000000  0.0000000
    ## Mean     2311.951         596.4554   2.839322  0.5566102
    ## Median   1100.000         509.0000   1.000000  0.0000000
    ## Max    284700.000        4661.0000 100.000000 51.0000000
    ## Sd       6522.920         411.0051   5.292458  1.5784880
    ## IQR       998.000         440.0000   1.000000  1.0000000
    ##        global_rate_positive_words global_subjectivity
    ## Min                    0.00000000           0.0000000
    ## Mean                   0.03136086           0.4032973
    ## Median                 0.03067485           0.4145577
    ## Max                    0.11272727           0.9500000
    ## Sd                     0.01433364           0.1083868
    ## IQR                    0.01824313           0.1095453

From numeric summary table, if one variable’s mean is greater than
median, it has a right skewed distribution. If the mean is less than
median, it has a left skewed distribution. If mean is close to median,
it may have a symmetric distribution. If the standard deviation is
unusual, there may be some outliers.

## Contingency Table

``` r
# Create contingency table of predictor "is_weekend"
table(train$is_weekend)
```

    ## 
    ##    0    1 
    ## 5143  757

From the contingency table, we can see how many articles are published
on weekday and weekend.

## Bar Plot

``` r
# Create bar plot of predictor "is_weekend"
g <- ggplot(data = train, aes(x = is_weekend))
g + geom_bar(fill = "Red", color = "Blue") +
  labs(title = "Bar Plot of is_weekend")
```

![](WorldAnalysis_files/figure-gfm/barplot-1.png)<!-- -->

From the bar plot, we can see how many articles are published on weekday
and weekend and visualize the difference.

``` r
# Create histogram of response "shares" and fill with predictor "is_weekend"
g <- ggplot(data = train, aes(x = shares))
g + geom_histogram(bins = 30, aes(fill = is_weekend)) +
  labs(x = "Number of Shares",
       title = "Histogram of Shares") +
  scale_fill_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

![](WorldAnalysis_files/figure-gfm/histograms-1.png)<!-- -->

For histogram, we can see the distribution of the number of shares. If
we have majority of count on the left side and less count on right side,
it may have a right skewed distribution. It indicates that most of
articles have small number of shares. If we have majority of count on
the right side and less count on left side, it may have a left skewed
distribution. It indicates that most of articles have large number of
shares. If we see a bell shape, it may have a symmetric distribution. It
indicating most of articles have relatively large shares.

``` r
# Create scatter plot of response "shares" and predictor "n_tokens_content".
# Filled with predictor "is_weekend"
g <- ggplot(data = train, aes(x = n_tokens_content, y = shares))
g + geom_point(aes(color = is_weekend)) +
  geom_smooth(method = "lm") +
  labs(x = "Number of Words in Content",
       y = "Number of Shares",
       title = "Scatter Plot of Shares vs Number of Words in Content") +
  scale_color_discrete(name = "Weekend Published", labels = c("No", "Yes"))
```

    ## `geom_smooth()` using formula 'y ~ x'

![](WorldAnalysis_files/figure-gfm/scatterplot-1.png)<!-- -->

We can inspect the trend of shares as a function of the number of words
in content. If the points show an upward trend, then articles with more
number of words in the content to be shared more often. If we see a
negative trend then articles with more number of words in the content
tend to be shared less often.

# Modeling

## Linear Regression

Regression models allow easy prediction of response and inference.
Linear regression is that we model a response as a linear function of
some predictors. Model fit by minimizing the sum of squared residuals.

``` r
# Fit linear model
mod <- lm(shares ~ (n_tokens_content + num_imgs + num_videos + 
                      global_rate_positive_words + global_subjectivity + 
                      is_weekend)^2 ,data =  train)
# Use forward selection to  choose model
forward_mod <- step(mod, direction = "forward")
```

    ## Start:  AIC=103559.4
    ## shares ~ (n_tokens_content + num_imgs + num_videos + global_rate_positive_words + 
    ##     global_subjectivity + is_weekend)^2

``` r
lmfit1 <- train(shares ~ (n_tokens_content + num_imgs + num_videos + 
                         global_rate_positive_words + global_subjectivity + 
                         is_weekend)^2,
             data = train,
             method = "lm",
             preProcess = c("center", "scale"),
             trControl = trainControl(method = "cv", number = 5))
lmfit1
```

    ## Linear Regression 
    ## 
    ## 5900 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (21), scaled (21) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 4721, 4719, 4720, 4719, 4721 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared    MAE     
    ##   6216.269  0.01938039  1986.099
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
lmpred1 <- predict(lmfit1, newdata = test) 
lm1 <- postResample(lmpred1, test$shares)
lm1
```

    ##         RMSE     Rsquared          MAE 
    ## 4.910846e+03 1.354806e-02 1.895915e+03

``` r
lmfit2 <- train(shares ~ n_tokens_content + num_imgs + num_videos + 
                         global_rate_positive_words + global_subjectivity + 
                         is_weekend,
             data = train,
             method = "lm",
             preProcess = c("center", "scale"),
             trControl = trainControl(method = "cv", number = 5))
lmfit2
```

    ## Linear Regression 
    ## 
    ## 5900 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 4720, 4721, 4720, 4721, 4718 
    ## Resampling results:
    ## 
    ##   RMSE     Rsquared    MAE     
    ##   6254.45  0.01728515  1972.325
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
lmpred2 <- predict(lmfit2, newdata = test) 
lm2 <- postResample(lmpred2, test$shares)
lm2
```

    ##         RMSE     Rsquared          MAE 
    ## 4.902192e+03 1.315236e-02 1.892490e+03

## Random Forest

Since the response is continuous, we choose to use regression tree.

The bootstrapping is that we resample from data or a fitted model and
apply method or estimation to each resample. We see how the model or
method behave.

For Bootstrap Aggregation(Bagged) for regression tree, we create a
bootstrap sample, train tree on the sample, repeat B = 1000 times, and
average over these predictions as final prediction.

Random forest follows Bootstrap Aggregation idea. We will create
multiple trees from bootstrap samples and average the results. But, we
will use a random subset of predictors for each bootstrap tree fit
instead of using all predictors. It may make bagged trees predictions
more correlated, which can help with reduction of variation.

``` r
rffit <- train(shares ~ n_tokens_content + num_imgs + num_videos + 
                         global_rate_positive_words + global_subjectivity + 
                         is_weekend, 
               data = train, 
               method = "rf", 
               trControl = trainControl(method = "cv", number = 5), 
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(mtry = 1:6))
rffit
```

    ## Random Forest 
    ## 
    ## 5900 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 4720, 4720, 4720, 4720, 4720 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared     MAE     
    ##   1     6277.066  0.014116899  1944.413
    ##   2     6356.651  0.015764978  2016.943
    ##   3     6430.661  0.013614552  2064.197
    ##   4     6533.173  0.011670037  2101.385
    ##   5     6662.034  0.010538321  2132.230
    ##   6     6774.038  0.009589344  2161.147
    ## 
    ## RMSE was used to select the optimal model using the
    ##  smallest value.
    ## The final value used for the model was mtry = 1.

``` r
rfpred <- predict(rffit, newdata = test) 
rf <- postResample(rfpred, test$shares)
rf
```

    ##         RMSE     Rsquared          MAE 
    ## 4.932713e+03 9.838046e-03 1.877377e+03

## Boosted Tree

Boosting is a way that slowly trains the tree so that the tree do not
over fit. For boosting, trees grow sequentially and each subsequent tree
is grown on a modified version of original data. Prediction updates as
trees grown.

The process of boosted tree:  
1. Initialized prediction as 0  
2. Find residuals(observed-predicted) 3. Fit a tree with d splits(d + 1
terminal nodes) treating the residuals as response  
4. Update predictions  
5. Update residuals for new predictions and repeat B times

``` r
boostedTfit <- train(shares ~ n_tokens_content + num_imgs + num_videos + 
                         global_rate_positive_words + global_subjectivity + 
                         is_weekend, 
               data = train, 
               method = "gbm", 
               trControl = trainControl(method = "cv", number = 5), 
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(expand.grid(n.trees = c(25,50,100,150,200), 
                                                 interaction.depth = 1:4,
                                                 shrinkage = 0.1,
                                                 n.minobsinnode = 10)),
               verbose = FALSE)
boostedTfit
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 5900 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 4720, 4720, 4719, 4721, 4720 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE      Rsquared    MAE     
    ##   1                   25      6035.912  0.01719569  1959.264
    ##   1                   50      6036.414  0.01711357  1951.671
    ##   1                  100      6028.187  0.02131993  1945.858
    ##   1                  150      6038.749  0.01864150  1942.928
    ##   1                  200      6032.422  0.02079747  1948.497
    ##   2                   25      6077.293  0.01079083  1962.536
    ##   2                   50      6067.544  0.01877149  1953.278
    ##   2                  100      6102.848  0.01968875  1985.845
    ##   2                  150      6156.938  0.01642336  2003.046
    ##   2                  200      6187.024  0.01644146  2012.134
    ##   3                   25      6063.597  0.02120393  1959.674
    ##   3                   50      6094.166  0.02241517  1963.892
    ##   3                  100      6171.554  0.01667218  1999.357
    ##   3                  150      6232.358  0.01643573  2036.658
    ##   3                  200      6294.562  0.01448298  2055.376
    ##   4                   25      6089.766  0.01895732  1976.435
    ##   4                   50      6150.088  0.01725549  1991.449
    ##   4                  100      6229.733  0.01799312  2031.613
    ##   4                  150      6288.742  0.01412100  2055.832
    ##   4                  200      6337.110  0.01456865  2088.032
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of
    ##  0.1
    ## Tuning parameter 'n.minobsinnode' was held constant at
    ##  a value of 10
    ## RMSE was used to select the optimal model using the
    ##  smallest value.
    ## The final values used for the model were n.trees =
    ##  100, interaction.depth = 1, shrinkage = 0.1 and
    ##  n.minobsinnode = 10.

``` r
boostedpre <- predict(boostedTfit, newdata = test) 
boosted <- postResample(boostedpre, test$shares)
boosted
```

    ##         RMSE     Rsquared          MAE 
    ## 4.922914e+03 1.043557e-02 1.891857e+03

``` r
allRMSE <- tibble(lm1[1], lm2[1], rf[1], boosted[1])
names(allRMSE) <- c("LinearRegression1", "LinearRegression2", "RandomForest", "BoostedTree")
RMSElong <- allRMSE %>%
  pivot_longer(cols = 1:4, names_to = "Model", values_to = "RMSE")
RMSE_sort <- RMSElong %>% 
  arrange(RMSE)
RMSE_sort[1,]
```

    ## # A tibble: 1 × 2
    ##   Model              RMSE
    ##   <chr>             <dbl>
    ## 1 LinearRegression2 4902.

The result is the best model and its RMSE.

# Automation

``` r
channels <- c("data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world")
# Create file names
name <- c("Lifestyle", "Entertainment", "Business", "SocialMedia",
          "Tech", "World")
output_file <- paste0(name, "Analysis.md")
# Create a list for each channel with just channel name parameter
params <- lapply(channels, FUN = function(x){
  list(Channels = x)
})
# Put into a data frame
reports <- tibble::tibble(output_file, params)
apply(reports, MARGIN = 1, FUN = function(x) {
  rmarkdown::render(input = "ST558_Project2_Group10.Rmd", 
                    output_format = "github_document", 
                    output_file = x[[1]], 
                    params = x[[2]], 
                    output_options = list(html_preview = FALSE)) 
})
```
