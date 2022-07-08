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

    ##                                  shares n_tokens_content
    ## shares                      1.000000000       0.07302425
    ## n_tokens_content            0.073024252       1.00000000
    ## num_imgs                    0.051201300       0.46442622
    ## num_videos                  0.088311059       0.04005714
    ## global_rate_positive_words -0.005395787       0.12779576
    ## global_subjectivity         0.017739883       0.09117454
    ##                               num_imgs    num_videos
    ## shares                      0.05120130  0.0883110588
    ## n_tokens_content            0.46442622  0.0400571423
    ## num_imgs                    1.00000000 -0.0550097529
    ## num_videos                 -0.05500975  1.0000000000
    ## global_rate_positive_words  0.06914830 -0.0000483498
    ## global_subjectivity         0.19925466  0.0254110044
    ##                            global_rate_positive_words
    ## shares                                  -0.0053957870
    ## n_tokens_content                         0.1277957592
    ## num_imgs                                 0.0691482967
    ## num_videos                              -0.0000483498
    ## global_rate_positive_words               1.0000000000
    ## global_subjectivity                      0.3833653985
    ##                            global_subjectivity
    ## shares                              0.01773988
    ## n_tokens_content                    0.09117454
    ## num_imgs                            0.19925466
    ## num_videos                          0.02541100
    ## global_rate_positive_words          0.38336540
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
    ## Min        28.000           0.0000   0.000000  0.0000000
    ## Mean     3535.421         634.7976   4.934783  0.4429348
    ## Median   1700.000         508.5000   1.000000  0.0000000
    ## Max    196700.000        8474.0000 111.000000 15.0000000
    ## Sd       8131.063         597.1858   8.281820  1.3419971
    ## IQR      2125.000         489.2500   7.000000  0.0000000
    ##        global_rate_positive_words global_subjectivity
    ## Min                    0.00000000          0.00000000
    ## Mean                   0.04470561          0.47396401
    ## Median                 0.04432815          0.47787444
    ## Max                    0.12138728          0.77777778
    ## Sd                     0.01517567          0.09191057
    ## IQR                    0.01870835          0.10199113

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
    ## 1186  286

From the contingency table, we can see how many articles are published
on weekday and weekend.

## Bar Plot

``` r
# Create bar plot of predictor "is_weekend"
g <- ggplot(data = train, aes(x = is_weekend))
g + geom_bar(fill = "Red", color = "Blue") +
  labs(title = "Bar Plot of is_weekend")
```

![](LifestyleAnalysis_files/figure-gfm/barplot-1.png)<!-- -->

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

![](LifestyleAnalysis_files/figure-gfm/histograms-1.png)<!-- -->

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

![](LifestyleAnalysis_files/figure-gfm/scatterplot-1.png)<!-- -->

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

    ## Start:  AIC=26400.8
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
    ## 1472 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (21), scaled (21) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 1179, 1177, 1176, 1177, 1179 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared     MAE     
    ##   8274.591  0.003626336  3323.125
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
lmpred1 <- predict(lmfit1, newdata = test) 
lm1 <- postResample(lmpred1, test$shares)
lm1
```

    ##         RMSE     Rsquared          MAE 
    ## 1.121763e+04 2.466283e-04 3.851382e+03

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
    ## 1472 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 1177, 1178, 1178, 1177, 1178 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared   MAE     
    ##   7707.162  0.0108539  3166.798
    ## 
    ## Tuning parameter 'intercept' was held constant at a value of TRUE

``` r
lmpred2 <- predict(lmfit2, newdata = test) 
lm2 <- postResample(lmpred2, test$shares)
lm2
```

    ##         RMSE     Rsquared          MAE 
    ## 1.089556e+04 2.754176e-04 3.690541e+03

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
    ## 1472 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 1178, 1178, 1178, 1177, 1177 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared     MAE     
    ##   1     7166.369  0.018446664  3030.324
    ##   2     7294.605  0.013125457  3158.129
    ##   3     7397.528  0.006885392  3203.126
    ##   4     7458.584  0.005101914  3235.539
    ##   5     7535.760  0.004688708  3262.605
    ##   6     7560.367  0.004318681  3278.198
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
    ## 1.092618e+04 8.974418e-04 3.742807e+03

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
    ## 1472 samples
    ##    6 predictor
    ## 
    ## Pre-processing: centered (6), scaled (6) 
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 1178, 1177, 1178, 1178, 1177 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE      Rsquared     MAE     
    ##   1                   25      7655.952  0.007241277  3120.906
    ##   1                   50      7686.595  0.007488185  3143.022
    ##   1                  100      7684.001  0.008571235  3118.175
    ##   1                  150      7650.802  0.008991964  3087.661
    ##   1                  200      7698.483  0.007824411  3128.217
    ##   2                   25      7686.215  0.009860349  3145.996
    ##   2                   50      7695.024  0.013333562  3188.685
    ##   2                  100      7792.925  0.012716309  3221.369
    ##   2                  150      7783.939  0.016777767  3224.684
    ##   2                  200      7841.793  0.018807243  3268.382
    ##   3                   25      7658.308  0.011443356  3140.642
    ##   3                   50      7713.388  0.011250838  3180.927
    ##   3                  100      7869.600  0.010431773  3233.186
    ##   3                  150      7839.001  0.016953242  3248.383
    ##   3                  200      7834.834  0.018907480  3277.814
    ##   4                   25      7629.122  0.014080331  3137.897
    ##   4                   50      7732.990  0.009155742  3203.752
    ##   4                  100      7925.713  0.010190683  3344.143
    ##   4                  150      7921.004  0.014911125  3383.256
    ##   4                  200      7852.893  0.020019426  3406.909
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of
    ##  0.1
    ## Tuning parameter 'n.minobsinnode' was held constant at
    ##  a value of 10
    ## RMSE was used to select the optimal model using the
    ##  smallest value.
    ## The final values used for the model were n.trees =
    ##  25, interaction.depth = 4, shrinkage = 0.1 and n.minobsinnode
    ##  = 10.

``` r
boostedpre <- predict(boostedTfit, newdata = test) 
boosted <- postResample(boostedpre, test$shares)
boosted
```

    ##         RMSE     Rsquared          MAE 
    ## 1.044403e+04 8.911805e-03 3.639445e+03

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
    ##   Model         RMSE
    ##   <chr>        <dbl>
    ## 1 BoostedTree 10444.

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
