Predicting Alcohol Consumption in England: Age, Sex, and Year as major
Influencers
================
Joshua Edefo
2025-02-16

The analysis uses a log linear model to predict mean alcohol consumption
based on age group, sex, and year using data from the Health Survey for
England (2011-2013). The model shows that women consume significantly
fewer alcohol units than men, with a difference of -0.60 log units (0.55
units). Older age groups, particularly those aged above 75 years,
consume significantly fewer alcohol units than the 16-24 age group, with
a difference of -0.45 log units ( 0.64 units) for the above 75 years
group. Other age groups, such as 45-54 years, show moderate reductions
in consumption compared to the reference group. Year does not
significantly affect alcohol consumption, suggesting minimal changes
over time. The analysis reveals that age and sex are important factors
influencing alcohol consumption, while year has little impact.

Libraries

``` r
# Load necessary libraries
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.2

    ## Warning: package 'ggplot2' was built under R version 4.3.3

    ## Warning: package 'dplyr' was built under R version 4.3.3

    ## Warning: package 'forcats' was built under R version 4.3.2

    ## Warning: package 'lubridate' was built under R version 4.3.3

``` r
#install.packages("car")
library(car)
```

    ## Warning: package 'car' was built under R version 4.3.3

    ## Warning: package 'carData' was built under R version 4.3.3

``` r
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.3.2

``` r
library(readxl)
```

Predicting the exposure to drinking alcohol based on age, sex and year

``` r
## Set directory
setwd("C:\\Users\\joe62\\OneDrive - Aberystwyth University\\Apps\\Desktop\\Destop Folder\\R code\\Extract from Health Survey for England")

# Extract from Health Survey for England (HSE- 2022-Adult-drinking-table)

## Importing the data
data <- read_excel ("HSE-2022-Adult-drinking-tables_extract.xlsx")
head(data)
```

    ## # A tibble: 6 × 5
    ##   age_group   sex   year_2011 year_2012 year_2013
    ##   <chr>       <chr>     <dbl>     <dbl>     <dbl>
    ## 1 16-24 years Men        15.3      17.2      15.3
    ## 2 25-34 years Men        16.5      14.9      15  
    ## 3 35-44 years Men        17.9      16.5      16.2
    ## 4 45-54 years Men        19.1      18.3      19.3
    ## 5 55-64 years Men        19.4      21.1      20.8
    ## 6 65-74 years Men        15.7      16.9      15.4

``` r
str(data)
```

    ## tibble [21 × 5] (S3: tbl_df/tbl/data.frame)
    ##  $ age_group: chr [1:21] "16-24 years" "25-34 years" "35-44 years" "45-54 years" ...
    ##  $ sex      : chr [1:21] "Men" "Men" "Men" "Men" ...
    ##  $ year_2011: num [1:21] 15.3 16.5 17.9 19.1 19.4 15.7 13.6 10.4 7.7 10.8 ...
    ##  $ year_2012: num [1:21] 17.2 14.9 16.5 18.3 21.1 16.9 10.7 10.3 9.5 12 ...
    ##  $ year_2013: num [1:21] 15.3 15 16.2 19.3 20.8 15.4 11.1 9.3 8.4 9.3 ...

``` r
view(data)


## Reshape data to long format for analysis
data_long <- data %>%
  gather(key = "year", value = "mean_alcohol_units", year_2011, year_2012, year_2013)

## Create a variable for the age group and sex
data_long$age_group <- factor(data_long$age_group, levels = unique(data_long$age_group))
data_long$sex <- factor(data_long$sex, levels = c("Men", "Women", "All adults"))
data_long$year <- as.factor(data_long$year)

model <- lm(log(mean_alcohol_units) ~ age_group + sex + year, data = data_long)

## Summary of the model
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = log(mean_alcohol_units) ~ age_group + sex + year, 
    ##     data = data_long)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.163202 -0.083802  0.000814  0.065678  0.223498 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           2.84337    0.04961  57.311  < 2e-16 ***
    ## age_group25-34 years -0.09518    0.06076  -1.566 0.123328    
    ## age_group35-44 years  0.06051    0.06076   0.996 0.323940    
    ## age_group45-54 years  0.13652    0.06076   2.247 0.028926 *  
    ## age_group55-64 years  0.10467    0.06076   1.723 0.090910 .  
    ## age_group65-74 years -0.09052    0.06076  -1.490 0.142340    
    ## age_group75+ years   -0.45368    0.06076  -7.466 8.94e-10 ***
    ## age_groupAll adults  -0.20085    0.05135  -3.911 0.000268 ***
    ## sexWomen             -0.60499    0.03248 -18.627  < 2e-16 ***
    ## sexAll adults              NA         NA      NA       NA    
    ## yearyear_2012         0.03028    0.03248   0.932 0.355478    
    ## yearyear_2013        -0.05061    0.03248  -1.558 0.125252    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1052 on 52 degrees of freedom
    ## Multiple R-squared:  0.9078, Adjusted R-squared:   0.89 
    ## F-statistic: 51.17 on 10 and 52 DF,  p-value: < 2.2e-16

``` r
## Plot the results 
ggplot(data_long, aes(x = age_group, y = log(mean_alcohol_units), color = sex, group = interaction(sex, year))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Log-Transformed Alcohol Consumption by Age, Sex, and Year", 
       x = "Age Group", y = "Log of Mean Alcohol Units")
```

![](Predict_files/figure-gfm/b-1.png)<!-- -->

ession information

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] readxl_1.4.3    usethis_2.2.2   car_3.1-3       carData_3.0-5  
    ##  [5] lubridate_1.9.3 forcats_1.0.0   stringr_1.5.0   dplyr_1.1.4    
    ##  [9] purrr_1.0.2     readr_2.1.4     tidyr_1.3.0     tibble_3.2.1   
    ## [13] ggplot2_3.5.1   tidyverse_2.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] utf8_1.2.3        generics_0.1.3    stringi_1.7.12    hms_1.1.3        
    ##  [5] digest_0.6.33     magrittr_2.0.3    evaluate_0.21     grid_4.3.1       
    ##  [9] timechange_0.2.0  fastmap_1.2.0     cellranger_1.1.0  Formula_1.2-5    
    ## [13] fansi_1.0.4       scales_1.3.0      abind_1.4-5       cli_3.6.1        
    ## [17] rlang_1.1.1       munsell_0.5.0     withr_2.5.0       yaml_2.3.7       
    ## [21] tools_4.3.1       tzdb_0.4.0        colorspace_2.1-0  vctrs_0.6.5      
    ## [25] R6_2.5.1          lifecycle_1.0.3   fs_1.6.3          pkgconfig_2.0.3  
    ## [29] pillar_1.9.0      gtable_0.3.4      glue_1.6.2        xfun_0.40        
    ## [33] tidyselect_1.2.0  rstudioapi_0.15.0 knitr_1.44        farver_2.1.1     
    ## [37] htmltools_0.5.8.1 rmarkdown_2.25    labeling_0.4.3    compiler_4.3.1
