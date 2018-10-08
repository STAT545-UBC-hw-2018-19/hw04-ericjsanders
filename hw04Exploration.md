Stat545 Homework 4
================
Eric Sanders
Submitted for 2018-10-09





















In this document we present some data exploration completed using the `gapminder` dataset. We will use the packages `tidyverse`, `gapminder`, and `ggplot2`.

``` r
library(tidyverse)
library(ggplot2)
library(gapminder)
```

The sections of this document are as follows:

-   Loading in and Checking the Data
-   Reshaping Prompt: Computing summaries of life expectancy for all combinations of year and continent in the `gapminder` dataset.
-   Joining Prompt: Creating a second data set using `gapminder` that

Loading in Data and Checking its Structure
==========================================

First, we load the data

``` r
# Call to preloaded data frame
data(gapminder)
attach(gapminder)
```

Next, we can look at some information on how this data set is organized.

``` r
# Determine what we can about the object flchain
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

And we can now see that `gapminder` is of class 'tbl\_df' with 1704 rows and 6 columns. We have two variables that come defined as factors, `country` and `continent`, and the other 4 variables come defined as either numeric or integer objects.

We can look at what the variables in our data set represent.

-   **country**: The country being examined.
-   **continent**: The continent that the country lies in.
-   **year**: The year under observation.
-   **lifeExp**: The life expectancy of the given country in the given year.
-   **pop**: The population of the country in the given year.
-   **gdpPercap**: The GDP per capita of the country in the given year.

Reshaping Prompt (Selected \#3)
===============================

We want first to calculate for each continent, for each year, some measure of life expectancy. We decide to use 5 summarizing statistics, the minimum, 1st quartile, median, third quartile, and maximum. Let us first create a new data frame to store this information in, using the `dplyr` functions `summarize` and `group_by`.

``` r
sum.lifeExp = gapminder %>%
  group_by(continent,year) %>%
  summarize(min=min(lifeExp),quart1=quantile(lifeExp)[[2]],med=median(lifeExp),quart3=quantile(lifeExp)[[4]],max=max(lifeExp))

head(sum.lifeExp)
```

    ## # A tibble: 6 x 7
    ## # Groups:   continent [1]
    ##   continent  year   min quart1   med quart3   max
    ##   <fct>     <int> <dbl>  <dbl> <dbl>  <dbl> <dbl>
    ## 1 Africa     1952  30     35.8  38.8   42.1  52.7
    ## 2 Africa     1957  31.6   37.4  40.6   44.8  58.1
    ## 3 Africa     1962  32.8   39.5  42.6   47.8  60.2
    ## 4 Africa     1967  34.1   41.4  44.7   49.5  61.6
    ## 5 Africa     1972  35.4   43.3  47.0   51.5  64.3
    ## 6 Africa     1977  36.8   44.5  49.3   53.9  67.1

And we see that we appear to have properly calculated the five summary statistics of life expectancy for each continent and year combination. The next step would be easier if we had only one summary statistic -- we could make a column for each continent, and for each row (year) we would have the summary statistic.

I decided to challenge myself to see how to most efficiently use a data set like this to create multiple data sets, where each data set contains one summary statistic in the desired format. In other words, instead of simply `spread`ing one summary statistic across continents, I wanted to `spread` by multiple different summary statistics one by one.

After testing out some approaches, I decided to loop through the 5 summary statistics and alternate piping the summary data set into `select` and then into `spread`. The most efficient R code I created is as follows.

``` r
sum.names = c('min','quart1','med','quart3','max')

for(i in 1:length(sum.names)){
  name = sum.names[i]
  
  sum.lifeExp %>%
    select(continent,year,paste(name)) %>% # Only take the summary statistic for this step of the 'for' loop
    spread(key=continent,value=paste(name)) %>% # Make there be a column for each continent
    assign(paste('sum.',name,sep=''),.,envir=.GlobalEnv) # The assign function is used here to declare a new global variable with a name that is pasted and varies with the loop. This is useful when you want to declare a new object with a new name for every iteration of a 'for' loop.
}
```

Inside the `for` loop, for each type of summary statistic, a new data frame was created that has a column for year, and a column for each continent containing the summary statistic in question. The 5 created data frames have the names `sum.min`, `sum.quart1`, `sum.med`, `sum.quart3`, and `sum.max`. We can examine each in turn,

``` r
# Look at minimum life expectancies
knitr::kable(sum.min,caption='Minimum Life Expectancy',format = 'pandoc')
```

|  year|  Africa|  Americas|  Asia|  Europe|  Oceania|
|-----:|-------:|---------:|-----:|-------:|--------:|
|  1952|      30|        38|    29|      44|       69|
|  1957|      32|        41|    30|      48|       70|
|  1962|      33|        43|    32|      52|       71|
|  1967|      34|        45|    34|      54|       71|
|  1972|      35|        47|    36|      57|       72|
|  1977|      37|        50|    31|      60|       72|
|  1982|      38|        51|    40|      61|       74|
|  1987|      40|        54|    41|      63|       74|
|  1992|      24|        55|    42|      66|       76|
|  1997|      36|        57|    42|      69|       78|
|  2002|      39|        58|    42|      71|       79|
|  2007|      40|        61|    44|      72|       80|

``` r
# Look at first quartile life expectancies
knitr::kable(sum.quart1)
```

|  year|  Africa|  Americas|  Asia|  Europe|  Oceania|
|-----:|-------:|---------:|-----:|-------:|--------:|
|  1952|      36|        45|    39|      61|       69|
|  1957|      37|        49|    42|      65|       70|
|  1962|      39|        52|    44|      67|       71|
|  1967|      41|        56|    48|      69|       71|
|  1972|      43|        58|    52|      70|       72|
|  1977|      45|        58|    55|      70|       73|
|  1982|      46|        61|    57|      71|       74|
|  1987|      47|        64|    60|      71|       75|
|  1992|      48|        67|    61|      72|       77|
|  1997|      47|        69|    62|      73|       78|
|  2002|      46|        71|    64|      74|       79|
|  2007|      48|        72|    65|      75|       80|

``` r
# Look at median life expectancies
knitr::kable(sum.med)
```

|  year|  Africa|  Americas|  Asia|  Europe|  Oceania|
|-----:|-------:|---------:|-----:|-------:|--------:|
|  1952|      39|        55|    45|      66|       69|
|  1957|      41|        56|    48|      68|       70|
|  1962|      43|        58|    49|      70|       71|
|  1967|      45|        61|    54|      71|       71|
|  1972|      47|        63|    57|      71|       72|
|  1977|      49|        66|    61|      72|       73|
|  1982|      51|        67|    64|      73|       74|
|  1987|      52|        70|    66|      75|       75|
|  1992|      52|        70|    69|      75|       77|
|  1997|      53|        72|    70|      76|       78|
|  2002|      51|        72|    71|      78|       80|
|  2007|      53|        73|    72|      79|       81|

``` r
# Look at third quartile life expectancies
knitr::kable(sum.quart3)
```

|  year|  Africa|  Americas|  Asia|  Europe|  Oceania|
|-----:|-------:|---------:|-----:|-------:|--------:|
|  1952|      42|        59|    51|      68|       69|
|  1957|      45|        63|    54|      69|       70|
|  1962|      48|        65|    57|      70|       71|
|  1967|      50|        66|    60|      71|       71|
|  1972|      52|        68|    64|      72|       72|
|  1977|      54|        69|    66|      74|       73|
|  1982|      57|        71|    69|      75|       75|
|  1987|      59|        72|    70|      76|       76|
|  1992|      60|        73|    71|      77|       77|
|  1997|      59|        74|    72|      78|       79|
|  2002|      58|        75|    74|      79|       80|
|  2007|      59|        76|    76|      80|       81|

``` r
# Look at maximum life expectancies
knitr::kable(sum.max)
```

|  year|  Africa|  Americas|  Asia|  Europe|  Oceania|
|-----:|-------:|---------:|-----:|-------:|--------:|
|  1952|      53|        69|    65|      73|       69|
|  1957|      58|        70|    68|      73|       70|
|  1962|      60|        71|    69|      74|       71|
|  1967|      62|        72|    71|      74|       72|
|  1972|      64|        73|    73|      75|       72|
|  1977|      67|        74|    75|      76|       73|
|  1982|      70|        76|    77|      77|       75|
|  1987|      72|        77|    79|      77|       76|
|  1992|      74|        78|    79|      79|       78|
|  1997|      75|        79|    81|      79|       79|
|  2002|      76|        80|    82|      81|       80|
|  2007|      76|        81|    83|      82|       81|

If I wanted to form a plot from these values, I would say the data are **not** in a convenient form to do so. It is most convenient for `ggplot2` and base R to have an output value in one column, and factors in another column, instead of having different columns represent different levels of one factor. For example, if I wanted to plot life expectancy summaries over time in each continent, it is very simple to do with the data frame I created before reshaping, `sum.lifeExp`, as follows.

``` r
ggplot(sum.lifeExp,aes(x=year))+
  geom_line(aes(y=min),linetype='dashed')+ # Add dashed lines for minimum and maximum life expectancy in the year given
  geom_line(aes(y=max),linetype='dashed')+
  geom_ribbon(aes(ymin=quart1,ymax=quart3,fill=continent))+ # Add shaded area to mark interquartile area of life expectancy in the year given
  geom_line(aes(y=med),size=2)+ # Add thick line for median life expectancy in year given
  facet_wrap(~continent)+
  ylab('Life Expectancy')+
  xlab('Year')+
  theme(legend.position='none')
```

![](hw04Exploration_files/figure-markdown_github/unnamed-chunk-7-1.png)

The new tables are no good at creating similar plots, and would only be useful if you wanted to make a plot for a single continent at a time, not for making plots for multiple continents.
