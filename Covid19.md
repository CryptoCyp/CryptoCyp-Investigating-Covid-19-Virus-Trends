Investigating COVID-19 Virus Trends
================
Eriz Tolay
9/10/2021

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

## COVID-19

A pneumonia of unknown cause detected in Wuhan, China was first
internationally reported from China on 31 December 2019. Today we know
this virus as Coronavirus. COVID-19 which stands for COronaVIrus Disease
is the disease caused by this virus. Since then, the world has been
engaged in the fight against this pandemic. Several measures have
therefore been taken to “flatten the curve”. We have consequently
experienced social distancing and many people have passed away as well.

Our analysis tries to provide an answer to this question: **Which
countries have had the highest number of positive cases against the
number of tests?**

``` r
# Read the data
covid_df <- read_csv('covid19.csv')
```

    ## Rows: 10903 Columns: 14

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (4): Continent_Name, Two_Letter_Country_Code, Country_Region, Province_...
    ## dbl  (9): positive, hospitalized, recovered, death, total_tested, active, ho...
    ## date (1): Date

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Determining the dimensions
dim(covid_df)
```

    ## [1] 10903    14

``` r
# Assign column names to a variable and display
vector_cols <- colnames(covid_df)
vector_cols
```

    ##  [1] "Date"                    "Continent_Name"         
    ##  [3] "Two_Letter_Country_Code" "Country_Region"         
    ##  [5] "Province_State"          "positive"               
    ##  [7] "hospitalized"            "recovered"              
    ##  [9] "death"                   "total_tested"           
    ## [11] "active"                  "hospitalizedCurr"       
    ## [13] "daily_tested"            "daily_positive"

``` r
# Display fist 5 rows
head(covid_df)
```

    ## # A tibble: 6 × 14
    ##   Date       Continent_Name Two_Letter_Country_Co… Country_Region Province_State
    ##   <date>     <chr>          <chr>                  <chr>          <chr>         
    ## 1 2020-01-20 Asia           KR                     South Korea    All States    
    ## 2 2020-01-22 North America  US                     United States  All States    
    ## 3 2020-01-22 North America  US                     United States  Washington    
    ## 4 2020-01-23 North America  US                     United States  All States    
    ## 5 2020-01-23 North America  US                     United States  Washington    
    ## 6 2020-01-24 Asia           KR                     South Korea    All States    
    ## # … with 9 more variables: positive <dbl>, hospitalized <dbl>, recovered <dbl>,
    ## #   death <dbl>, total_tested <dbl>, active <dbl>, hospitalizedCurr <dbl>,
    ## #   daily_tested <dbl>, daily_positive <dbl>

``` r
# Display summary of the dataset
glimpse(covid_df)
```

    ## Rows: 10,903
    ## Columns: 14
    ## $ Date                    <date> 2020-01-20, 2020-01-22, 2020-01-22, 2020-01-2…
    ## $ Continent_Name          <chr> "Asia", "North America", "North America", "Nor…
    ## $ Two_Letter_Country_Code <chr> "KR", "US", "US", "US", "US", "KR", "US", "US"…
    ## $ Country_Region          <chr> "South Korea", "United States", "United States…
    ## $ Province_State          <chr> "All States", "All States", "Washington", "All…
    ## $ positive                <dbl> 1, 1, 1, 1, 1, 2, 1, 1, 4, 0, 3, 0, 0, 0, 0, 1…
    ## $ hospitalized            <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ recovered               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ death                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ total_tested            <dbl> 4, 1, 1, 1, 1, 27, 1, 1, 0, 0, 0, 0, 0, 0, 0, …
    ## $ active                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ hospitalizedCurr        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ daily_tested            <dbl> 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ daily_positive          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

## Filtering and Extracting the Data

Looking at the few lines of our dataset we displayed in the previous
step, we can see that the Province\_State column mixes data from
different levels: country level and state/province level. Since we
cannot run an analysis on all these levels at the same time, we need to
filter what we are interested in.

We will, therefore, extract only the country-level data in order to not
bias our analyses.

``` r
# To filter for All States in Province_State column
covid_df_all_states <- covid_df %>%
  filter(
    Province_State == "All States"
  )
# Dropping Province_State column
covid_df <- covid_df[,-5]
```

Our analysis would be biased if we made the mistake of comparing a
column containing cumulative data and another one containing only
one-day data.

Thereafter, we work mainly with daily data. So we will extract the
columns related to the daily measures.

``` r
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)
```

We will summarize the covid\_df\_all\_states\_daily dataframe by
computing the overall number of tested, positive, active and
hospitalized cases. Then, we can arrange this aggregated data by the
overall number of tested cases. Finally, we can extract the first ten
rows as the top ten tested cases countries.

``` r
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarise(
    tested = sum(daily_tested),
    positive = sum(daily_positive),
    active = sum(active),
    hospitalized = sum(hospitalizedCurr)
  ) %>%
  arrange(-tested)
  
covid_df_all_states_daily_sum
```

    ## # A tibble: 108 × 5
    ##    Country_Region   tested positive  active hospitalized
    ##    <chr>             <dbl>    <dbl>   <dbl>        <dbl>
    ##  1 United States  17282363  1877179       0            0
    ##  2 Russia         10542266   406368 6924890            0
    ##  3 Italy           4091291   251710 6202214      1699003
    ##  4 India           3692851    60959       0            0
    ##  5 Turkey          2031192   163941 2980960            0
    ##  6 Canada          1654779    90873   56454            0
    ##  7 United Kingdom  1473672   166909       0            0
    ##  8 Australia       1252900     7200  134586         6655
    ##  9 Peru             976790    59497       0            0
    ## 10 Poland           928256    23987  538203            0
    ## # … with 98 more rows

``` r
covid_top_10 <- head(covid_df_all_states_daily_sum,10)
covid_top_10
```

    ## # A tibble: 10 × 5
    ##    Country_Region   tested positive  active hospitalized
    ##    <chr>             <dbl>    <dbl>   <dbl>        <dbl>
    ##  1 United States  17282363  1877179       0            0
    ##  2 Russia         10542266   406368 6924890            0
    ##  3 Italy           4091291   251710 6202214      1699003
    ##  4 India           3692851    60959       0            0
    ##  5 Turkey          2031192   163941 2980960            0
    ##  6 Canada          1654779    90873   56454            0
    ##  7 United Kingdom  1473672   166909       0            0
    ##  8 Australia       1252900     7200  134586         6655
    ##  9 Peru             976790    59497       0            0
    ## 10 Poland           928256    23987  538203            0

Our goal now is to answer this question: **Which countries have had the
highest number of positive cases against the number of tests?**

``` r
countries <- covid_top_10$Country_Region
tested_cases<- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

positive_cases / tested_cases
```

    ##  United States         Russia          Italy          India         Turkey 
    ##    0.108618191    0.038546552    0.061523368    0.016507300    0.080711720 
    ##         Canada United Kingdom      Australia           Peru         Poland 
    ##    0.054915490    0.113260617    0.005746668    0.060910738    0.025840932

``` r
positive_tested_top_3 <- c("United Kingdom" = 0.11, "United States" = 0.10, "Turkey" = 0.08)
positive_tested_top_3
```

    ## United Kingdom  United States         Turkey 
    ##           0.11           0.10           0.08

Our goal now is to find a way to keep all the information available for
the top three countries that have had the highest number of positive
cases against the number of tests.

To make sure we won’t lose other information about these countries we
can create a matrix that contains the ratio and the overall number of
COVID-19 tested, positive, active and hospitalized cases.

``` r
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

covid_mat <- rbind(united_kingdom, united_states, turkey)
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
covid_mat
```

    ##                Ratio   tested positive  active hospitalized
    ## united_kingdom  0.11  1473672   166909       0            0
    ## united_states   0.10 17282363  1877179       0            0
    ## turkey          0.08  2031192   163941 2980960            0

``` r
question <- "Which countries have had the highest number of positive cases against the number of tests?"

answer <- c("Positive tested cases" = positive_tested_top_3)
```

``` r
datasets <- list(
  original = covid_df,
  allstates = covid_df_all_states,
  daily = covid_df_all_states_daily,
  top_10 = covid_top_10
)
matrices <- list(covid_mat)
vectors <- list(vector_cols, countries)
data_structure_list <- list("dataframe" = datasets, "matrix" = matrices, "vector" = vectors)
covid_analysis_list <- list(question, answer, data_structure_list)
covid_analysis_list[[2]]
```

    ## Positive tested cases.United Kingdom  Positive tested cases.United States 
    ##                                 0.11                                 0.10 
    ##         Positive tested cases.Turkey 
    ##                                 0.08
