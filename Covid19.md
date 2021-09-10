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

    ## Error: 'covid19.csv' does not exist in current working directory ('/Users/eriztolay/Desktop/R/CryptoCyp-Investigating-Covid-19-Virus-Trends').

``` r
# Determining the dimensions
dim(covid_df)
```

    ## Error in eval(expr, envir, enclos): object 'covid_df' not found

``` r
# Assign column names to a variable and display
vector_cols <- colnames(covid_df)
```

    ## Error in is.data.frame(x): object 'covid_df' not found

``` r
vector_cols
```

    ## Error in eval(expr, envir, enclos): object 'vector_cols' not found

``` r
# Display fist 5 rows
head(covid_df)
```

    ## Error in head(covid_df): object 'covid_df' not found

``` r
# Display summary of the dataset
glimpse(covid_df)
```

    ## Error in glimpse(covid_df): object 'covid_df' not found

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
```

    ## Error in filter(., Province_State == "All States"): object 'covid_df' not found

``` r
# Dropping Province_State column
covid_df <- covid_df[,-5]
```

    ## Error in eval(expr, envir, enclos): object 'covid_df' not found

Our analysis would be biased if we made the mistake of comparing a
column containing cumulative data and another one containing only
one-day data.

Thereafter, we work mainly with daily data. So we will extract the
columns related to the daily measures.

``` r
covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)
```

    ## Error in select(., Date, Country_Region, active, hospitalizedCurr, daily_tested, : object 'covid_df_all_states' not found

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
```

    ## Error in group_by(., Country_Region): object 'covid_df_all_states_daily' not found

``` r
covid_df_all_states_daily_sum
```

    ## Error in eval(expr, envir, enclos): object 'covid_df_all_states_daily_sum' not found

``` r
covid_top_10 <- head(covid_df_all_states_daily_sum,10)
```

    ## Error in head(covid_df_all_states_daily_sum, 10): object 'covid_df_all_states_daily_sum' not found

``` r
covid_top_10
```

    ## Error in eval(expr, envir, enclos): object 'covid_top_10' not found

Our goal now is to answer this question: **Which countries have had the
highest number of positive cases against the number of tests?**

``` r
countries <- covid_top_10$Country_Region
```

    ## Error in eval(expr, envir, enclos): object 'covid_top_10' not found

``` r
tested_cases<- covid_top_10$tested
```

    ## Error in eval(expr, envir, enclos): object 'covid_top_10' not found

``` r
positive_cases <- covid_top_10$positive
```

    ## Error in eval(expr, envir, enclos): object 'covid_top_10' not found

``` r
active_cases <- covid_top_10$active
```

    ## Error in eval(expr, envir, enclos): object 'covid_top_10' not found

``` r
hospitalized_cases <- covid_top_10$hospitalized
```

    ## Error in eval(expr, envir, enclos): object 'covid_top_10' not found

``` r
names(tested_cases) <- countries
```

    ## Error in eval(expr, envir, enclos): object 'countries' not found

``` r
names(positive_cases) <- countries
```

    ## Error in eval(expr, envir, enclos): object 'countries' not found

``` r
names(active_cases) <- countries
```

    ## Error in eval(expr, envir, enclos): object 'countries' not found

``` r
names(hospitalized_cases) <- countries
```

    ## Error in eval(expr, envir, enclos): object 'countries' not found

``` r
positive_cases / tested_cases
```

    ## Error in eval(expr, envir, enclos): object 'positive_cases' not found

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
```

    ## Error in eval(expr, envir, enclos): object 'covid_df' not found

``` r
matrices <- list(covid_mat)
vectors <- list(vector_cols, countries)
```

    ## Error in eval(expr, envir, enclos): object 'vector_cols' not found

``` r
data_structure_list <- list("dataframe" = datasets, "matrix" = matrices, "vector" = vectors)
```

    ## Error in eval(expr, envir, enclos): object 'datasets' not found

``` r
covid_analysis_list <- list(question, answer, data_structure_list)
```

    ## Error in eval(expr, envir, enclos): object 'data_structure_list' not found

``` r
covid_analysis_list[[2]]
```

    ## Error in eval(expr, envir, enclos): object 'covid_analysis_list' not found
