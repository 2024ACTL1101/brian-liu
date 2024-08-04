---
title: "ACTL1101 Assignment Part B"
author: "Brian Liu"
date: "2024 T2"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
# calculate the daily returns for each row, for both stocks, using the given formula
df <- df %>%
mutate(Daily_return_AMD = (AMD - lag(AMD)) / lag(AMD), 
       Daily_return_GSPC = (GSPC - lag(GSPC)) / lag(GSPC))
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
# calculates the daily risk-free rate for each row using the annual rate which is given
df <- df %>%
mutate(Daily_RF = (1 + RF/100)^(1/360) - 1)
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
# Calculates the excess return on each row using the daily return 
# and daily risk-free rate previously calcuated 
df <- df %>%
mutate(Excess_return_AMD = Daily_return_AMD - Daily_RF, 
       Excess_return_GSPC = Daily_return_GSPC - Daily_RF)
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
# When analysing data points we want to omit any NA data points 
df <- na.omit(df)
linear_regression_model = lm(Excess_return_AMD ~ Excess_return_GSPC, data = df)
summary(linear_regression_model)
beta <- linear_regression_model$coefficients[2]
paste("hence the coeffecient beta is", beta)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
As seen from the above regression analysis, the estimated coeffecient, beta
is found to be 1.5699987. 

Hence, since beta, which is equal to 1.5699987, is greater than 1, AMD is more
volatile than the market. This is because on average, for every 1% change in the
market's excess return, the stock's excess return changes by approximately
1.57% and is hence more sensitive to market movements, and is thus 
more volatile. This generally means that the AMD stock has a higher risk 
than the market average however as it is more volatile, however this may imply 
there is a possibility more greater returns.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# The x-axis should be snp500 while y-axis is AMD 
ggplot(aes(x = Excess_return_GSPC, y = Excess_return_AMD), data = df) +
geom_point() +
geom_smooth(method = "lm", se = FALSE) +
labs(title = "AMD vs. S&P 500 excess returns",
       x = "Excess Return of S&P 500",
       y = "Excess Return of AMD") +
theme(plot.title = element_text(hjust = 0.5))
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return. 

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
# Calculating the standard error for each day 
daily_se <- sqrt(mean(linear_regression_model$residuals^2))

# Calculate the annual standard error using the given assumption 
annual_se <- daily_se * sqrt(252)

# Expected market return
expected_market_return <- 0.133

# Inputting values into capm model or formula
expected_amd_return <- 0.05 + beta * (expected_market_return - 0.05)

# implementing a 90% prediction interval, 5% on each tail 
z_score <- qnorm(0.95)
lower_bound <- expected_amd_return - z_score * annual_se
upper_bound <- expected_amd_return + z_score * annual_se

# output the calculated values 

paste("The calculated lower bound is", lower_bound)
paste("The calculated upper bound is", upper_bound)

paste("Thus, the 90% prediction interval for AMD's annual expected return is:")

paste("Between",round(lower_bound * 100, 2), "% and", round(upper_bound * 100, 2), "%")
```

