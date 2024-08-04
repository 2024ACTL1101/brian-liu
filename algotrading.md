---
title: "ACTL1101 Assignment Part A"
author: "Brian Liu"
date: "2024 T2"
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1.  **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2.  **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3.  **Customize Trading Period:** Choose your entry and exit dates.

4.  **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5.  **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI.

6.  **Discussion:** Summarise your finding.

## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates.

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```

##Plotting the Data Plot the closing prices over time to visualize the price movement.

```{r plot}
plot(amd_df$date, amd_df$close,'l')
```

## Step 3: Customize Trading Period

-   Define a trading period you wanted in the past five years

```{r period}
## Define start and end date of the data frame 
start_date <- as.Date('2022-01-01')
end_date <- as.Date('2023-01-01')

## update the dataframe to its subset which only includes dates between the set time period
amd_df <- subset(amd_df, date >= start_date & date <= end_date)
```

## Step 2: Trading Algorithm

Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

-   Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
-   Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
    -   If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
    -   Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
    -   You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
    -   If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.

```{r trading}

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# for loop for all rows in the dataframe
for (row in 1:nrow(amd_df)) {
    # set the current_price to the stock price of the current row
    current_price <- amd_df$close[row]
    
    # Buy condition when previous price is 0 or when current price 
    # is less than previous price 
    if (previous_price == 0 || current_price < previous_price) {
      #update row variables accordingly to buy
      amd_df$trade_type[row] <- 'buy'
      amd_df$costs_proceeds[row] <- -current_price * share_size
      accumulated_shares <- accumulated_shares + share_size
    } 
  
    # below occurs when we are at the last row, and we sell
    if (row == nrow(amd_df)) {
      #update row variables accordingly to sell
      amd_df$trade_type[nrow(amd_df)] <- 'sell'
      amd_df$costs_proceeds[row] <- current_price * accumulated_shares
      #after selling all shares the accumulated shares is 0 
      accumulated_shares <- 0
    }
  
  #update the previous price and the accumulated shares column for each row 
  amd_df$accumulated_shares[row] <- accumulated_shares
  previous_price <- amd_df$close[row]
}


```

## Step 4: Run Your Algorithm and Analyze Results

After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

-   Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
-   Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
-   ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# The PNL is given by all values in the costs proceeds column added together
# i.e the money recieved from selling (sells) - the cost of purchasing the stocks (buys)
total_profit_loss_1 <- sum(amd_df$costs_proceeds, na.rm = TRUE)
# The total invested capital is the costs proceeds column added together which have the buy condition
# i.e. the cost of purchasing the stocks (buys)
total_invested_capital_1 <- sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
# total invested capital should be the absolute value as you msut invest a positive amount of cash 
total_invested_capital_1 <- abs(total_invested_capital_1)
# implementing the ROI formula
ROI_1 <- (total_profit_loss_1/total_invested_capital_1) * 100

# Below displays the calculated invested capital, PNL and ROI respectively for strategy 1:

total_invested_capital_1
total_profit_loss_1
ROI_1
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)

-   Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
-   Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

```{r option}
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking
amd_df$accumulated_purchase <- 0 # The accumulated invested capital
amd_df$average_purchase_price <- 0 # Average purchase price used for sell mechanism 

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
accumulated_purchase <- 0 # The accumulated invested capital
average_purchase_price <- 0 # Average purchase price used for sell mechanism 

# for loop for all rows in the dataframe
for (row in 1:nrow(amd_df)) {
    # set the current_price to the stock price of the current row
    current_price <- amd_df$close[row]
    
    if (current_price < average_purchase_price * 0.9) {
      # Sell condition when current price fallse more than 10% below the average purchase price 
      amd_df$trade_type[row] <- 'sell'
      # Sell half holdings of stock
      amd_df$costs_proceeds[row] <- current_price * (accumulated_shares / 2)
      accumulated_shares <- accumulated_shares / 2 
      # Half accumulated purchase accordingly to weight correctly so 
      # average purchase price remains unaffected after selling 
      accumulated_purchase = accumulated_purchase / 2
    } else if (previous_price == 0 || current_price < previous_price) {
      # Buy condition when previous price is 0 or when current price 
      # is less than previous price, note this is overrided by sell mechanism 
      amd_df$trade_type[row] <- 'buy'
      amd_df$costs_proceeds[row] <- -current_price * share_size
      accumulated_shares <- accumulated_shares + share_size
      # update accumulated purchase value after each buy operation
      accumulated_purchase = accumulated_purchase + current_price * share_size
      # the average purchase price is given by the below expression 
      average_purchase_price = accumulated_purchase / accumulated_shares
    }
  
    # below occurs when we are at the last row, and we sell
    if (row == nrow(amd_df)) {
      #update row variables accordingly to sell all shares
      amd_df$trade_type[nrow(amd_df)] <- 'sell'
      amd_df$costs_proceeds[row] <- current_price * accumulated_shares
      #after selling all shares the accumulated shares is 0 
      accumulated_shares <- 0 
    }
  
  # update previous price, and all variables for each column in every row 
  amd_df$accumulated_purchase[row] <- accumulated_purchase
  amd_df$average_purchase_price[row] <- average_purchase_price
  amd_df$accumulated_shares[row] <- accumulated_shares
  previous_price <- amd_df$close[row]
}
```

## Step 6: Summarize Your Findings

-   Did your P/L and ROI improve over your chosen period?
-   Relate your results to a relevant market event and explain why these outcomes may have occurred.

```{r}
# The PNL is given by all values in the costs proceeds column added together
# i.e the money recieved from selling (sells) - the cost of purchasing the stocks (buys)
total_profit_loss_2 <- sum(amd_df$costs_proceeds, na.rm = TRUE)
# The total invested capital is the costs proceeds column added together which have the buy condition
# i.e. the cost of purchasing the stocks (buys)
total_invested_capital_2 <- sum(amd_df$costs_proceeds[amd_df$trade_type == 'buy'], na.rm = TRUE)
# total invested capital should be the absolute value as you msut invest a positive amount of cash 
total_invested_capital_2 <- abs(total_invested_capital_2)
# implementing the ROI formula
ROI_2 <- (total_profit_loss_2/total_invested_capital_2) * 100

# Below displays the calculated invested capital, PNL and ROI respectively for strategy 1:

total_invested_capital_1
total_profit_loss_1
ROI_1
 
# Below displays the calculated invested capital, PNL and ROI respectively for strategy 2:

total_invested_capital_2
total_profit_loss_2
ROI_2

#Discussion:

#The chosen period was the year 2022, Jan 1 to Dec 31.

#The PNL of the first strategy (in step 2) was -335456.04 dollars, for the second strategy 
#(in step 5) it was -69805.49 dollars. Hence strategy 2 earned 265,650.55 dollars
#more (or lost 265,650.55 dollars less) than strategy 1 in this time period, 
#therefore providing a better PNL and hence a better ROI.

#The ROI of the first strategy was -27% while for the second strategy it was -13%.
#Therefore, strategy 2 had a 14% higher ROI than strategy 2, and overall was the more
#effective strategy during this time period.

#This time period, 2022, was plagued with multiple market events. On a macroeconomic 
#level, a key market event was the rise in interest rates. As interest rates rose,
#investors were less incentivised to put money into riskier investments like AMD, 
#resulting in a fall of AMD stock. The fall in the AMD stock price during the 2022 time
#period, as seen in the below plot, can hence be explained by the rising interest rates of 2022.

# a plot for the stock price over the set time period only
plot(amd_df$date, amd_df$close,'l')

#Overall, the decrease in value of the AMD stock during 2022, meant that during this time 
# period both strategies would have a negative PNL and ROI. However, because strategy 2
#included a stop-loss mechanism it heavily reduced the total loss, as it would sell half 
#of all owned stocks as soon as the stock went down by even 10% below the average purchase 
#price, whereas strategy 1 would simply continually buy the stock as it went down, only selling 
#at the end for a larger loss. Overall, this meant that strategy 2 was 
#more effective in this period.
```
