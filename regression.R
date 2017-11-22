download_data = function(url) {
  raw = read.table(url, header=TRUE, sep=",")
  raw = raw[, c(1, 7)] # 1 is Date column, 7 is Adj.Close column
  raw = raw[nrow(raw):1, ] # Sort oldest to newest
  return(raw)
}

daily_returns = function(data) {
  close_values = data[, 2] # 2 is Adj.Close column
  num = length(close_values) - 1
  returns = numeric(num)
  for (i in 1:num) {
    returns[i] = (close_values[i + 1] - close_values[i]) / close_values[i] * 100
  }
  return(returns)
}

#####################################################################################
# Step 1 Download data for last 1 year for the DJIA (Dow Jones Industrial Average) 
# and all its 30 constituent stocks. 
#####################################################################################
print("===== Download DJIA =====")
djia_url = "http://chart.finance.yahoo.com/table.csv?s=^DJI&a=8&b=8&c=2015&d=8&e=8&f=2016&g=d&ignore=.csv"
djia_data = download_data(djia_url)

print("===== Daily returns of DJIA =====")
djia_daily_returns = daily_returns(djia_data)
print(djia_daily_returns)

print("===== Download DJIA's 30 component stocks =====")
stocks = c("JPM", "JNJ", "UNH", "MCD", "PFE", "PG", "V", "MSFT", "MRK", "DIS", 
  "AXP", "NKE", "TRV", "GS", "WMT", "IBM", "DD", "CSCO", "HD", "XOM",
  "MMM", "AAPL", "INTC", "UTX", "GE", "CVX", "CAT", "KO", "BA", "VZ")

alpha_vector = c()
beta_vector = c()

for (stock in stocks) {
  url = paste("http://chart.finance.yahoo.com/table.csv?s=", stock, "&a=8&b=8&c=2015&d=8&e=8&f=2016&g=d&ignore=.csv", sep="")
  data = download_data(url)
  #####################################################################################
  # Step 2
  # Calculate daily returns of the DJIA index and the downloaded stocks over the period 
  # under study
  #####################################################################################
  print(paste("===== Daily Returns of ", stock, " =====", sep=""))
  returns = daily_returns(data)
  print(returns)

  #####################################################################################
  # Step 3
  # Considering the equation form provided above and matching the Index returns vs. the 
  # returns of one of its constituent stocks at a time, perform linear regression fits 
  # and calculate α(alpha) and β values for each stock
  #####################################################################################
  matching = data.frame(
    djia = djia_daily_returns,
    stock = returns
  )
  lm_result = lm(returns ~ djia_daily_returns, data = matching)
  alpha = coef(lm_result)["(Intercept)"]
  beta = coef(lm_result)["djia_daily_returns"]
  print(paste("===== Alpha of ", stock, " = ", alpha, " =====", sep=""))
  print(paste("===== Beta of ", stock, " = ", beta, " =====", sep=""))
  alpha_vector = c(alpha_vector, alpha)
  beta_vector = c(beta_vector, beta)
}

#####################################################################################
# Step 4 Graphically represent the distribution of α(alpha) and β values for the 
# constituents of DJIA
#####################################################################################
hist(alpha_vector, breaks=20, main="Distribution of Alpha")
hist(beta_vector, breaks=16, main="Distribution of Beta")