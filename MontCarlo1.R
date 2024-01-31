#install.packages("quantmod")

library(quantmod)
library(data.table)
library(ggplot2)

getSymbols("AAPL")
getSymbols("MSFT")
getSymbols("AMZN")
getSymbols("NVDA")
getSymbols("GOOGL")
getSymbols("TSLA")
getSymbols("META")
getSymbols("GOOG")

head(AAPL)
plot(AAPL$AAPL.Open)

# give return of apple over everyday
# make portfolio equally weighted to top 8 stocks of vanguard 500 etf
AAPL_return <- AAPL$AAPL.Close/ lag(AAPL$AAPL.Close)
AAPL_return <- as.numeric(AAPL_return)
AAPL_return[1] <- 1

MSFT_return <- MSFT$MSFT.Close/ lag(MSFT$MSFT.Close)
MSFT_return <- as.numeric(MSFT_return)
MSFT_return[1] <- 1

AMZN_return <- AMZN$AMZN.Close/ lag(AMZN$AMZN.Close)
AMZN_return <- as.numeric(AMZN_return)
AMZN_return[1] <- 1

NVDA_return <- NVDA$NVDA.Close/ lag(NVDA$NVDA.Close)
NVDA_return <- as.numeric(NVDA_return)
NVDA_return[1] <- 1

GOOGL_return <- GOOGL$GOOGL.Close/ lag(GOOGL$GOOGL.Close)
GOOGL_return <- as.numeric(MSFT_return)
GOOGL_return[1] <- 1

TSLA_return <- TSLA$TSLA.Close/ lag(TSLA$TSLA.Close)
TSLA_return <- as.numeric(TSLA_return)
TSLA_return[1] <- 1

META_return <- META$META.Close/ lag(META$META.Close)
META_return <- as.numeric(META_return)
META_return[1] <- 1

GOOG_return <- GOOG$GOOG.Close/ lag(GOOG$GOOG.Close)
GOOG_return <- as.numeric(GOOG_return)
GOOG_return[1] <- 1

ret <- (MSFT_return + AAPL_return + AMZN_return + NVDA_return + GOOGL_return 
        + TSLA_return + META_return + GOOG_return)/ 8


#monte carlo simul
# we use replicate function to run 1000 days simulation 100 time 
set.seed(0)
ndays <- 365
paths <- replicate(n = 200,
                   expr = sample(ret, ndays, replace = TRUE ))
paths <- apply(paths, 2, cumprod)

#prepare data for plotting
paths <- data.table(paths)
paths$days <- 1:nrow(paths)

# turn data from wide into long format, ggplot rly likes long format 

paths <- melt(paths, id.vars = "days")

#plot
ggplot(paths, aes(x = days, y = (value-1) *100, col = variable)) +
  geom_line() + 
  theme_bw() +
  # if you have lots of variable this ensures there is not legend value for
  # each variable 
  theme(legend.position = "none") +
  xlab("Days Invested") + ylab("portfolio Return %")


summary((paths$value[paths$days == ndays] -1) * 100)
mean((paths$values[paths$days == ndays]-1) * 100 < 0)

#look at ret 
view(ret)


