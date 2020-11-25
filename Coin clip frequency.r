###
#title: Check the frequency of coin flip changes from heads and tails
#       Replicate by 10,000 times.
#       Find how many outliers in the left and right of the true value
#       with 95% confidence interval
#author: "Mousaidna Rosario"
###

# create a sample flip with a size of 50

fl <- sample(0:1,size=50, replace = TRUE)

# replace=TRUE means if you want data repeated.

fl
#initialize value for the loop
k <- length(fl) # get value of fl[n]
i <- 1 #var to get the current status when flipping
f <- 0 #initialize count status change
cnt <- 0

# start a loop 
flip.Cnt <- function(h){ 
  while(i <= k)
  {
    if (f!=h[i]) # f-1,h=1
    {
      # get the value of f for next comparison
      f = h[i] 
      #count changes
      cnt = 1 + cnt
    }
    # counter
    i = 1 + i
  }
  return(cnt)
}

# run function
nf <- flipCnt(fl)
nf # no of flip changes in 50 flips

# Simulate this 10,000 times and get the outliers from left and right of the histogram

NoOfFluctuation <- replicate(10000, {
  sampD <- sample(0:1,size=50, replace = TRUE)
  #put the sample in table form for histogram
  flip.Cnt(sampD)
}
)

hist(NoOfFluctuation, col = "red",ylim = c(0, 2500), 
     xlab = "Flip Coin Change Frequency", ylab="Frequency" ,
     main = paste("Histogram of 10000 Coin Flip changes frequency"))

abline(v=quantile(NoOfFluctuation, c(0.025, 0.975)), lwd=2, col='blue', lty=2)
abline(v=mean(NoOfFluctuation), lwd=2, col='yellow', lty=2)

quantile(NoOfFluctuation, c(0.025, 0.975)) # 95% confidence interval

length(NoOfFluctuation[NoOfFluctuation < 18]) # outliers count in the left, < 0.05 
length(NoOfFluctuation[NoOfFluctuation > 32]) # outliers count in the right, > 0.95
