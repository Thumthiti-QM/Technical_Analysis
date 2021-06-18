#################################
### FRE7241 Homework #1 due at 6AM Tuesday April 27th, 2021
#################################
# Max score 130pts


# Please write in this file the R code needed to perform 
# the tasks below, rename it to your_name_hw1.R,
# and upload the file to NYU Classes


############## Part I
# Summary: Calculate the maximum drawdown of a time series.

# 1. (20pts) 
# Extract the close prices from rutils::etf_env$VTI into 
# a variable called price_s.  price_s is an xts time series. 


# You can use the function Cl() from package quantmod.

library(rutils)
### write your code here
price_s <- quantmod::Cl(rutils::etf_env$VTI)

# You should get the following output:
class(price_s)
# [1] "xts" "zoo"
tail(price_s)
#             VTI.Close
# 2021-03-19  204.5837
# 2021-03-22  205.6900
# 2021-03-23  203.3477
# 2021-03-24  201.6633
# 2021-03-25  203.1500
# 2021-03-26  206.4700

library(dplyr)

plusvec <-c(0.02,-0.08,0.02,0.01,-0.03,-0.05,0.02,0.03,-0.03,0.01,0.03) +1
#plusvec <-c(0.01,-0.08,-0.01,0.02,0.03,0.1,0.04,0.01) +1

price_sus <- cumprod(plusvec)
price_sus


cummax(price_sus)
price_derm <- price_sus[1:length(price_sus)-1]
price_derm <- c(price_derm[1] , price_derm)
price_derm

vec_number <- ifelse(price_derm < price_sus , 1 , 0)
vec_number

vec_cum <- cumsum(vec_number)


## not useful for new max series

dd <- 1 - price_sus/cummax(price_sus)

dd <- dd *-1

dd
print(min(dd))



price_sus<- price_sus[-c(which.max(dd):which.min(dd))]


dd <- 1 - price_sus/cummax(price_sus)

dd <- dd *-1

dd
print(min(dd))
price_sus<- price_sus[-c(which.max(dd):which.min(dd))]

dd <- 1 - price_sus/cummax(price_sus)

dd <- dd *-1

dd
print(min(dd))

# The cumulative maximum of a price series is the maximum
# price in the past, reached up to that point in time.
# Calculate the cumulative maximum of price_s using
# function cummax() and call it cum_max.
# Plot the cumulative maximum of price_s using function
# quantmod::chart_Series().

### write your code here



cum_max <- cummax(price_s)
cum_max

quantmod::chart_Series(price_s)
quantmod::chart_Series(cum_max)


# A drawdown is a drop in price from its previous maximum.
# Calculate the xts time series of drawdowns of price_s,
# as the difference between price_s minus the cumulative
# maximum of price_s and call it draw_down.

### write your code here


draw_down <- price_s - cum_max


# You should get the following output:
NROW(draw_down)
# [1] 4987
tail(draw_down)
#             VTI.Close
# 2021-03-19 -3.578171
# 2021-03-22 -2.471828
# 2021-03-23 -4.814085
# 2021-03-24 -6.498516
# 2021-03-25 -5.011831
# 2021-03-26 -1.691831


# Extract the date index from the time series price_s 
# and call it date_s.  date_s is a vector of dates.
# You can use the function zoo::index().

### write your code here

date_s <- zoo::index(price_s)




# You should get the following output:
class(date_s)
# [1] "Date"
NROW(date_s)
# [1] 4987
tail(date_s)
# [1] "2021-03-19" "2021-03-22" "2021-03-23" "2021-03-24" "2021-03-25" "2021-03-26"


# Find the minimum value of draw_down (call it 
# max_drawdown), find the integer index of the 
# minimum value (call it index_trough), and find 
# the date when it reaches its minimum (call it 
# date_trough). 
# You can use date_s and the function which.min().

### write your code here

index_trough <- which.min(draw_down)
date_trough <- date_s[index_trough]
max_drawdown <-draw_down[which.min(draw_down)]

# You should get the following output:
index_trough
# [1] 4732
date_trough
# [1] "2020-03-23"
max_drawdown
#             VTI.Close
# 2020-03-23 -59.03535


# Plot draw_down using function quantmod::chart_Series().

x11(width=6, height=5)
### write your code here

plot_theme <- chart_theme()
plot_theme$col$line.col <-  "blue"

quantmod::chart_Series(draw_down , theme = plot_theme, name="VTI Drawdowns")


# Add a vertical red line to the draw_down plot,
# at the date date_trough.
# You can use the function abline(),

### write your code here

abline(v= index_trough, lwd=2, col="red")



# Your plot should be similar to drawdowns_vti.png


# 2. (20pts) Divide draw_down into two time series at
# the date date_trough.
# First subset draw_down to dates before date_trough,
# and call it pre_drawdown,

### write your code here




pre_drawdown <- draw_down[paste0("/",date_trough-1)  ]







# Next subset draw_down to dates after date_trough,
# and call it post_drawdown,

### write your code here


post_drawdown <- draw_down[paste0(date_trough +1, "/")  ] 

# Now find the date when the drawdown period starts.
# The drawdown starts when draw_down is first zero
# and then starts decreasing to some price below zero.
# Find the latest date when pre_drawdown was still
# equal to zero, and call it date_from.
# date_from is when the drawdown started.
# You can use the functions index() and max().

### write your code here


date_from <- max(index(pre_drawdown[pre_drawdown$VTI.Close == max(pre_drawdown$VTI.Close)]))


# Now find the date when the drawdown period ends.
# When the current price exceeds the previous maximum
# Price, then draw_down returns back to zero, and the
# drawdown period is over.
# A drawdown ends when draw_down first returns back
# to zero after date_trough.
# Find the first date when post_drawdown returns back
# to zero, and call it date_to.
# date_to is when the drawdown has ended.
# You can use the functions index() and min(),

### write your code here

date_to <- min(index(post_drawdown[post_drawdown$VTI.Close == max(post_drawdown$VTI.Close)]))



# You should get the following output:
date_from
# [1] "2020-02-19"
date_to
# [1] "2020-08-12"


# 3. (20pts) Combine the three dates: date_from,
# date_trough, and date_to into a named vector with
# names "from", "trough", and "to", and call it
# drawdown_dates.

### write your code here

drawdown_dates <- c(date_from , date_trough , date_to)
names(drawdown_dates) <- c("from","trough","to")



# You should get the following output:
drawdown_dates
#       from       trough           to
# "2020-02-19" "2020-03-23" "2020-08-12"

# Subset the price_s to data since 2020-01-01, and call 
# it prices_2020. 
# Hint: Use the notation "2020-01-01/".

### write your code here

prices_2020 <- price_s["2020-01-01/"]




# You should get the following output:
dim(prices_2020)
# [1] 311   1
head(prices_2020)
#            VTI.Close
# 2020-01-02  161.6271
# 2020-01-03  160.5985
# 2020-01-06  161.1471
# 2020-01-07  160.7650
# 2020-01-08  161.5586
# 2020-01-09  162.5676


# Plot prices_2020 using the function 
# quantmod::chart_Series().

x11(width=6, height=5)
### write your code here
plot_theme <- chart_theme()
plot_theme$col$line.col <-  "blue"
quantmod::chart_Series(prices_2020, theme = plot_theme, name="VTI drawdown dates")



# Add vertical green, red, and orange lines for the
# three dates: date_from, date_trough, date_to.
# Add text at the vertical lines equal to
# names(drawdown_dates).
# Hint: Use the function match() and index() to first
# calculate the index of drawdown_dates.
# You can use the functions match(), index(), abline(),
# names(), and text().

### write your code here


abline(v= match(drawdown_dates,index(prices_2020)), lwd=2, col=c("green","red","orange"))
text(match(drawdown_dates,index(prices_2020)) 
     ,prices_2020[match(drawdown_dates,index(prices_2020))]$VTI.Close  +10
     , names(drawdown_dates) )








# Your plot should be similar to drawdown_plot_vti.png



############## Part II
# Summary: Compare the performance of trailing versus centered 
# Hampel filters, to identify bad data in a time series.

# Create a time series of prices called price_s, by 
# introducing 200 random isolated price jumps into 
# VTI close prices as follows:

# Run the code below:
library(rutils)
price_s <- na.omit(rutils::etf_env$price_s$VTI)
set.seed(1121)
n_bad <- 200
is_jump <- logical(NROW(price_s))
is_jump[sample(x=NROW(is_jump), size=n_bad)] <- TRUE
price_s[is_jump] <- price_s[is_jump]*sample(c(0.95, 1.05), size=n_bad, replace=TRUE)






# 1. (10pts) 
# Calculate from win_dow a half window called half_window.
# You must use the %/% operator. 

win_dow <- 11

### write your code here

half_window <- win_dow %/% 2

# You should get the following output:
half_window
# [1] 5

# Calculate a time series of rolling z-scores 
# called z_trailing, using the Hampel filter code 
# from the lecture slides.
# You can use the functions TTR::runMedian(), 
# TTR::runMAD(), and is.na(). 

### write your code here



medi_an <- TTR::runMedian(price_s, n=win_dow)
ma_d <- TTR::runMAD(price_s, n=win_dow)
z_trailing <- (price_s - medi_an)/ma_d
z_trailing[1:win_dow,] <-0




# You should get the following outputs:

sum(is.na(z_trailing))
# [1] 0

tail(z_trailing)
#             VTI.Close
# 2021-03-19  0.0000000
# 2021-03-22  0.0000000
# 2021-03-23 -0.9664959
# 2021-03-24  2.0333474
# 2021-03-25 -1.1806942
# 2021-03-26  0.0000000

tail(z_trailing[is_jump])
#             VTI.Close
# 2020-09-04  1.594187
# 2020-10-06  3.408226
# 2020-12-11  5.447233
# 2021-01-28 -4.005667
# 2021-02-02 -2.347685
# 2021-03-24  2.033347


# Calculate the time series of centered median prices 
# and centered MAD of prices. 
# Note that the functions runMedian() and runMAD() perform 
# calculations over a trailing interval over past prices, 
# not over a centered interval, so you have to center the 
# time series returned by those functions by advancing 
# (negative lagging) the time series by half the periods 
# in win_dow.
# You can use the function rutils::lag_it(). 

### write your code here


medi_an <- rutils::lag_it(medi_an, lagg=-half_window)
ma_d <- rutils::lag_it(ma_d, lagg=-half_window)

medi_an_temp <- medi_an
ma_d_temp <- ma_d

medi_an[medi_an == 0] <- NA
ma_d[ma_d == 0] <- NA

medi_an <- na.locf(na.locf(medi_an), fromLast = TRUE)
ma_d <- na.locf(na.locf(ma_d), fromLast = TRUE)






# You should get the following output:
tail(medi_an, 9)
#               [,1]
# 2021-03-16 205.690
# 2021-03-17 206.308
# 2021-03-18 206.308
# 2021-03-19 206.470
# 2021-03-22 206.470
# 2021-03-23 206.470
# 2021-03-24 206.470
# 2021-03-25 206.470
# 2021-03-26 206.470

tail(ma_d, 9)
#                 [,1]
# 2021-03-16 2.423452
# 2021-03-17 2.674664
# 2021-03-18 2.674664
# 2021-03-19 2.508309
# 2021-03-22 2.508309
# 2021-03-23 2.508309
# 2021-03-24 2.508309
# 2021-03-25 2.508309
# 2021-03-26 2.508309


# Calculate a time series of rolling centered z-scores 
# called z_centered, from the price_s and the centered 
# medi_an and ma_d.
# Set all the NA or Inf values equal to 0.
# You can use the functions TTR::runMedian(), 
# TTR::runMAD(), is.na(), and is.finite(). 

### write your code here


z_centered <- (price_s - medi_an)/ma_d
z_centered[1:win_dow, ] <- 0
z_centered[which(is.na(z_centered))] <- 0
z_centered[which(!is.finite(z_centered))] <- 0




# You should get the following outputs:

sum(is.na(z_centered))
# [1] 0

sum(!is.finite(z_centered))
# [1] 0

tail(z_centered, 9)
#             VTI.Close
# 2021-03-16  0.6744908
# 2021-03-17  0.6744908
# 2021-03-18 -0.7415672
# 2021-03-19 -0.7520364
# 2021-03-22 -0.3109653
# 2021-03-23 -1.2447644
# 2021-03-24  2.1036007
# 2021-03-25 -1.3236009
# 2021-03-26  0.0000000

tail(z_centered[is_jump])
#             VTI.Close
# 2020-09-04  0.8323935
# 2020-10-06  0.9594287
# 2020-12-11  3.8048326
# 2021-01-28 -3.4129498
# 2021-02-02 -1.5379732
# 2021-03-24  2.1036007


# 2. (20pts) 
# Create a vector of threshold values (called 
# threshold_s), from 0.2 to 5.0, every 0.2.
# You can use the function seq(). 

### write your code here

threshold_s <- seq(0.2,5.0,0.2)


# You should get the following output:
threshold_s
#  [1] 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0 2.2 2.4 2.6 2.8 3.0 3.2 3.4 3.6 3.8
# [20] 4.0 4.2 4.4 4.6 4.8 5.0

# Define confusion matrix as function of thresh_old:
# Run the code below:

con_fuse <- function(actu_al, z_scores, thresh_old) {
  confu_sion <- table(!actu_al, !(abs(z_scores) > thresh_old))
  confu_sion <- confu_sion / rowSums(confu_sion)
  c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])
}  # end con_fuse

# You should get the following output:
con_fuse(is_jump, z_trailing, thresh_old=3)
#     typeI     typeII 
# 0.05138918 0.28000000

# Calculate matrices of error rates for z_trailing 
# and for z_centered, called error_trailing and
# error_centered.
# You can adapt the code from the lecture slides.

### write your code here


error_trailing <- t(sapply(threshold_s, con_fuse, actu_al=is_jump, z_scores=z_trailing))
error_centered <- t(sapply(threshold_s, con_fuse, actu_al=is_jump, z_scores=z_centered))


error_trailing <- rbind(c(1,0),error_trailing , c(0,1))
error_centered <- rbind(c(1,0),error_centered, c(0,1))

# You should get the following output:
head(error_trailing, 3)
#         typeI   typeII
# [1,] 1.0000000   0.00
# [2,] 0.8604554   0.01
# [3,] 0.7800292   0.02

tail(error_trailing, 3)
#           typeI  typeII
# [25,] 0.01483184  0.540
# [26,] 0.01399624  0.575
# [27,] 0.00000000  1.000

head(error_centered, 3)
#           typeI typeII
# [1,] 1.0000000   0.00
# [2,] 0.6574055   0.01
# [3,] 0.5253812   0.01

tail(error_centered, 3)
#           typeI    typeII
# [25,] 0.0004177982  0.610
# [26,] 0.0004177982  0.645
# [27,] 0.0000000000  1.000


# 3. (10pts) 
# Calculate the area under the ROC curve (AUC)
# for error_trailing.

### write your code here



true_pos_t <- (1 - error_trailing[, "typeII"])
true_pos_t <- (true_pos_t + rutils::lag_it(true_pos_t))/2
false_pos_t <- rutils::diff_it(error_trailing[, "typeI"])
abs(sum(true_pos_t*false_pos_t))





# You should get the following output:
# [1] 0.8991811

# Calculate the area under the ROC curve (AUC)
# for error_centered.

### write your code here


true_pos_c <- (1 - error_centered[, "typeII"])
true_pos_c <- (true_pos_c + rutils::lag_it(true_pos_c))/2
false_pos_c <- rutils::diff_it(error_centered[, "typeI"])
abs(sum(true_pos_c*false_pos_c))



# You should get the following output:
# [1] 0.9633899

# The AUC for error_centered is greater than for 
# error_trailing which shows that it's a better classifier
# of the bad data points.


# 4. (10pts) 
# The informedness is equal to the sum of sensitivity plus 
# specificity. 
# The sensitivity is equal to the true-positive rate.
# The specificity is equal to the true-negative rate.
# Calculate the vectors of informedness for error_trailing 
# and for error_centered.
# Calculate the threshold value corresponding to the 
# highest informedness.
# You can use the function which.max().

### write your code here


sensitivity <- 1-error_trailing[, "typeII"]
specificity <- 1-error_trailing[, "typeI"]
informedness <- sensitivity + specificity
threshold_trailing <- threshold_s[which.max(informedness)]





# You should get the following threshold value corresponding 
# to the highest informedness for error_trailing:
threshold_trailing
# [1] 2.2

### write your code here

sensitivity <- 1-error_centered[, "typeII"]
specificity <- 1-error_centered[, "typeI"]
informedness <- sensitivity + specificity
threshold_centered <- threshold_s[which.max(informedness)]


# You should get the following threshold value corresponding 
# to the highest informedness for error_centered:
threshold_centered
# [1] 1.6


# 5. (20pts) 
# Plot the ROC curves for error_trailing and error_centered.
# Add points with the highest informedness.
# You can use the functions plot(), lines(), abline(), points(),
# text(), and legend(). 
# Your plot should be similar to hampel_roc_centered.png

x11(width=6, height=6)

### write your code here
plot(
  x=error_trailing[, "typeI"],
  y=1-error_trailing[, "typeII"],
  xlab="FALSE positive rate", ylab="TRUE positive rate",
  xlim=c(0, 1), ylim=c(0, 1), main="ROC Curve for Hampel Classifier",
  type="l", lwd=3, col="blue")

lines(x=error_centered[, "typeI"],  y=1-error_centered[, "typeII"], lwd=2, col="red")

points(x =error_trailing[, "typeI"][match(threshold_trailing,threshold_s )],
       y =(1-error_trailing[, "typeII"])[match(threshold_trailing,threshold_s )],
       pch = 19 , col = "blue" ,cex = 1.2 )

points(x =error_centered[, "typeI"][match(threshold_centered,threshold_s )],
       y =(1-error_centered[, "typeII"])[match(threshold_centered,threshold_s )],
       pch = 19 , col = "red"  ,cex = 1.2)

abline(a=0.0, b=1.0, lwd=3, col="orange")

legend("right", inset=0.05, bty="n",
       leg=c("trailing", "centered"),
       lwd=6, lty=1, col=c("blue", "red"))
text(x =error_trailing[, "typeI"][match(threshold_trailing,threshold_s )],
     y =(1-error_trailing[, "typeII"])[match(threshold_trailing,threshold_s )]+0.05
     , labels  = "Trailing", col = "blue")


text( x =error_centered[, "typeI"][match(threshold_centered,threshold_s )],
      y =(1-error_centered[, "typeII"])[match(threshold_centered,threshold_s )]+0.05
      , labels  = "Centered", col = "red")


# Plot ROC curve for Hampel classifier
  

