# -------------------- Code for HW 3 Question 2 -----------------------------
# Clear environment

rm(list = ls())

# ---------------------------- Load Libraries -------------------------------------

#This library is necessary for using the ses() function
library(smooth)

# ---------------------------- Data manipulation -------------------------------------

# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)

data <- read.table("temps.txt", header = TRUE)

#
# optional check to make sure the data is read correctly
#

head(data)

# Console output for head(data)
##    DAY X1996 X1997 X1998 X1999 X2000 X2001 X2002 X2003 X2004 X2005
##1 1-Jul    98    86    91    84    89    84    90    73    82    91
##2 2-Jul    97    90    88    82    91    87    90    81    81    89
##3 3-Jul    97    93    91    87    93    87    87    87    86    86
##4 4-Jul    90    91    91    88    95    84    89    86    88    86
##5 5-Jul    89    84    91    90    96    86    93    80    90    89
##6 6-Jul    93    84    89    91    96    87    93    84    90    82
##        X2006 X2007 X2008 X2009 X2010 X2011 X2012 X2013 X2014 X2015
##1          93    95    85    95    87    92   105    82    90    85
##2          93    85    87    90    84    94    93    85    93    87
##3          93    82    91    89    83    95    99    76    87    79
##4          91    86    90    91    85    92    98    77    84    85
##5          90    88    88    80    88    90   100    83    86    84
##6          81    87    82    87    89    90    98    83    87    84
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#

# create a vector of this data

data <- as.vector(unlist(data[,2:21]))

# turn the vector into a time series object

myts <- ts(data,start=1996,end=2015,frequency=123)
myts

plot.ts(myts)

# ---------------------------- Models with Holt Winters -------------------------------------


# Single exponential smoothing

m1 <- HoltWinters(myts,beta=FALSE,gamma=FALSE)
m1

## Holt-Winters exponential smoothing without trend and without seasonal component.
## 
## Call:
## HoltWinters(x = myts, beta = FALSE, gamma = FALSE)
## 
## Smoothing parameters:
##  alpha: 0.8396301
##  beta : FALSE
##  gamma: FALSE
## 
## Coefficients:
##       [,1]
## a 81.62444
#
# So, the baseline estimate at the end is 81.62444, and the 
# best value of alpha found is 0.8396301.
# [Of course, both of those have more significant digits reported
# than are reasonable.]


# Double exponential smoothing

m2 <- HoltWinters(myts,gamma=FALSE)
m2

## Holt-Winters exponential smoothing with trend and without seasonal component.
## 
## Call:
## HoltWinters(x = myts, gamma = FALSE)
## 
## Smoothing parameters:
##  alpha: 0.8455303
##  beta : 0.003777803
##  gamma: FALSE
## 
## Coefficients:
##           [,1]
## a 81.729657393
## b -0.004838906
#
# Notice that the final trend estimate (b) is very close to zero
# (-0.004838906) and the value of beta is also very close to zero.
# This suggests that there isn't really a significant trend.


# Triple exponential smoothing (additive seasonality)

m3a <- HoltWinters(myts)
m3a

# Lots of output (123 seasonal factors) but the key is that
# b and beta are again both zero or very close to it.


# Triple exponential smoothing (multiplicative seasonality)

m3m <- HoltWinters(myts,seasonal="multiplicative")
m3m

# Lots of output (123 seasonal factors) but the key is that
# b and beta are again both zero or very close to it.

# m3m$fitted[4] shows the seasonal factors for each data point.
#
# Put the factors into a matrix
m <- matrix(m3m$fitted[1:length(m3m$fitted[,4])-1,4],nrow=123)
m

#From here, we can run the same CUSUM analysis as in the previous homework, but on the seasonal factors.



# ---------------------------- Models with es -------------------------------------

#Note that the es function uses the model input to indicate what type of model to use for 
#the es function in relation to the coefficients alpha, beta, and gamma, which are the 3 letters
#used in the model input. So "ANN" indicates null values for beta and gamma
#A is additive, N is null (at least that is what seems to be the case) and M is multiplicative
#Partial print outs from the console are provided below to compare coefficients between the models

# Exponential smoothing without trend and without seasonal component.
# 
es1a <- es(myts, model="ANN")
es1a

es1m <- es(myts, model="MNN")
es1m

##Model estimated: ETS(ANN)
##  alpha 
##  0.84 
##  AIC     AICc      BIC 
##13968.70 13968.71 13985.97 

##Model estimated: ETS(MNN)
##  alpha 
##  0.845 
##  AIC     AICc      BIC 
##14304.73 14304.74 14322.00 

# Exponential smoothing with trend and without seasonal component.
#
es2a <- es(myts, model="AAN")
es2a

es2m <- es(myts, model="AMN")
es2m

##Model estimated: ETS(AAN)
##  alpha  beta 
##  0.84  0.00 
##  AIC     AICc      BIC 
##13972.69 13972.72 14001.48 

##Model estimated: ETS(AMN)
##  alpha  beta 
##  0.84  0.00 
##  AIC     AICc      BIC 
##13971.13 13971.16 13999.92 

# Triple exponential smoothing (additive seasonality)

es3a <- es(myts, model="AAA")
es3a

es3m <- es(myts, model="AMA")
es3m

##Model estimated: ETS(AAA)
##  alpha  beta gamma 
##  0.798 0.001 0.103 
##  AIC     AICc      BIC 
##14042.79 14057.98 14785.45 

##Model estimated: ETS(AMA)
##  alpha  beta gamma 
##  0.57  0.00  0.01 
##  AIC     AICc      BIC 
##13920.65 13935.84 14663.30 

# Triple exponential smoothing (multiplicative seasonality)

es3m <- es(myts, model="AAM")
es3m

es4m <- es(myts, model="AMM")
es4m

##Model estimated: ETS(AAM)
##  alpha  beta gamma 
##  0.306 0.002 0.008 
##  AIC     AICc      BIC 
##14203.28 14218.47 14945.94 

##Model estimated: ETS(AMM)
##  alpha  beta gamma 
##  0.576 0.001 0.009 
##  AIC     AICc      BIC 
##13945.79 13960.98 14688.45 
