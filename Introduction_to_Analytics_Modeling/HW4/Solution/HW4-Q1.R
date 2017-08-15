# -------------------- Code for HW 4 Question 1 -----------------------------
# Clear environment

rm(list = ls())

# Install the DAAG package, which has cross-validation functions

install.packages("DAAG")
library(DAAG)


# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)


# ---------------------------- Data manipulation -------------------------------------

# First, read in the data
#

data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#
# Optional check to make sure the data is read correctly
#

head(data)

##      M So   Ed  Po1  Po2    LF   M.F Pop   NW    U1  U2 Wealth Ineq     Prob    Time Crime
## 1 15.1  1  9.1  5.8  5.6 0.510  95.0  33 30.1 0.108 4.1   3940 26.1 0.084602 26.2011   791
## 2 14.3  0 11.3 10.3  9.5 0.583 101.2  13 10.2 0.096 3.6   5570 19.4 0.029599 25.2999  1635
## 3 14.2  1  8.9  4.5  4.4 0.533  96.9  18 21.9 0.094 3.3   3180 25.0 0.083401 24.3006   578
## 4 13.6  0 12.1 14.9 14.1 0.577  99.4 157  8.0 0.102 3.9   6730 16.7 0.015801 29.9012  1969
## 5 14.1  0 12.1 10.9 10.1 0.591  98.5  18  3.0 0.091 2.0   5780 17.4 0.041399 21.2998  1234
## 6 12.1  0 11.0 11.8 11.5 0.547  96.4  25  4.4 0.084 2.9   6890 12.6 0.034201 20.9995   682
# NOTE: ALL ROWS OF THIS FILE STARTING WITH "##" DENOTE R OUTPUT
#
# Crime is response, other variables are predictors

# Run PCA on matrix of scaled predictors

pca <- prcomp(data[,1:15], scale. = TRUE)
summary(pca)

## Importance of components:
##                          PC1   PC2   PC3    PC4    PC5    PC6    PC7    PC8    PC9   PC10   PC11    PC12    PC13   PC14
## Standard deviation     2.453 1.674 1.416 1.0781 0.9789 0.7438 0.5673 0.5544 0.4849 0.4471 0.4191 0.35804 0.26333 0.2418
## Proportion of Variance 0.401 0.187 0.134 0.0775 0.0639 0.0369 0.0214 0.0205 0.0157 0.0133 0.0117 0.00855 0.00462 0.0039
## Cumulative Proportion  0.401 0.588 0.722 0.7992 0.8631 0.9000 0.9214 0.9419 0.9576 0.9709 0.9826 0.99117 0.99579 0.9997
##                          PC15
## Standard deviation     0.06793
## Proportion of Variance 0.00031
## Cumulative Proportion  1.00000

# Another way to use the prcomp function

pca <- prcomp(~.,data = data[,1:15], scale. = TRUE)

# The following are useful visualizations when deciding how many principal components to choose.
# In this case, we are told to just use the first 4 principal components.

screeplot(pca, type="lines",col="blue")

var <- pca$sdev^2
propvar <- var/sum(var)

plot(propvar, xlab = "Principal Component", ylab = "Proportion of Variance Explained",~
     ylim = c(0,1), type = "b")

plot(cumsum(propvar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

# Getting the first 4 principal components (three ways)
#------------------

# Method 1: direct from prcomp output

PCs <- pca$x[,1:4]

# Method 2: calculated from prcomp output

data.scale <- as.data.frame(scale(data[,1:15]))
data.mat = as.matrix(data.scale)
PCs2 <- data.mat %*% pca$rotation[,1:4]

# Method 3: calculated using the math, if you did not use the prcomp function

E <- eigen(t(data.mat) %*% data.mat)
PCs3 <- data.mat %*% E$vectors[,1:4]
# NOTE: Eigenvectors 3&4 are the negative of what we get using the other approaches

# Build linear regression model with the first 4 principal components

PCcrime <- cbind(PCs, data[,16])
model <- lm(V5~., data = as.data.frame(PCcrime))
summary(model)

## Call:
## lm(formula = V5 ~ ., data = as.data.frame(PCcrime))
## 
## Residuals:
##   Min     1Q Median     3Q    Max 
## -557.8 -210.9  -29.1  197.3  810.3 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    905.1       49.1   18.44   <2e-16 ***
## PC1             65.2       20.2    3.23   0.0024 ** 
## PC2            -70.1       29.6   -2.36   0.0227 *  
## PC3             25.2       35.0    0.72   0.4760    
## PC4             69.4       46.0    1.51   0.1387    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 336 on 42 degrees of freedom
## Multiple R-squared:  0.309,	Adjusted R-squared:  0.243 
## F-statistic:  4.7 on 4 and 42 DF,  p-value: 0.00318

# This model has R^2 = 0.309 and R^2_adj = 0.243. See below for followup.

# Coefficeints for this linear regression model

beta0 <- model$coefficients[1]
beta0

## (Intercept) 
## 905 

betas <- model$coefficients[2:5]
betas

##  PC1   PC2   PC3   PC4 
## 65.2 -70.1  25.2  69.4 

# Transform the PC coefficients into coefficients for the original variables

alphas <- pca$rotation[,1:4] %*% betas
t(alphas)

##          M   So   Ed  Po1  Po2  LF   M.F  Pop   NW    U1   U2 Wealth  Ineq Prob  Time
## [1,] -21.3 10.2 14.4 63.5 64.6 -14 -24.4 39.8 15.4 -27.2 1.43   38.6 -27.5  3.3 -6.61

# Regression model from previous homework

model2 <- lm( Crime ~ ., data = data)
summary(model2)

# This model has R^2 = 0.803 and R^2_adj = 0.708.

# These results suggest that we are better off using a more straightforward regression model
# instead of PCA before using regression.
# If we had used all 15 principal components, we would have obtained
# an R-squared value of 0.803, which is the same R-squared value when using all 
# 15 regular predictors in a basic linear regression model.

# In fact, let's try all possibilities: for i=1..15, run a regression using the first i principal components
#

r2 <- numeric(15) # create a vector to store the R-squared values

for (i in 1:15) {
  pclist <- pca$x[,1:i]  # use the first i prinicipal components
  pcc <- cbind(data[,16],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  r2[i] <- 1 - sum(model$residuals^2)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared
}

r2

## [1] 0.1711351 0.2631339 0.2716416 0.3091121 0.6451941 0.6586023 0.6881819 0.6898765
## [9] 0.6920491 0.6962873 0.6973865 0.7692656 0.7723664 0.7911447 0.8030868

# Compare these two plots: 
# ... cumulative proportion of variance explained, and
# ... R-squared with this many principal components

plot(cumsum(propvar), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type = "b")

plot(r2, xlab = "Principal Component", ylab = "R-squared with this many principal components",
     ylim = c(0,1), type = "b")

# Notice the difference between the curves.
# Even though PCA estimates "proportion of variance" between the factors, 
# and R-squared estimates "proportion of variance explained" in the response,
# it turns out that they sometimes don't track so well.

# Anyway, back to the question of the PCA model seeming way worse than the non-PCA model.
# Remember from HW3 the big overfitting problem.
# Cross-validation estimated a much lower R-squared than
# the model showed on its training set.
# So, let's see what cross-validation says for PCA models:

# Install the DAAG package, which has cross-validation functions

install.packages("DAAG")
library(DAAG)

# do 5-fold cross-validation

r2cross <- numeric(15) # create a vector to store the R-squared values

for (i in 1:15) {
  pclist <- pca$x[,1:i]  # use the first i prinicipal components
  pcc <- cbind(data[,16],pclist)  # create data set
  model <- lm(V1~.,data = as.data.frame(pcc)) # fit model
  c <- cv.lm(as.data.frame(pcc),model,m=5) # cross-validate 
  r2cross[i] <- 1 - attr(c,"ms")*nrow(data)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared
}

r2cross

##  [1] 0.0735 0.0910 0.0666 0.1057 0.4872 0.4628 0.4562 0.3664 0.3337 0.2954 0.1863 0.3897
## [13] 0.3902 0.4736 0.4134

plot(r2cross, xlab = "Principal Component", ylab = "Cross-validated R-squared with this many principal components",
     ylim = c(0,1), type = "b")

#Notice that the 5th principal component seems to make a big difference (both on training data and in cross-validation).  So, let's see what happens if we use just that component in a model.

pcc <- cbind(data[,16],pca$x[,5])
model <- lm(V1~.,data = as.data.frame(pcc))
summary(model)

## Coefficients:
##            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    905.1       46.5   19.47   <2e-16 ***
## V2            -229.0       48.0   -4.77    2e-05 ***
## ---
## Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
##
## Residual standard error: 319 on 45 degrees of freedom
## Multiple R-squared:  0.336,     Adjusted R-squared:  0.321 
## F-statistic: 22.8 on 1 and 45 DF,  p-value: 1.95e-05

c <- cv.lm(as.data.frame(pcc),model,m=5) # cross-validate 
1 - attr(c,"ms")*nrow(data)/sum((data$Crime - mean(data$Crime))^2) # calculate R-squared

## [1] 0.216

# NOTE: PCA generally does not work well with binary data.
# In this data set, the second column is binary.
# There are some advanced methods to work with binary data,
# but we_re not going to cover them in this course.
# Instead, for this data set we could remove the binary factor,
# run PCA on the rest, and then add the binary factor back in.

# I won_t go through all the steps above, but I'll just show how it's done below.

pca2 <- prcomp(cbind(data[,1],data[3:15]),scale.=TRUE) # PCA without column 2
PCs2 <- pca2$x[,1:4] # first 4 principal components
PCcrime2 <- cbind(data[,2],PCs2,data[,16]) # Add column 2 back in
model2 <- lm(V6~.,data=as.data.frame(PCcrime2)) # regression model
summary(model2)


# -------------------- Code for HW 4 Question 1 - Alternate -----------------------------

install.packages("pls")
library(pls)

# Run principal component regression function with only the first 4 principal components

numcomp <- 4
pcr.fit <- pcr(Crime ~ ., data = data, scale = TRUE, ncomp = numcomp)
summary(pcr.fit)

## Data: 	X dimension: 47 15 
## Y dimension: 47 1
## Fit method: svdpc
## Number of components considered: 4
## TRAINING: % variance explained
##        1 comps  2 comps  3 comps  4 comps
## X        40.13    58.81    72.17    79.92
## Crime    17.11    26.31    27.16    30.91

# These are the first 4 principal components

pcr.fit$scores

# These are the regression coefficients for the original variables

coef(pcr.fit)

## , , 4 comps
## 
## Crime
## M      -21.28
## So      10.22
## Ed      14.35
## Po1     63.46
## Po2     64.56
## LF     -14.01
## M.F    -24.44
## Pop     39.83
## NW      15.43
## U1     -27.22
## U2       1.43
## Wealth  38.61
## Ineq   -27.54
## Prob     3.30
## Time    -6.61

# We can calculate the R-squared value directly.
# R-squared = 1 - SSEresiduals/SSEtotal
#
# Total sum of squared differences between data and its mean

SStot <- sum((data$Crime - mean(data$Crime))^2)

# Find predictions from this regression model

pcr.pred <- predict(pcr.fit, data = data, ncomp = numcomp)

# Calulating SSres and R-squared

SSres.pred <- sum((pcr.pred - data[,16])^2)
R2.pred <- 1 - SSres.pred/SStot
R2.pred

## [1] 0.309

