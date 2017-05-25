library("kknn")
library("caTools") #must be install for split
library("RCurl") # must be install for acess table url on MacBook --getURL

#Getting data with header from EDX
data <- getURL("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/e39a3df780dacd5503df6a8322d72cd2/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt")
data = read.table(text = data, header = TRUE)

#Putting seed for evalute data eliminating randomess noise for each rerunning
set.seed(111)

#splitting data for train
spl = sample.split(data$R1, SplitRatio = 0.6)
train = subset(data, spl == TRUE)

#splitting data for eval & test
temp = subset(data, spl == FALSE)
spl = sample.split(temp$R1, SplitRatio = 0.5)

eval = subset(temp, spl == TRUE)
test = subset(temp, spl == FALSE)
nrow(test)/4
#Cross Validation folds

folds1 = test[1:32, ]
folds2 = test[33:65, ]
folds3 = test[66:97, ]
folds4 = test[98:130, ]
folds = list(folds1, folds2, folds3, folds4)

#Must initializate a data.frame for results
kresults = data.frame()

# looking for best k
kvalue = 1
nFolds = 1

#Put which level of aceptance for give or not a loan
threshold = 0.5

#Cross Validation
for (nFolds in 1:4) {
  for(kvalue in 1:100){
    #Running to get model
    model = train.kknn(R1 ~ ., data = train[], ks = kvalue, scale = TRUE)
  
    #Use threshold as level of aceptance 
    #Multiple by 1 to convert boolean (FALSE/TRUE) to number (0/1)
    predict = (predict(model, folds[[nFolds]][, -11]) >= threshold) * 1
  
    # Storing result
    testResult = sum(predict == folds[[nFolds]][, -11])
    resultAcc = testResult / length(predict)
    kresults[kvalue * nFolds, 1] = kvalue
    kresults[kvalue * nFolds, 2] = resultAcc 
  }
}

#Looking best k
bestk = kresults[which.max(kresults$V2), ]

# rerunning model with best K
model = train.kknn(R1 ~ ., data = train[], 
                   ks = bestk[1,1], scale = TRUE)

#Remenber threshold is level of aceptance 
#Multiple by 1 to convert boolean (FALSE/TRUE) to number (0/1)
predict = (predict(model, eval[, -11]) >= threshold) * 1

# Best k "Accuraccy"
testResult = sum(predict == eval[, 11])
testAcc = testResult / length(predict)

# Compare results graphically
plot(kresults, main = "KNN Accuracy by k", xlab = "Numeber k", 
     ylab = "Accuracy", xlim = c(1, 100), ylim = c(0.8, 0.9))
