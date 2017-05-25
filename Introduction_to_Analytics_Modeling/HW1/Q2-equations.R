data <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data.txt", header = TRUE)

model <- ksvm(as.matrix(data[ ,1:10]), as.factor(data[ ,11]), type = "C-svc", kernel="vanilladot", C=100, scaled = TRUE)

a <- colSums(data[model@SVindex, 1:10] * model@coef[[1]])

a0 <- sum(a*data[1,1:10]) - model@b

pred <- predict(model, data[,11])

predfrac <- sum(pred == data[,11]) / nrow(data)

print(a)
print(a0)
print(predfrac)