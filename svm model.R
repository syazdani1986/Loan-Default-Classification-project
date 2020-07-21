# hmw1

# Q2 

# Part1 KSVM model
rm(list=ls())

getwd() # where I am
setwd("C:/Users/shafa/OneDrive/Documents/Rdata")

creditData <- read.table("credit_card_data-headers.txt", header = TRUE)
View(creditData)
head(creditData)

# call ksvm. Vanilladot is a simple linear kernel.
library(kernlab)

model <- ksvm(R1 ~., data = creditData ,type= "C-svc", kernel="vanilladot",C=0.05 , scaled=TRUE)
# calculate a1.am
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
# calculate a0
a0 <- model@b
a0
# see what the model predicts
pred <- predict(model,creditData[,1:10])
pred
# see what fraction of the model's predictions match the
#actual classification
# accuracy
accuracy <- sum(pred == creditData[,11]) / nrow(creditData)

#applying different C value

for(i in c(0.000001,0.00001,0.0001,0.001,0.01,0.1,1,2,4,6,8,10,50,100,1000,10000)){
  model <- ksvm(R1 ~., data = creditData ,type= "C-svc", kernel="vanilladot",C=i , scaled=TRUE)
  model
  pred <- predict(model,creditData[,1:10])
  accuracy <- sum(pred == creditData[,11]) / nrow(creditData)
  cat("c=",i,"modelaccuracy=",accuracy )
}



# part 2 


# part 3 KKNN model

model <- kknn(R~. )

library(kknn)
# import data
data<- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)
View(data)
head(data)

#leave one-out crossvalidation via train.kknn.
set.seed(1)
#splitting the data set
set.seed(1)
654 <- nrow(data)
shuffled <- data[sample(654),]
data_training = data[1:456, ]
data_val = data[456:555, ]
data_test = data[555:654, ]


model2<- kknn(V11 ~.,data_training ,data_test, k=12, scale=TRUE)
model2$fitted.values


#finding the prediction
round(model2$fitted.values)

round(model2$fitted.values)== data_test$V11

# see what fraction of the model's predictions match the
#actual classification
# accuracy
accuracy <- sum(round(model2$fitted.values)== data_test$V11) / nrow(data_test)

# applying different K value 

for(i in c(1:25)){
  model2<- kknn(V11 ~.,data_training ,data_test, k=i, scale=TRUE)
  model2
  
  round(model2$fitted.values)== data_test$V11
  accuracy <- sum(round(model2$fitted.values)== data_test$V11) / nrow(data_test)
  cat("k=",i,"modelaccuracy=",accuracy )
}

