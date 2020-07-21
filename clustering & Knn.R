
#Q 3.1 part a 
rm(list=ls())
getwd() # where I am
setwd("C:/Users/shafa/OneDrive/Documents/Rdata")

set.seed(42)
library(kknn)
# import data
data<- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)
view(data)
head(data)

shuffled <- data[sample(654),]
data_training = data[1:555, ]
data_test = data[556:654, ]

#leave one-out crossvalidation via train.kknn.
set.seed(1)
model<-train.kknn(V11~., data_training,kmax=20, scale=TRUE)
yhat <-model$fitted.value

round(yhat)
(model$fitted.values)== data$V11

##obtain fitted value from this train.kknn() object.
round(fitted(model))
fitted(model)[[4]][1:nrow(data)]

##accuracy
accuracy <- sum(round(model$fitted.values)== data$V11) / nrow(data)


#using Cv.kknn to perfor specific type of cv for i given number of neighbors.
# kcv=10 is 10-fold cross-validation ,K=5 is number of neighbors .
set.seed(1)
model<-cv.kknn(V11~., data, kcv=10,k=5, scale=TRUE)
model[[1]][,2]

pred_cvknn <-predict(model,data_val[,1:10])
sum(pred_cvknn == data[,11]) / nrow(data)


# Q3.1 part b
#splitting the data set
set.seed(1)
654 <- nrow(data)
shuffled <- data[sample(654),]
data_training = data[1:456, ]
data_val = data[457:555, ]
data_test = data[556:654, ]

# model with CVM
library(kernlab)


#model with kknn
library(kknn)
model_knn<-cv.kknn(V11~., data_training,data_val, kcv=10 ,k=12, scale = TRUE)
model[[1]][,2]
round(model2$fitted.values)== data_test$V11
accuracy <- sum(round(model2$fitted.values)== data_test$V11) / nrow(data_test)

#finding the accuracy
round(fitted(model_knn))== data_val$V11
for(i in c(1:25)){
  model2<- kknn(V11 ~.,data_training ,data_test, k=i, scale=TRUE)
  model2
  
  round(model2$fitted.values)== data_test$V11
  accuracy <- sum(round(model2$fitted.values)== data_test$V11) / nrow(data_test)
  cat("k=",i,"modelaccuracy=",accuracy )
}

# model with KSVM 
for(i in c(0.000001,0.00001,0.0001,0.001,0.01,0.1,1,2,4,6,8,10,50,100,1000,10000)){
  model <- ksvm(V11 ~.,  data_training ,type= "C-svc", kernel="vanilladot",C=i , scaled=TRUE)
  model
  pred <- predict(model,data_test[,1:10])
  accuracy <- sum(pred == data_test[,11]) / nrow(data_test)
  cat("c=",i,"modelaccuracy=",accuracy )
}


Q4.1.

One of the usage of clustering models is in healthcare study.
Specially in medicine, when know that repsone of indiviual to prescriptions varies alot. knowing that how each person reponse to specicif prescription can help the physican
and health system alot through optimization of the prescription and diminishing the drugs side effects.
So that each patients response can be observed and then the clustering model can be applied to 
classify the different patients group based on their reponse to durg(specific prescriptions). 
Many predictors can be used in this models.for example for a drug like Metformin which is used for diabetic patients 
predictors like Blood pressures , weight, heart rate, glucose level, mental health, etc can be used for 
clustering model of pateints response to drug.





# Q 4.2

rm(list=ls())
data<-read.table("iris.txt", header = TRUE)
data<-data[,2:6]
table(data$Species)

library(ggplot2)
ggplot(data, aes(Sepal.Length, Petal.Width, color=Species))+geom_point()

#using unsupervised learning method
set.seed(1)
clustering_1<- kmeans(data[,1:4], centers= 1 , nstart = 3)
clustering_2<- kmeans(data[,1:4], centers= 2, nstart = 3)
clustering_3<- kmeans(data[,1:4], centers= 3 , nstart = 3)
clustering_4<- kmeans(data[,1:4], centers= 4 , nstart = 3)
clustering_5<- kmeans(data[,1:4], centers= 5 , nstart = 3)
clustering_6<- kmeans(data[,1:4], centers= 6 , nstart = 3)

#total whitin sum of square is good metric for elbow diagram
clustering_1$tot.withinss
clustering_2$tot.withinss
clustering_3$tot.withinss
clustering_4$tot.withinss
clustering_5$tot.withinss
clustering_6$tot.withinss

# elbow diagram for choosing best value k for k mean clustering
k <- c(1,2,3,4,5,6)
distortion <- c(clustering_1$tot.withinss, clustering_2$tot.withinss,clustering_3$tot.withinss
, clustering_4$tot.withinss,clustering_5$tot.withinss, clustering_6$tot.withinss)
plot(x = k , y = distortion,type= "l", xlab= "k", ylab="distortion")


#

table(clustering_3$cluster, data$Species)
ggplot()
ggplot(data, aes(Sepal.Length, Petal.Width, color=clustering_3$cluster))+geom_point()
ggplot(data, aes(Sepal.Length, Petal.Width, color=clustering_2$cluster))+geom_point()

# the best clustering was clutering_3 due to matches the real data mostly.





