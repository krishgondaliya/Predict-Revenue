library(readr)
data <- read.csv('P5_train1.csv', sep = ",")
View(data)
plot(data$Source,data$Revenue)
plot(data$Duration,data$Revenue,col=data$Activity)
plot(data$Duration,data$Revenue,col=data$Source)

levels(data$Source)
ad = data[data$Source == "GoogleAdWord",]
link = data[data$Source == "Directlink",]
search = data[data$Source == "GoogleSearch",]

plot(ad$Duration,ad$Revenue,col=ad$Activity)

summary(link)
 ## SEARCH
plot(search$Duration,search$Revenue,col=search$Activity)
predictCol <- rep(0,nrow(search))
for(i in seq(1, 21, by = 1)) {
  sub = search[search$Duration > 37 & search$Activity == i,]
  model.lm <- lm(Revenue~ Duration, data = sub)
  pred =  predict(model.lm, newdata = sub)
  predictCol[search$Duration > 37 & search$Activity == i] = pred
}
mean((predictCol-search$Revenue)^2)
## error is mostly coming from below 37 

#LINK
plot(link$Duration,link$Revenue,col=link$Activity)
predictCol <- rep(0,nrow(link))
for(i in seq(1, 11, by = 1)) {
  sub = link[link$Duration > 37 & link$Activity == i,]
  model.lm <- lm(Revenue~ Duration, data = sub)
  pred =  predict(model.lm, newdata = sub)
  predictCol[link$Duration > 37 & link$Activity == i] = pred
}
mean((predictCol-link$Revenue)^2)









summary(link)
plot(link$Duration,link$Revenue,col=link$Activity)
one = link[link$Activity == 1,]
one
two = link[link$Activity == 2,]
three = link[link$Activity == 3,]
four = link[link$Activity == 4,]
five = link[link$Activity == 5,]
six = link[link$Activity == 6,]
seven = link[link$Activity == 7,]
eight = link[link$Activity == 8,]
nine = link[link$Activity == 9,]
ten = link[link$Activity == 10,]
eleven = link[link$Activity == 11,]
twelve = link[link$Activity == 12,]
thirteen = link[link$Activity == 13,]
plot(one$Duration,one$Revenue,col=one$Activity)
plot(two$Duration,two$Revenue,col=two$Activity)
plot(three$Duration,three$Revenue,col=three$Activity)
plot(four$Duration,four$Revenue,col=four$Activity)
summary(four[four$Duration %% 2 == 0,])
dim(five[five$Revenue == 0,])
dim(five[five$Revenue != 0,])
fourALT = four
fourALT$nonzero <- FALSE
fourALT[four$Revenue > 0,]$nonzero = TRUE
four
tree <- rpart(Revenue~.,data=fourALT)
rpart.plot(tree)

intersect(four[four$Revenue == 0,]$Duration, four[four$Revenue != 0,]$Duration)
intersect(five[five$Revenue == 0,]$Duration, five[five$Revenue != 0,]$Duration)
intersect(six[six$Revenue == 0,]$Duration, six[six$Revenue != 0,]$Duration)


plot(five$Duration,five$Revenue,col=five$Activity)
plot(six$Duration,six$Revenue,col=six$Activity)
plot(seven$Duration, seven$Revenue,col=seven$Activity)
plot(eight$Duration, eight$Revenue,col=eight$Activity)
plot(nine$Duration, nine$Revenue,col=nine$Activity)
plot(ten$Duration,ten$Revenue,col=ten$Activity)
plot(eleven$Duration,eleven$Revenue,col=eleven$Activity)
plot(twelve$Duration,twelve$Revenue,col=twelve$Activity)
plot(thirteen$Duration,thirteen$Revenue,col=thirteen$Activity)







par(mfrow=c(2,1))
plot(data$Duration,data$Revenue,col=data$Source)
plot(data$Duration,pred,col=data$Source)

library(rpart)
library(rpart.plot)
tree <- rpart(Revenue~.,data=link)
rpart.plot(tree)
p =  predict(tree, newdata = link)






#RPART
library(rpart)
library(rpart.plot)
tree <- rpart(Revenue~.,data=data, control = rpart.control(cp = 0))
rpart.plot(tree)
p =  predict(tree, newdata = data)
# 17.52254

for(i in seq(1, 21, by = 1)) {
  sub = data[data$Source == "GoogleSearch" & data$Duration > 37 & data$Activity == i,]
  model.lm <- lm(Revenue~ Duration, data = sub)
  pred =  predict(model.lm, newdata = sub)
  p[data$Source == "GoogleSearch" & data$Duration > 37 & data$Activity == i] = pred
}
mean((p-data$Revenue)^2)

## predict it

testFile <- read.csv('P5_test1_students.csv', sep = ",")
tree <- rpart(Revenue~.,data=data)
rpart.plot(tree)
p =  predict(tree, newdata = testFile)
for(i in seq(1, 21, by = 1)) {
  sub = data[data$Source == "GoogleSearch" & data$Duration > 37 & data$Activity == i,]
  model.lm <- lm(Revenue~ Duration, data = sub)
  subTest = testFile[testFile$Source == "GoogleSearch" & testFile$Duration > 37 & testFile$Activity == i,]
  pred =  predict(model.lm, newdata = subTest)
  p[testFile$Source == "GoogleSearch" & testFile$Duration > 37 & testFile$Activity == i] = pred
}
p
sub <- read.csv('revenue_submission.csv', sep = ",")
sub$Revenue <- p
write.csv(sub, file = "mysubmission.csv",row.names=FALSE)


summary(data)




split_ratio <- 0.75
sample_size <- floor(split_ratio * nrow(data))
iteration <- 10
all_errors <- c()
for (val in c(1:iteration)){
  train_ind <- sample(seq_len(nrow(data)), size = sample_size)
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  tree <- rpart(Revenue~., data = train)
  p <- predict(tree,test)
  length(p)
  for(i in seq(1, 21, by = 1)) {
    sub = train[train$Source == "GoogleSearch" & train$Duration > 37 & train$Activity == i,]
    model.lm <- lm(Revenue~ Duration, data = sub)
    subTEST = test[test$Source == "GoogleSearch" & test$Duration > 37 & test$Activity == i,]
    pred =  predict(model.lm, newdata = subTEST)
    dim(pred)
    dim(test[test$Source == "GoogleSearch" & test$Duration > 37 & test$Activity == i,])
    p[test$Source == "GoogleSearch" & test$Duration > 37 & test$Activity == i] = pred
  }
  MSE <- mean((p-test$Revenue)^2)
  print(MSE)
  all_errors <- c(all_errors, MSE)
}
print(all_errors)
print(mean(all_errors))








#RandomForest
library(randomForest)
perf.rf = randomForest::randomForest(Revenue ~ ., data = data)
pred =  predict(perf.rf, newdata = data)
mean((pred-data$Revenue)^2) 
# 20.03397

#SVM
install.packages("e1071")
library(e1071)
perf.svm = svm(Revenue ~ ., data = data)
pred =  predict(perf.svm, newdata = data)
mean((pred - data$Revenue)^2)
#27.39045
