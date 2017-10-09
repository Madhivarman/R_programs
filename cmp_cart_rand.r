"This program shows the variance between CART and Random Forest Model"

#install require packages
installed.packages("googleVis")
#iris dataset is used
data <- iris
#summary 
summary(iris)
#create a data frame
df <- data.frame(iris$Petal.Length,iris$Petal.Width)
#plot
chart <-gvisScatterChart(df,options = list(
    title = "Iris Dataset",
    legend = "right",
    pointSize = 15,
    series = 0,
    width = 600,height = 600
))

plot(chart)

# three specimen can be well seggreagated then
# dividing the set into 50 50 into three models
# there are 3 species which exactly 50 is present
# the model will take 25 for training
library(rpart)
library(caret)

train.flag <- createDataPartition(y=iris$Species,p=0.5,list=FALSE)
training <- iris[train.flag,]
validation <- iris[-train.flag,]

modfit <- train(Species ~. ,method="rpart",data=training)

library(rattle)

fancyRpartPlot(modfit$finalModel)

#validating that model

train.cart <- predict(modfit,newdata=training)
table(train.cart,training$Species)

#while running table we can see misclassification rate is 3/75
#Misclassfication rate = 3/75

pred.cart <- predict(modfit,newdata = validation)
table(pred.cart,training$Species)

#while running above we can see misclassification rate is 5/75
#Missclassfication rate  = 5/75

#now we can find where the prediction went wrong
correct <- pred.cart == validation$Species

#qplot
# in graph red color dots is where misplaced is happened
qplot(Petal.Length,Petal.Width,color = correct,data = validation)

#the above model is the CART Model which there is 5 misclassification out of 5/75

#the below model we gonne use is randomForest Model
#install randomForest library

library(randomForest)
library(randomForestSRC)

#train the modal
modfit <- train(Species ~.,  method="rf",data=training)

#predict the classification
pred <- predict(modfit,training)
table(pred,training$Species)

#you can see above model rf has no misclassification of the
#data model
train.cart <- predict(modfit,newdata = training)
table(train.cart,training$Species)

#measure
rf_correct <- train.cart == validation$Species
#Missclassification rate 0/75
qplot(Petal.Length,Petal.Width,color= rf_correct,data=training)

