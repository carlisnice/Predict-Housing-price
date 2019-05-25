#Data cleaning--------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("~/Desktop/machine learning")
detach("package:MASS", unload=TRUE)
library(e1071)
library(dplyr)
newdata=read.csv(file="kc_house_data.csv", header=TRUE)
newdata=newdata[c("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","condition","grade","yr_built","yr_renovated","lat","long")]
newdata1=data.frame(newdata)
newdata1=newdata1[-c(1720,15871),]
newdata1$waterfront=as.factor(newdata1$waterfront)


#number of bathroom per house
newdata1$bathrooms=newdata1$bathrooms*newdata1$bedrooms
 
#delete missing value
newdata1=na.omit(newdata1)
#delete bathrooms=0 or bedrooms=0
newdata1=newdata1[-which(newdata1$bathrooms == 0 | newdata1$bedrooms==0), ]


#replace yr_built if yr_renovated greater than
for (i in 1:nrow(newdata1)){
  if(newdata1$yr_renovated[i]!=0)
    newdata1$yr_built[i]=newdata1$yr_renovated[i]
}
newdata1$age=as.integer(2018-newdata1$yr_built)
#delete yr_renovated
newdata1=select(newdata1, -yr_renovated,-yr_built,-grade)
str(newdata1)



#-----------------------------------------------------------------------------------------------------------
#split data
set.seed(2)
library(caTools)
split<-sample.split(newdata1$price,SplitRatio = 0.7)
training_data<-subset(newdata1,split=="TRUE")
testing_data<-subset(newdata1,split=="FALSE")

#Exploratory Analysis

cr=cor(select(newdata1,-waterfront))
cr
# to visualize the same 
library(corrplot)
corrplot(cr,type="lower")
corrplot(cr,method="number")
#looks like price and sqft_living is positively correlated


#Multicollinearity exists when two or more predictor are highly correlated among themselves.
#when correlation among X's is low, OLS has lots of information to estimate
#when correlation among X's is high, OLS has very little information to estimate
#a vif of 1 means that there is no correlation among variables

library(car)
model=lm(price~., data=training_data)
vif(model)


library(MASS)
library(sp)
library(rworldmap)
library(rworldxtra)
quantile(sort(newdata1$price))
q1=filter(newdata1,price<322000)
q2=filter(newdata1,price>=322000 & price<450000)
q3=filter(newdata1,price>=450000 & price<645000)
q4=filter(newdata1,price>=645000 & price<800000)
q5=filter(newdata1,price>=800000)


#We can see that the housing price around city center area is relatively high. So location is also an 
#important factor that will affect the price.
par(mar=c(6,4,4,2))
worldmap = getMap(resolution = "high")
NrthAm = worldmap[which(worldmap$REGION == "North America"), ]
plot(NrthAm, xlim = c(-122.519, -121.315), ylim = c(47.1559,47.7776), col = "white", bg = "lightblue",
     main = "Location of the house")
points(q1$long, q1$lat, pch = ".",col=1,cex=2)
points(q2$long, q2$lat, pch = ".",col=3,cex=2)
points(q3$long, q3$lat, pch = ".",col=4,cex=2)
points(q4$long, q4$lat, pch = ".",col=5,cex=2)
points(q5$long, q5$lat, pch = ".",col=6,cex=2)
points(-121.983,47.548, pch="*",col="red",cex=3)
legend("bottomright", col = c(2,1,3,4,5,6), pch=c("*",".",".",".",".","."),
       c("center","price from 0-322000","price from 322000-450005","price from 450000-655000",
         "price from 655000-800000", "price greater than 800000"),cex=0.6,box.lty = 0,pt.cex=3)
est2 = kde2d(newdata1$long, newdata1$lat,n = c(121, 150))
contour(est2, add = TRUE, col = 1, lwd = 1)

##-----------------------------------------------------------------------------------------------------------
# Custom Control Parameters
#use 10 fold cross validation, training data is divided into 10 parts and then the model
#is made from 9 parts and the remain part is used for error estimation.
#This process is repeated 10 times with different part used for error estimation

library(caret)
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = F)
# Linear Model
set.seed(1234)
lm <- train(price~.,training_data,method="lm",trControl=custom)
lm
# Results
lm$results
summary(lm)
plot(lm$finalModel)

# Ridge Regression
set.seed(1234)
ridge <- train(price~.,training_data,method="glmnet",
               tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,1,length=10)),
               trControl=custom)

# Plot Results
plot(ridge)
plot(ridge$finalModel, xvar = "lambda", label = T)
plot(ridge$finalModel, xvar = 'dev', label=T)
plot(varImp(ridge, scale=T))


# Lasso Regression
set.seed(1234)
lasso <-  train(price~.,training_data,method="glmnet",
                tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,1,length=10)),
                trControl=custom)

# Plot Results
plot(lasso)
plot(lasso$finalModel, xvar = 'lambda', label=T)
plot(lasso$finalModel, xvar = 'dev', label=T)
plot(varImp(lasso))

ridge
lasso

# Elastic Net Regression
set.seed(1234)
en <- train(price~.,training_data,method="glmnet",
            tuneGrid=expand.grid(alpha=seq(0,1,length=10),lambda=seq(0.0001,1,length=5)),
            trControl=custom)

# Plot Results
plot(en)
plot(en$finalModel, xvar = 'lambda', label=T)
plot(en$finalModel, xvar = 'dev', label=T)
plot(varImp(en))

# Compare Models
model_list <- list(LinearModel=lm,Ridge=ridge,Lasso=lasso, ElasticNet=en)
res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res,metric = "RMSE")
#looks like elastic net performs the best from their RMSE
# Best Model
en$bestTune
best <- en$finalModel
coef(best, s = en$bestTune$lambda)


# Save Final Model for Later Use
saveRDS(en, "final_model.rds")
fm <- readRDS("final_model.rds")
print(fm)

# Prediction using en
p1 <- predict(fm, testing_data)
sqrt(mean((testing_data$price-p1)^2))

# Prediction using lm
p2<-predict(lm,testing_data)
sqrt(mean((testing_data$price-p2)^2))


