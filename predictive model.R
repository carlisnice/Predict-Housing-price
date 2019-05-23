rm(list=ls())
library(e1071)

library(car)
library(dplyr)
newdata=read.csv(file="kc_house_data.csv", header=TRUE)
newdata=newdata[c("price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","condition","grade","yr_built","yr_renovated","lat","long")]
newdata1=data.frame(newdata)
newdata1=newdata1[-c(1720,15871),]
newdata1$floors=as.factor(newdata1$floors)
newdata1$condition=as.factor(newdata1$condition)
newdata1$waterfront=as.factor(newdata1$waterfront)

#number of bathroom per house
newdata1$bathrooms=newdata1$bathrooms*newdata1$bedrooms

#delete missing value
newdata1=na.omit(newdata1)
#delete bathrooms=0 or bedrooms=0
newdata1=newdata1[-which(newdata1$bathrooms == 0 | newdata1$bedrooms==0), ]


#replace yr_built if yr_renovated greater than
for (i in 1:21593){
  if(newdata1$yr_renovated[i]!=0)
    newdata1$yr_built[i]=newdata1$yr_renovated[i]
}
newdata1$age=2018-newdata1$yr_built
#delete yr_renovated
newdata1=select(newdata1, -"yr_renovated",-"yr_built",-"grade")
str(newdata1)

names(newdata1)

y=newdata1$price
x1=newdata1$bedrooms
x2=newdata1$bathrooms
x3=newdata1$sqft_living
x4=newdata1$sqft_lot
x5=newdata1$floors
x6=newdata1$waterfront
x7=newdata1$condition
x8=newdata1$age
x9=newdata1$lat
x10=newdata1$long
full_model=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
summary(full_model)
str(newdata1)

par(mfrow=c(1,1))
fit1=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
stud_res1=studres(fit1)
yhat1=fit1$fitted.values
summary(fit1)
vif(fit1)

fit2<-stepAIC(fit1,direction="both")          #all AIC are 24405.19
stepAIC(fit1,direction="backward") 
fitnull=lm(y~1)
step(fitnull,scope=list(lower=fitnull,upper=fit1),direction="forward")  

summary(fit2)
##one more bedroom will result in approx. 30k lower in price
plot(x1,y)

#y is not normal at all from the Q-Q Plot,so we transfrom
qqnorm(y)
qqline(y,col="red")

#boxcox transformation
b=boxcox(fit2, lambda = seq(-2, 2, 1/10))
best.lam=b$x[which(b$y==max(b$y))]        #best.lam=-0.0606
yp=y^(best.lam)

#yp is pretty normal
qqnorm(yp)
qqline(yp,col="red")

#aic selection model
fit3=lm(yp~x1+x2+x3+x5+x6+x7+x8+x9+x10)
summary(fit3)
plot(fit3)


#produce studentized residual of the final model
stud_res3=studres(fit3)
yhat3=fit3$fitted.values
plot(yhat3,stud_res3)

par(mfrow=c(1,2))
#compare the probability plot(nomality is good)
probplot(stud_res1)
probplot(stud_res3)


#residual VS fitted value(constant variance is not bad)
plot(yhat1,stud_res1,xlab="fitted value",ylab="studentized residual",main="full model")
abline(h=0,col="red")
plot(yhat3,stud_res3,xlab="fitted value",ylab="studentized residual",main="final model")
abline(h=0,col="red")







library(glmnet)
set.seed(123)
#x is a matrix of data use in Ridge, Lasso, Elastic-Net Regression to predict values in y 
# x = model.matrix(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10-1,data=newdata1)
# y = newdata1$price
# #divide data into training and testing sets
# #sample function randomly selects numbers b/w 1 and number of rows in x 
# #and it will select .66*n row numbers which means 2/3 of the data in training set
# train_rows=sample(1:nrow(x), .66*nrow(x))
# x_train=x[train_rows,] #just contains the training data
# x_test=x[-train_rows,] #testing set for x
# y_train=y[train_rows]
# y_test=y[-train_rows]
# 
# 
# #lasso
# lasso_model=glmnet(x,y,alpha=1)
# plot(lasso_model, xvar="lambda", label=TRUE)
# alpha1_fit=cv.glmnet(x_train, y_train, type.measure = "mse", alpha=1)
# alpha1_predicted=predict(alpha1_fit, s=alpha1_fit$lambda.1se, newx = x_test)
# #if dont do ridge, elastic-net comparison, then use lambda.min for s
# #alpha1_predicted=predict(alpha1_fit, s=alpha1_fit$lambda.min, newx = x_test)
# 
# #ridge
# alpha2_fit=cv.glmnet(x_train,y_train, type.measure = "mse", alpha=0)
# alpha2_predicted=predict(alpha2_fit, s=alpha2_fit$lambda.1se, newx = x_test)
# 
# #mean squared error
# mean_lasso=mean((y_test-alpha1_predicted)^2)
# mean_ridge=mean((y_test-alpha2_predicted)^2)
# mean_lasso
# mean_ridge
# #lasso performs better
# 
# 
# 
# plot(alpha1_fit)
# best_lambda=alpha1_fit$lambda.1se
# min_lambda=alpha1_fit$lambda.min
# best_lambda
# min_lambda
# coef(alpha1_fit,s=best_lambda)
# 
# 
# lasso_coef=predict(lasso_model,type="coefficients",s=best_lambda)[1:18,]
# lasso_coef[lasso_coef!=0]
# lasso_coef
# #x3, x6, x9
# 
# #R^2
# lasso_mod=glmnet(x,y,alpha=1,lambda=best_lambda)
# lasso_mod$dev.ratio
# 
# final_model=lm(y~x3+x6+x9)
# PRESS(final_model)

#compare mse, smaller the mse, R^2 tends to be larger 

#adj R^2



x1 = model.matrix(yp~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10-1,data=newdata1)
#yp
#divide data into training and testing sets
#sample function randomly selects numbers b/w 1 and number of rows in x 
#and it will select .66*n row numbers which means 2/3 of the data in training set
train_rows1=sample(1:nrow(x1), .66*nrow(x1))
x_train1=x1[train_rows1,] #just contains the training data
x_test1=x1[-train_rows1,] #testing set for x
y_train1=yp[train_rows1]
y_test1=yp[-train_rows1]


#lasso
lasso_model1=glmnet(x1,yp,alpha=1)
plot(lasso_model1, xvar="lambda", label=TRUE)
alpha1_fit1=cv.glmnet(x_train1, y_train1, type.measure = "mse", alpha=1)
alpha1_predicted1=predict(alpha1_fit1, s=alpha1_fit1$lambda.1se, newx = x_test1)
#if dont do ridge, elastic-net comparison, then use lambda.min for s
#alpha1_predicted=predict(alpha1_fit, s=alpha1_fit$lambda.min, newx = x_test)

#ridge
alpha2_fit1=cv.glmnet(x_train1,y_train1, type.measure = "mse", alpha=0)
alpha2_predicted1=predict(alpha2_fit1, s=alpha2_fit1$lambda.1se, newx = x_test1)

#mean squared error
mse_lasso=mean((y_test1-alpha1_predicted1)^2)
mse_ridge=mean((y_test1-alpha2_predicted1)^2)
mse_lasso
mse_ridge
#lasso performs better


plot(alpha1_fit1)
best_lambda1=alpha1_fit1$lambda.1se
min_lambda1=alpha1_fit1$lambda.min
best_lambda1
min_lambda1
coef(alpha1_fit1,s=best_lambda1)
coef(alpha2_fit1,.s=best_lambda1)

lasso_coef1=predict(lasso_model1,type="coefficients",s=best_lambda1)[1:19,]
lasso_coef1[lasso_coef1!=0]
lasso_coef1

#ridge
best_lambda2=alpha2_fit1$lambda.1se
min_lambda2=alpha2_fit1$lambda.min
best_lambda2
min_lambda2
ridge_model=glmnet(x1,yp,alpha=0)
ridge_coef=predict(ridge_model,type="coefficients",s=best_lambda2)[1:19,]
ridge_coef[ridge_coef!=0]


#xx2 is matrix with variables selected from AIC w/o variable x4
xx2 = model.matrix(fit3)[,-1]
#divide data into training and testing sets
#sample function randomly selects numbers b/w 1 and number of rows in x 
#and it will select .66*n row numbers which means 2/3 of the data in training set
train_rows2=sample(1:nrow(xx2), .66*nrow(xx2))
x_train2=xx2[train_rows2,] #just contains the training data
x_test2=xx2[-train_rows2,] #testing set for x
y_train2=yp[train_rows2]
y_test2=yp[-train_rows2]

x2_set=data.frame(xx2)
x2train_set=data.frame(x_train2)
x2test_set=data.frame(x_test2)
fit3
AIC_fit=lm(y_train2~x1+x2+x3+x51.5+x52+x52.5+x53+x53.5+x61+x72+x73+x74+x75+x8+x9+x10, data=x2train_set)
AIC.predicted=predict(AIC_fit, x2test_set)


mse_AICs=mean((y_test2-AIC.predicted)^2)
#mse for AIC, LASSO, RIDGE
#conclude lasso and ridge is way better than AIC selection model 
mse_AICs
mse_lasso
mse_ridge             
#compare mse, smaller the mse, R^2 tends to be larger

