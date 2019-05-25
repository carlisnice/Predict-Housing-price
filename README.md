# Predict-Housing-Price

Regarding all the factors affecting the housing price, some argue locations are the primary reason and the other say that lot size is dominating among other factors. The answers are diversified and complex. However, due to the subjective impressions of the public, they do not have enough evidences or theatrical reasons for the influential factors. The purpose of this project is to perform objective analysis of the housing price in King County,Washington, by building a multiple linear regression model, and shrinkage methods.


Shrinkage Methods
The methods we choose to apply on our model are ridge and lasso regression. These methods are very similar, and they are both regulated by adding a penalty term to the sum of squared residuals. 

Lasso regression
Lasso regression performs two main tasks: regularization and variable selection. According to the formula, Lasso applies a regularization or shrinkage process where it penalizes the coefficients of the regressors all the way to zero. Other non-zero coefficients after variable selection will be selected to be parts of the model. That is due to the strength of the penalty which is controlled by the tuning parameter λ. The larger parameter λ is, the more numbers of coefficients are shrunk to zero. Therefore, if most of explainary variables are insignificant, Lasso regression tends to do better estimation.

Ridge regression
Ridge regression is another shrinkage method. In the case of variable selection, it cannot reduce the numbers of parameters in the model. However, it does shrink the coefficients of the regressors towards zero, but never equal to zero. Evidently, it performs better than Lasso regression when most of the explanatory variables are highly correlated. Furthermore, this method is widely used tool to the problem of multicollinearity. It can effectively eliminate collinearity, improve accuracy, and provide more interpretable parameter estimates. 

Elastic net regression
Elastic net regression is the a combination of Lasso and ridge regression penalties with two tuning parameters λ: λ1 for Lasso, λ2 for Ridge. We use cross validation on differnet combination of λ1 and λ2 to find the best value. It has the strength of the other two shrinkage methods and is used if we do not know which explainatory variable is insignificant in the to our response variable in advance.
