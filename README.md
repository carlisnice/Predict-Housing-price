# Predict-Housing-Price

Shrinkage Methods
The methods we choose to apply on our model are ridge and lasso regression. These methods are very similar, and they are both regulated by adding a penalty term to the sum of squared residuals. 

Lasso regression
Lasso regression performs two main tasks: regularization and variable selection. According to the formula above, Lasso applies a regularization or shrinkage process where it penalizes the coefficients of the regressors all the way to zero. Other non-zero coefficients after variable selection will be selected to be parts of the model. That is due to the strength of the penalty which is controlled by the tuning parameter λ. The larger parameter λ is, the more numbers of coefficients are shrunk to zero. 

Ridge regression
Ridge regression is another shrinkage method. In the case of variable selection, it cannot reduce the numbers of parameters in the model. However, it does shrink the coefficients of the regressors towards zero, but never equal to zero. Evidently, it performs better than Lasso regression when most of the explanatory variables are highly correlated. Furthermore, this method is widely used tool to the problem of multicollinearity. It can effectively eliminate collinearity, improve accuracy, and provide more interpretable parameter estimates. 
