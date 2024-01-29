set.seed(100)
library(ISLR)
library(finalfit)
data("Hitters")
View(Hitters)

## Checking data structure, first few observation, & if any NA

is.na(Hitters)

names(Hitters)

glimpse(Hitters)
missing_glimpse(Hitters)
ff_glimpse(Hitters)
head(Hitters)
Hitters<-na.omit(Hitters)
## 75% of the sample size

smp_size <- floor(0.75 * nrow(Hitters))
train_ind <- sample.int(nrow(Hitters), size = smp_size)

# train_ind <- sample(seq_len(nrow(Hitters)), size = smp_size)

train <- Hitters[train_ind, ]
test <- Hitters[-train_ind, ]

## We estimate basic model with all predictors/feature

mod1 <- lm(Salary~., data=train)
summary(mod1)

mod1.pred <- predict(mod1, new_data=test)
mod_mse <- mean((mod1.pred - test$Salary)^2)
mod_mse


#ML: Regularized regression techniques: Lasso, 
#Ridge Regression, and Elastic Net



##ridge regression on the Hitters data training set.


library(glmnet) 
library(coefplot)
set.seed(100)
std <- TRUE  # Required for Lasso
Y <- train$Salary
X <- model.matrix(Salary~.,train)[,-1]
# 10-fold CV to find the optimal lambda 
ridge.cv=cv.glmnet(X, Y, alpha=0, type="deviance", family="gaussian", standardize=std, nfolds=10)
## Fit lasso model with 100 values for lambda
ridge_mdl = glmnet(X, Y, alpha=0, nlambda=100, standardize=std, family="gaussian")
## Extract coefficients at optimal lambda
coef(ridge_mdl,s=ridge.cv$lambda.min)


plot(ridge_mdl, xvar="lambda", lwd=2, label=T)
abline(v=log(ridge.cv$lambda.min), col="blue", lty=2)
coefpath(ridge_mdl)
coefplot(ridge_mdl, lambda=ridge.cv$lambda.min, sort="magnitude")


##We can re-estimate the same steps for Lasso regression. 
#This time we set the value of α to 1 to force the model to use Lasso.

# 10-fold CV to find the optimal lambda 
lasso.cv=cv.glmnet(X,Y,alpha=1, type="deviance", family="gaussian", standardize=std, nfolds=10)
## Fit lasso model with 100 values for lambda
lasso_mdl = glmnet(X,Y,alpha=1,family="gaussian", standardize=std, nlambda=100)
## Extract coefficients at optimal lambda
coef(lasso_mdl,s=lasso.cv$lambda.min)


plot(lasso_mdl, xvar="lambda", lwd=2, label=T)
abline(v=log(lasso.cv$lambda.min), col="blue", lty=2)

coefpath(lasso_mdl)
coefplot(lasso_mdl, lambda=lasso.cv$lambda.min, sort="magnitude")



## Elastic net


# 10-fold CV to find the optimal lambda 
enet.cv=cv.glmnet(X,Y,alpha=0.5, type="deviance", family="gaussian", standardize=std, nfolds=10)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(X,Y,alpha=0.5,standardize=std,nlambda=100)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)


plot(enet_mdl, xvar="lambda", lwd=2, label=T)
abline(v=log(enet.cv$lambda.min), col="blue", lty=2)


coefplot(enet_mdl, lambda=enet.cv$lambda.min, sort="magnitude")
coefpath(enet_mdl)


## Compare models and take the decisions


tmatrix <- model.matrix(Salary~.,test)[,-1]
ridge.pred = predict(ridge_mdl, s=ridge.cv$lambda.min, newx=tmatrix)
ridge_mse = mean((ridge.pred-test$Salary)^2)
ridge_mse


lasso.pred = predict(lasso_mdl, s=lasso.cv$lambda.min, newx=tmatrix)
lasso_mse = mean((lasso.pred-test$Salary)^2)
lasso_mse

enet.pred = predict(enet_mdl, s=enet.cv$lambda.min, newx=tmatrix)
enet_mse = mean((enet.pred-test$Salary)^2)


enet_mse


barplot( c(ridge_mse, lasso_mse, enet_mse, mod_mse), 
         main="Mean Squared Error", 
         names.arg=c('Ridge','Lasso','Elastic Net','Full'))






