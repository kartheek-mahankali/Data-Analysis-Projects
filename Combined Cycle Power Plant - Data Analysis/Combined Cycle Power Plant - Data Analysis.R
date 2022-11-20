#########################################################################
#########################################################################
# Combined Cycle Power Plant Data Set
#########################################################################
#########################################################################
# Installing required libraries
#install.packages("readxl")
#install.packages("leaps")
library("readxl")
library("leaps")
library("caret")
library("boot")
library("glmnet")
library("pls")
# Load Dataset into a dataframe named my_data
my_data <- read_excel("D:/CCPP.xlsx")
summary(my_data)

# Let us log transform the data, as it is mentioned in the data description
#that the variables are not normalised
log_tran_my_data = log(as.data.frame(my_data))
summary(log_tran_my_data)

#####################################################
# Subset and Model Selection
#####################################################
# Using Validation set approach and CV for selecting models

# On Training subset
set.seed (1)
train <- sample(c(TRUE , FALSE), nrow(log_tran_my_data),
                replace = TRUE)
test <- (!train)
regfit.best <- regsubsets(PE ~ .,
                          data = log_tran_my_data[train , ], nvmax = 4)
test.mat <- model.matrix(PE ~ ., data = log_tran_my_data[test , ])
val.errors <- rep(NA, 4)
for (i in 1:4) {
  coefi <- coef(regfit.best , id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean (( log_tran_my_data$PE[test] - pred)^2)
}
val.errors

which.min(val.errors)

# Hence, we select the 4 variable model, as it gives least Test MSE 0.0001060837

coef(regfit.best , 4)

# We now know that the 4 variables we got to select are AT, V, AP, RH

# perform best subset selection within each of the k training sets
predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}
k <- 10
n <- nrow(log_tran_my_data)
set.seed (1)
folds <- sample(rep (1:k, length = n))
cv.errors <- matrix(NA, k, 4,
                    dimnames = list(NULL , paste (1:4)))


for (j in 1:k) {
  best.fit <- regsubsets(PE ~ .,
                         data = log_tran_my_data[folds != j, ],
                         nvmax = 4)
  for (i in 1:4) {
    pred <- predict(best.fit , log_tran_my_data[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean (( log_tran_my_data$PE[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors , 2, mean)
mean.cv.errors
# From above result, select the model with least MSE and apply best subset
# selection for that model

reg.best <- regsubsets(PE ~ ., data = log_tran_my_data ,
                       nvmax = 4)
summary(reg.best)
# We see that the 4 variable model gives least MSE as 0.0001090167
coef(reg.best , 4)

#####################################################
# Ridge Regression
#####################################################
x <- model.matrix(PE ~ ., log_tran_my_data)[, -1]
y <- log_tran_my_data$PE
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))
set.seed (1)
train <- sample (1: nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

## Now lets find best lambda value using CV

set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

# We got the best lambda value as 0.003453709
# Let us now find out the test MSE at the best lambda value
ridge.pred <- predict(ridge.mod , s = bestlam ,
                      newx = x[test , ])
mean (( ridge.pred - y.test)^2)

# TEST MSE using Ridge Regression with lambda chosen by CV is 0.0001361383

#####################################################
# Lasso Regression
#####################################################
lasso.mod <- glmnet(x[train , ], y[train], alpha = 1, 
                    lambda = grid)
plot(lasso.mod)
set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
# We got the best lambda value as 0.0001184784
lasso.pred <- predict(lasso.mod , s = bestlam ,       
                      newx = x[test , ])
mean (( lasso.pred - y.test)^2)
# TEST MSE using Lasso Regression with lambda chosen by CV is 0.0002372278

#####################################################
# Principal Component Regression
#####################################################

set.seed (1)
pcr.fit <- pcr(PE ~ ., data = log_tran_my_data , subset = train ,
               scale = TRUE , validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit , val.type = "MSEP")
pcr.pred <- predict(pcr.fit , x[test , ], ncomp = 4)
mean (( pcr.pred - y.test)^2)

# TEST MSE using Principal Component Regression is 0.0001082877

#####################################################
# Partial Least Squares
#####################################################
set.seed (1)
pls.fit <- plsr(PE ~ ., data = log_tran_my_data , subset = train ,
                scale = TRUE , validation = "CV")
summary(pls.fit)
validationplot(pls.fit , val.type = "MSEP")
pls.pred <- predict(pls.fit , x[test , ], ncomp = 4)
mean (( pls.pred - y.test)^2)

# TEST MSE using Principal Component Regression is 0.0001082877

#####################################################
# Leave One Out Cross Validation
#####################################################

glm.fit <- glm(PE ~ . , data = log_tran_my_data)
cv.err <- cv.glm(log_tran_my_data , glm.fit)
cv.err$delta   # this gives us the cross validation estimate for the test error

# Test MSE using LOOCV is 0.0001090107

#####################################################
# k Fold Cross Validation (k=5)
#####################################################

# Linear Model
set.seed(15)
cv.error.5 <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(PE ~ ., data = log_tran_my_data)
  cv.error.5[i] <- cv.glm(log_tran_my_data , glm.fit , K = 5)$delta [1]
}
cv.error.5
which.min(cv.error.5)
summary(glm.fit)
#with(summary(glm.fit), 1 - deviance/null.deviance)

#Polynomial Model

set.seed (20)
cv.error.5 <- rep(0, 5)
for (i in 1:5) {
  glm.fit <- glm(PE ~ poly(AT,i) + poly(V,i) + poly(RH,i) + poly(AP,i), data = log_tran_my_data)
  cv.error.5[i] <- cv.glm(log_tran_my_data , glm.fit , K = 5)$delta [1]
}
cv.error.5
which.min(cv.error.5)
summary(glm.fit)
#with(summary(glm.fit), 1 - deviance/null.deviance)

#####################################################
# k Fold Cross Validation (k=10)
#####################################################

# Linear Model
set.seed (12)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(PE ~ ., data = log_tran_my_data)
  cv.error.10[i] <- cv.glm(log_tran_my_data , glm.fit , K = 10)$delta [1]
}
cv.error.10
which.min(cv.error.10)
summary(glm.fit)
#with(summary(glm.fit), 1 - deviance/null.deviance)

#Polynomial Model

set.seed (30)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(PE ~ poly(AT,i) + poly(V,i) + poly(RH,i) + poly(AP,i), data = log_tran_my_data)
  cv.error.10[i] <- cv.glm(log_tran_my_data , glm.fit , K = 10)$delta [1]
}
cv.error.10
which.min(cv.error.10)
summary(glm.fit)
#with(summary(glm.fit), 1 - deviance/null.deviance)


#########################################################################
#########################################################################

