library(tidyverse)
library(broom)
library(glmnet)
library(randtoolbox)
library(RandomFields)

#Generate a toy spatial dataset
set.seed(123)
n = 500
coords <- matrix(runif(2*n), n) 
X <- scale(coords, center=T)

#Another example using a gaussain kernel
#ss=0.1
#model <- RMgauss(scale=ss)
#y<- matrix(RFsimulate(model, x=X)$variable1) +rnorm(n,0,0.5)

y <- matrix(X[,1]^2 + X[,2]^2)+rnorm(n, 0, 1) #Latent function with gaussian noise

col <- terrain.colors(100)[as.numeric(cut(y,breaks = 100))]
plot(X,col=col,pch=16)
mean(y)
sd(y)


#Divide our data up into training & testing data 
train_rows <- sample(1:n, 0.2*n) 
x.train <- X[train_rows, ]
x.test <- X[-train_rows, ]
y.train <- y[train_rows, ]
y.test <- y[-train_rows, ]


#Linear Model
fit_lin <- glmnet(x.train, y.train, alpha=0, lambda = 0)
ytrain_lin <- predict(fit_lin, s=0, newx=x.train)
ytest_lin <- predict(fit_lin, s=0, newx=x.test)
train_lin <- mean((y.train - ytrain_lin)^2)
test_lin <- mean((y.test - ytest_lin)^2)


# RFF mapping of the input data
m <- 300 #Number of features for the RFF
Omega = matrix(rnorm(m*ncol(X)), m) # squared exponential kernel
Proj = X %*% t(Omega) # projection
Phi = (cbind(cos(Proj), sin(Proj)) / sqrt(m))
phi.train <- Phi[train_rows, ]
phi.test <- Phi[-train_rows, ]


# Non_linear Regression Model unsig RFF (no regularisation)
fit_krr <- glmnet(phi.train, y.train, alpha=0, lambda=0)
ytrain_krr <- predict(fit_krr, s=0, newx=phi.train)
ytest_krr <- predict(fit_krr, s=0, newx=phi.test)
train_krr <- mean((y.train - ytrain_krr)^2)
test_krr <- mean((y.test - ytest_krr)^2)

#kernel Ridge regression Model using RFF
lambdas <- 10^seq(3, -5, by = -.1)
cv_rff <- cv.glmnet(phi.train, y.train,  alpha=0, lambda = lambdas)
plot(cv_rff)
opt_rff <- cv_rff$lambda.min
opt_rff # Optimal lambda chosen by cross-validation

fit_rff <- glmnet(phi.train, y.train, alpha=0, lambda= opt_rff)
ytrain_rff <- predict(fit_rff, s=0, newx=phi.train)
ytest_rff <- predict(fit_rff, s=opt_rff, newx=phi.test)
train_rff <- mean((y.train - ytrain_rff)^2)
test_rff <- mean((y.test - ytest_rff)^2)


#Comparison of model performance
train_lin
test_lin

train_krr
test_krr

train_rff
test_rff


