#Oct. 30, 2020 Machine Learning Class

setwd("~/Documents/NC State/Fall I/Linear Algebra")
library(leaps) #stepwise selection
library(glmnet) #lasso/ridge/elastic net
hitters <- load("hitters.RData")
Hitters = na.omit(Hitters) # omit rows with missing values


set.seed(7515)
train=sample(c(T,F), nrow(Hitters), rep=TRUE, p=c(0.6,0.4))
test=!train
regfit.fwd = regsubsets(Salary ~ ., data=Hitters[train,], nvmax=19, method="forward")
regfit.bwd = regsubsets(Salary ~ ., data=Hitters[train,], nvmax=19, method="backward")
summary(regfit.fwd)
summary(regfit.bwd)

regfit.fwd$vorder
regfit.bwd$vorder

coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

X = model.matrix(Salary~., data=Hitters[test, ])
fwd.val.MSE=vector()
bwd.val.MSE=vector()
for (i in 1:19) {
   beta.fwd = coef(regfit.fwd, i)
   beta.bwd = coef(regfit.bwd, i)
   pred.fwd = X[,names(beta.fwd)] %*% beta.fwd
   pred.bwd = X[,names(beta.bwd)] %*% beta.bwd
   fwd.val.MSE[i] = mean((Hitters$Salary[test] - pred.fwd)^2)
   bwd.val.MSE[i] = mean((Hitters$Salary[test] - pred.bwd)^2)
   }

min(fwd.val.MSE)
min(bwd.val.MSE)

plot(1:19,fwd.val.MSE,
      type='b',
      pch=16,
      col='magenta',
      xlab="Number of Inputs",
      ylab = "Validation MSE",
      main = "Forward Selection Results"
      )
abline(v = which.min(fwd.val.MSE), col='blue')

plot(1:19,bwd.val.MSE,
     type='b',
     pch=16,
     col='orange',
     xlab="Number of Inputs",
     ylab = "Validation MSE",
     main = "Forward Selection Results"
)
abline(v = which.min(fwd.val.MSE), col='blue')

coef(regfit.fwd,10)
coef(regfit.bwd,7)

regfit.fwd.best = regsubsets(Salary~., data=Hitters, method="forward", nvmax=10)
regfit.bwd.best = regsubsets(Salary~., data=Hitters, method="backward", nvmax=7)
coef(regfit.fwd.best,10)
