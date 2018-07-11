# Testing Trees

library(caret)
library(MASS)
library(Rlab)
library(boot)
library(ggplot2)
library(ggridges)
library(dplyr)
set.seed(12345)

N <- 2000
P <- 50

## Ballanced
mu <- runif(P, -1,1)
Sigma <- rWishart(n=1, df=P, Sigma=diag(P))[,,1]
Sigma <- ifelse(row(Sigma) != col(Sigma), 0, Sigma)
X <- mvrnorm(N, mu=mu, Sigma = Sigma)
p <- rbern(P, 1)
beta <- p*rnorm(P,1,0.9) + (1-p)*rnorm(P,0,0.3)
eta <- X%*%beta
pi <- inv.logit(eta)
Y <- rbern(N, pi)
## sum(Y)/length(Y)
Y <- as.factor(Y)
data.1.Bal <- data.frame(X, Y)

## High >> .5
mu2 <- runif(P, 0.5, 1.5)
Sigma2 <- rWishart(n=1, df=P, Sigma=diag(P))[,,1]
Sigma2 <- ifelse(row(Sigma2) != col(Sigma2), 0, Sigma2)
X2 <- mvrnorm(N, mu=mu2, Sigma = Sigma2)
p2 <- rbern(P, 1)
beta2 <- p2*rnorm(P,1,0.1) + (1-p2)*rnorm(P,0,1)
eta2 <- X2%*%beta2
pi2 <- inv.logit(eta2)
Y2 <- rbern(N, pi2)
## sum(Y2)/length(Y2)
Y2 <- as.factor(Y2)
data.2.High <- data.frame(X2, Y2)


## Low << .5
mu3 <- runif(P, -2, 0)
Sigma3 <- rWishart(n=1, df=P, Sigma=diag(P))[,,1]
Sigma3 <- ifelse(row(Sigma3) != col(Sigma3), 0, Sigma3)
X3 <- mvrnorm(N, mu=mu3, Sigma = Sigma3)
p3 <- rbern(P, 1)
beta3 <- p3*rnorm(P,1,1.7) + (1-p3)*rnorm(P,0,0.01)
eta3 <- X3%*%beta3
pi3 <- inv.logit(eta3)
Y3 <- rbern(N, pi3)
## sum(Y3)/length(Y3)
Y3 <- as.factor(Y3)
data.3.Low <- data.frame(X3, Y3)

# creating all of the test sets training sets
## Test/Training Sets for Even Data
test.data.1.Bal <- data.1.Bal[1:500,]
train.data.1.Bal <- data.1.Bal[501:2000,]

## Test/Training Sets for High Success Data
test.data.2.High <- data.2.High[1:500,]
train.data.2.High <- data.2.High[501:2000,]

## Test/Training Sets for Low Success Data
test.data.3.Low <- data.3.Low[1:500,]
train.data.3.Low <- data.3.Low[501:2000,]


### All of these elastic net lambda's for the assignment have
#    been narrowed from 0-200 and ## Elastic Net:
enet.Bal <- train(Y~., method="glmnet",
                  tuneGrid=expand.grid(alpha=seq(0, 1, .1),
                                       lambda=seq(0, 5,.05)),
                  data=train.data.1.Bal,
                  preProcess=c("center"),
                  trControl=trainControl(method="cv",number=2, search="grid"))


ggplot(enet.Bal, aes(x=enet.Bal$results$lambda,
                     y=enet.Bal$results$Accuracy))+
     geom_point( aes(colour = factor(enet.Bal$results$alpha)) )+
     guides(shape = "none")+
     ggtitle("Elastic Net For Ballanced Data")+
     xlab(expression(paste("Regularization Parameter ", lambda)))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))+
     theme(legend.position = c(.85, .65))



## High
enet.High <- train(Y2~., method="glmnet",
                   tuneGrid=expand.grid(alpha=seq(0, 1, .1),
                                        lambda=seq(0,.3,.005)),
                   data=train.data.2.High,
                   preProcess=c("center"),
                   trControl=trainControl(method="cv",number=2, search="grid"))


ggplot(enet.High, aes(x=enet.High$results$lambda,
                      y=enet.High$results$Accuracy))+
     geom_point( aes(colour = factor(enet.High$results$alpha)) )+
     guides(shape = "none")+
     ggtitle("Elastic Net For High Success DGP")+
     xlab(expression(paste("Regularization Parameter ", lambda)))+
     theme_bw()+
     theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14))+
     theme(legend.position = c(.85, .65))

## Low
enet.Low <- train(Y3~., method = "glmnet",
                  tuneGrid = expand.grid(alpha = seq(0, 1, .1),
                                        lambda = seq(0, .7, .005)),
                  data = train.data.3.Low,
                  preProcess = c("center"),
                  trControl = trainControl(method="cv",number=2, search="grid"))


ggplot(enet.Low, aes(x = enet.Low$results$lambda,
                     y = enet.Low$results$Accuracy))+
     geom_point( aes(colour = factor(enet.Low$results$alpha)) )+
     guides(shape = "none")+
     ggtitle("Elastic Net For Low Success DGP")+
     xlab(expression(paste("Regularization Parameter ", lambda)))+
     theme_bw()+
     theme(axis.text = element_text(size=12),
          axis.title = element_text(size=14))+
     theme(legend.position = c(.85, .65))

######################
# Ballanced:
yhat = predict(enet.Bal)

qplot(train.data.1.Bal$Y, yhat, alpha=I(0.25))+
     geom_jitter()+
     xlab( expression(paste("Training ", hat(y))))+
     ylab( expression(paste("Elastic Net ", y)))+
     theme_bw()+
theme(axis.text = element_text(size=12),
     axis.title = element_text(size=14))+
     ggtitle("Comparing Y Values: Ballanced Data")




# need a lot of packages for this:
packages <- c("caret", "MASS", "Rlab", "boot", "ggplot2", "ggridges", "dplyr")
invisible( lapply(packages, library, character.only = TRUE))

set.seed(12345)

N <- 2000
P <- 25 # P is cut down for computation time, (but could be much larger!)

## Ballanced
mu <- runif(P, -1,1)
Sigma <- rWishart(n=1, df=P, Sigma=diag(P))[,,1]
Sigma <- ifelse(row(Sigma) != col(Sigma), 0, Sigma)
X <- mvrnorm(N, mu=mu, Sigma = Sigma)
p <- rbern(P, 1)
beta <- p*rnorm(P,1,0.9) + (1-p)*rnorm(P,0,0.3)
eta <- X%*%beta
pi <- inv.logit(eta)
Y <- rbern(N, pi)
Y <- as.factor(Y)
data.1.Bal <- data.frame(X, Y)

## Test/Training Sets for Even Data
test.data.1.Bal <- data.1.Bal[1:500,]
train.data.1.Bal <- data.1.Bal[501:2000,]

library(kernlab)
library(microbenchmark)
# Linear SVM
linear.svm.bal <- train(Y~.,
                       data=train.data.1.Bal,
                       method = "svmLinear")

linear.svm.bal$results$Accuracy

# A polynomial kernal: (long computation!)
# poly.svm.bal <- train(Y~.,
#                      data=train.data.1.Bal,
#                      method="svmPoly")

# poly.svm.bal$results[as.numeric(rownames(poly.svm.bal$bestTune)),]
# .951 Accuracy; .902 Kappa; AccuracySD of .011


# Radial kernal:
# radial.svm.bal = train(Y~.,
#                        data=train.data.1.Bal,
#                        method="svmRadialCost")

# radial.svm.bal$results[as.numeric(rownames(radial.svm.bal$bestTune)),]
# Accuracy of .54; AccuracySD .017



### Trees
# Using "rpart", may need to reload R to get things to work.
library(rpart)
# grow tree
tree.bal <- rpart(Y~.,
                  method="class", data=train.data.1.Bal)
## printcp(tree.bal) # display the results
## plotcp(tree.bal) # visualize cross-validation results
## summary(tree.bal) # detailed summary of splits

# plot tree
plot(tree.bal, uniform=TRUE,
     main="Classification Tree for Ballanced Data")
text(tree.bal, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree
post(tree.bal,
     title = "Classification Tree for Ballanced Data")
# then we have a lot of options off of this, including pruning the tree back,
# or making -multiple- trees into a forrest and
# selecting the best one! (intutively a form of advanced bootstrapping!)
# prune the tree, by the error that was previously generated above
ptree.bal <- prune(tree.bal,
                   cp=tree.bal$cptable[which.min(tree.bal$cptable[,"xerror"]),
                                       "CP"])

# plot the pruned tree
plot(ptree.bal, uniform=TRUE,
     main ="Pruned Classification Tree for Ballanced Data")
text(ptree.bal, use.n=TRUE, all=TRUE, cex=.8)


post(ptree.bal,
     title = "Pruned Classification Tree for Ballanced Data")
