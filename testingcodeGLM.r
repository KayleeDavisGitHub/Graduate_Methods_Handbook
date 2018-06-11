#### Preamble
setwd("Data Files")
library(foreign)
library(arm) #curve, sim, other functions
library(car) #recoding variables
library(texreg)
library(ggplot2)

#loading beer.dta, a ranking of beers and their specifications
d <- read.dta(file.choose())
d
d$rating <- as.numeric(d$rating)
d
d$rating <- recode(d$rating,"3=0; 2=1; 1=2")
d
class(d$rating) #numeric and coded intuitively, could be better
d$rating <- recode(d$rating,"0=-1")
d$rating <- recode(d$rating,"1=0")
d$rating <- recode(d$rating,"2=1")
d
class(d$rating)
d$rating #having the median ranking be zero will give our intercepts more interprability

hist(d$rating)
hist(d$price)
summary(d$price)






#### Regression Initial Stuff

set.seed(8675309)
mod1 <- lm(d$rating ~ d$price)
summary(mod1)

texreg(l= list(mod1), stars = numeric(0),
       custom.coef.names = c("Intercept", "Beer Price"),
       caption.above = T, float.pos = "h!", custom.note = "Dependent variable: Beer Rating" )


plot(d$price, jitter(d$rating), pch=19, xlab= "Price", ylab="Rating", main="Rating by Price of Beer: with 100 simulations")
abline(mod1)
m1sim <- sim(mod1)
for (i in 1:100){
  curve (coef(m1sim)[i,1] + coef(m1sim)[i,2]*x, add=T, col="grey")
}
curve (mod1$coef[1] + mod1$coef[2]*x, add=T, lwd=3)

plot(mod1)






  #Model 2
mod2 <- lm(d$rating ~ d$price + d$alcohol)
summary(mod2)
plot(mod2)


texreg(l= list(mod1, mod2), stars = numeric(0),
       custom.coef.names = c("Intercept", "Beer Price", "Alcohol Content"),
       caption.above = T, float.pos = "h!", custom.note = "Dependent variable: Beer Rating" )










#### logit and probit Modeling
logmod1 <- glm(d$verygood ~ d$price, family = binomial(link=logit))
summary(logmod1)

promod1 <- glm(d$verygood ~ d$price, family = binomial(link=probit))
summary(posmod1)

texreg(l= list(logmod1, promod1), stars = numeric(0),
       custom.model.names = c("Logit Model", "Probit Model"),
       custom.coef.names = c("Intercept", "Price"),
       caption.above = T, float.pos = "h!", custom.note = "Dependent variable: Very Good Beer (1)" )


logmod2 <- glm(d$verygood ~ d$price + d$avail + d$alcohol + d$calories, family = binomial(link=logit))
summary(logmod2)
plot(logmod2)

promod2 <- glm(d$verygood ~ d$price + d$avail + d$alcohol + d$calories, family = binomial(link=probit))
summary(promod2)
plot(promod2)


texreg(l= list(logmod2, promod2), stars = numeric(0),
       bold = T, ci.force = T,
       custom.model.names = c("Logit Model", "Probit Model"),
       custom.coef.names = c("Intercept", "Price", "Availability", "Alcohol Content", "Caloric Content"),
       caption.above = T, float.pos = "h!", custom.note = "Dependent variable: Very Good Beer (1)" )


# Probability Curve
logmod1 <- glm(d$verygood ~ d$price, family = binomial(link=logit))
curve(invlogit(logmod1$coef[1] + logmod1$coef[2]*x), 1, 5, ylim=c(-.01,.9),
      xlim=c(0,7.5), xaxt="n", xaxs="i", mgp=c(2,.5,0),
      ylab="Pr(Very Good Beer)", xlab="Price", main="Price of Beer on Probability of Very Good Beer", lwd=4)
curve(invlogit(logmod1$coef[1] + logmod1$coef[2]*x), -2, 8, lwd=5, add = T)
axis(1, 1:7, mgp=c(2,.5,0))
mtext ("(two dollars)", 1, 1.5, at=2, adj=.5)
mtext ("(six dollars)", 1, 1.5, at=6, adj=.5)
points(jitter(d$price, 1.5), jitter(d$verygood, 0.1), pch=20, cex=.1)



# Binned Residual plotting
par(mfrow=c(1,2))

x <- predict(logmod1, type = "response")
y <- resid(logmod1)
binnedplot(x, y, nclass=NULL,
           xlab="Expected Values", ylab="Average residual",
           main="Binned residual plot: Model 1",
           cex.pts=0.8, col.pts=1, col.int="gray")


x <- predict(logmod2, type = "response")
y <- resid(logmod2)
binnedplot(x, y, nclass=NULL,
           xlab="Expected Values", ylab="Average residual",
           main="Binned residual plot: Model 2",
           cex.pts=0.8, col.pts=1, col.int="gray")

par(mfrow=c(1,1))


# Rope Ladder Plotting
f1<-data.frame(Variable=rownames(summary(logmod1)$coef),
               Coefficient=summary(logmod1)$coef[,1],
               SE=summary(logmod1)$coef[,2],
               modelName= "Model 1")

f2<-data.frame(Variable=rownames(summary(logmod2)$coef),
               Coefficient=summary(logmod2)$coef[,1],
               SE=summary(logmod2)$coef[,2],
               modelName= "Model 2")

combinedframe <- data.frame(rbind(f1, f2))
interval1 <- qnorm((1-0.9)/2)
interval2 <- qnorm((1-0.95)/2)

rl1 <- ggplot(combinedframe)
rl1 <- rl1 + geom_hline(yintercept = 0, color = grey(1/2), lty = 5)
rl1 <- rl1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
rl1 <- rl1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2), color=("Black"),
                             shape = 21, fill = "BLACK")
rl1 <- rl1 + coord_flip() + theme_bw()
rl1 <- rl1 + ggtitle("Comparing Two Models:")
print(rl1)


#### Poisson Modeling was in Quant II Lecture 9/ HW 9
install.packages("faraway")
library(faraway)
library(arm)
library(MASS)
data(africa)
## Examine Dataset ##
`?`(africa)
str(africa)  # There are some NAs in the data





#### Multinomial Modeling
library(nnet)
install.packages("faraway")
library(faraway)
nes96

nes96$PID
nes96$PID3 <-  recode(nes96$PID,"c( 'strRep', 'weakRep', 'indRep') = 'Republican'; c('strDem', 'weakDem', 'indDem')='Democrat'; c('indind')='Independent'")
nes96$PID3



mul1 <- multinom(PID3 ~ age + TVnews, data = nes96)
mul1

library(texreg)
texreg(l= list(mul1), stars = numeric(0),
       bold = T, ci.force = T,
       custom.model.names = c("Model"),
       custom.coef.names = c("Intercept", "Age", "TV News"),
       caption.above = T, float.pos = "h!")


mul11 <- step(mul1)


#######

?mice

library(mice)
## Refering to the mice help file on imputation on medical data:

# do default multiple imputation on a numeric matrix
imp <- mice(nhanes)
#  head(imp)  # Checking, lots of useful information in here

# list the actual imputations for BMI
imp$imp$bmi

# first completed data matrix, can save this for analysis
complete(imp)

# imputation on mixed data with a different method per column:
# mice(nhanes2, meth=c('sample','pmm','logreg','norm'))
# This will give you all information per column like before.


