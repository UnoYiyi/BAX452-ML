rm(list = ls())
#Question 1.22.1
library(freqparcoord)
data(mlb)
head(mlb)
xvalpart <- function(data, p){
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace = FALSE)
  list(train = data[trainidxs,], valid = data[-trainidxs,])
}
xvallm <- function(data, ycol, predvars, p, meanabs = TRUE){
  tmp <- xvalpart(data, p)
  train <- tmp$train
  valid <- tmp$valid
  trainy <- train[, ycol]
  trainpreds <- train[, predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[, predvars])
  predy <- cbind(1, validpreds)%*% coef(lmout)
  realy <- valid[,ycol]
  if (meanabs) return (mean(abs(predy-realy)))
  list(predy = predy, realy = realy)
}
lm <- round(xvallm(mlb, 5, c(4,6), 4/5),3)
lm

#try 5 times
for (i in 1:5){
  print(xvallm(mlb, 5, c(4,6), 2/3))
}

#KNN MODEL
library(regtools)
xvalknn <-  function(data, ycol, predvars, k, p, meanabs=TRUE){
  data <-  data[ ,c(predvars, ycol)] 
  ycol <-  length(predvars) + 1 
  tmp <-  xvalpart (data, p) 
  train <-  tmp$train 
  valid <- tmp$valid 
  valid <-  as.matrix(valid) 
  xd <-  preprocessx(train[,-ycol], k) 
  kout <-  knnest(train[,ycol], xd, k) 
  predy <-  predict(kout, valid[,-ycol], TRUE) 
  realy <-   valid[, ycol] 
  if (meanabs) 
    return(mean(abs(predy-realy)))
  list( predy = predy , realy = realy )
}
knn <- round(xvalknn(mlb, 5, c(4,6), 5, 4/5),3)
knn
#try 5 times
for (i in 1:5){
  print(xvalknn(mlb, 5, c(4,6), 25, 2/3))
}
##comparison
print(paste("Linear Model:", lm))
print(paste("KNN Model:", knn))
#therefore they are similar but some change

#Question 1.22.2
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
#add the interaction terms
pe$agefem <- pe$age * pe$fem
pe$age2fem <- pe$age2 * pe$fem
# model
model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem, data = pe)
summary(model)
# Predict for a 32-year-old person, female, with a Master's degree
age <- 32
age2 <- 32^2
wkswrkd <- 52
ms <- 1
phd <- 0
fem <- 1
agefem <- age*fem
age2fem <- age2*fem
input <- data.frame(age,age2,wkswrkd,ms,phd,fem,agefem,age2fem)
predict(model, input, interval = "prediction", level = 0.95)
#？not sure which one is the effect
effect <-  4715.271*32 + 32^2 *(-49.081) -2503.678 * 32 +27.607 * 32^2 +9810.033+41739
effect

#Question 1.22.3
library(mfp)
data(bodyfat)
head(bodyfat)
model = lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data = bodyfat)
summary(model)
#Question 1.22.4
#a The overall mean height of people is the weighted average of the male mean height and female mean height
#b The overall proportion of people taller than 70 inches is the weighted average of proportion of male taller than 70 inches and female taller than 70 inches.

#Question 2.14.1
data(prgeng)
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
#add the interaction terms
pe$msfem <- pe$ms * pe$fem
# model
model = lm(wageinc ~., data = pe)
sum_model <- summary(model)
beta6 <- sum_model$coefficients['fem',]
beta7 <- sum_model$coefficients['msfem',]
t_value <- qt(0.975, nrow(prgeng)-1)
beta6_h <- beta6[1] + t_value*beta6[2]
beta6_l <- beta6[1] - t_value*beta6[2]
beta7_h <- beta7[1] + t_value*beta7[2]
beta7_l <- beta7[1] - t_value*beta7[2]
sprintf('a. The 95 percent confidence interval for beta6 is %f and %f', beta6_l, beta6_h)
sprintf('b. The 95 percent confidence interval for beta6+beta7 is %f and %f', beta6_l +beta7_l, beta6_h + beta7_h)

#Question 2.14.2
day <- read.csv('day.csv')
day$temp2 <- day$temp^2
day$clearday <- as.integer(day$weathersit == 1)
bike <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
bike_summ <- summary(bike)
t_value <- qt(0.975, nrow(day)-1)
yr <- bike_summ$coefficients['yr',]
yr_l <- yr[1] - t_value * yr[2]
yr_h <- yr[1] + t_value * yr[2]

sprintf('The 95 percent confidence interval for beta6 is %f and %f', yr_l, yr_h)

#Question 2.14.4
simr2 <- function(n,p,nreps) {
  r2s <- vector(length = nreps)
  for (i in 1:nreps) {
    x <- matrix(rnorm(n*p), ncol = p)
    y <- x%*% rep(1,p) + rnorm(n,sd = sqrt(p))
    r2s[i] <- getr2(x,y)
  }
  hist(r2s)
}

getr2 <- function(x,y) {
  smm <- summary(lm(y~x))
  smm$r.squared
}
simr2(25,8,1000)
simr2(250,8,1000)
simr2(1000,8,1000)
