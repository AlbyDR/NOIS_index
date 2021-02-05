##############################################################################
############## Näive Overfitting Index Selection (NOIS) ######################
## see https://doi.org/10.1016/j.isprsjprs.2017.09.012  ######################
## for more details about the method                    ######################
##############################################################################
library(MASS)
library(hsdar)
library(caret)
##############################################################################
############## simulate an uncorrelated predictors  ##########################
##############################################################################
# generate data independent to the response variable
# LAIspectra_0pSD is the spectra dataset (400nm-2100nm - n=100)
# the data is composed by 30 realization for a landscape 
# without spatial dependency

set.seed(1234)
GEdata000 <- data.frame(sapply(1:30, FUN = function(i)
                 data.frame((mvrnorm(n=100, 
                      colMeans(data.frame(LAIspectra_0pSD[[i]][-1]), # [-1] to exclude LAI
                               na.rm = FALSE, dims=1),
                               cov(data.frame(LAIspectra_0pSD[[i]][-1])) ))) ))

##########################################################################
# check the remain correlation between LAI and each wavelength
##########################################################################
CorMatrix <- data.frame(sapply(1:30, FUN = function(i) 
           data.frame(cor(data.frame(lai=LAIspectra_0pSD[[i]]$lai,GEdata000[[i]]))) ))

CorYbandGE <- data.frame(sapply(1:30, FUN = function(i)
                         CorMatrix[[i]]$lai[-1]))

# maximum Rsquared between LAI and a wavelength
# based on the remain correlation a tolerance baseline can be adjusted
sapply(1:30, FUN=function(i) max(abs(CorYbandGE[[i]]))^2)

# plot correlation per wavelength
colour <- colorRamp(c("orange", "yellow"))
         
Corspectra <- sapply(1:30, FUN = function(i) 
         speclib(CorYbandGE[[i]], rep(401:2501)))

plot(Corspectra[[1]], FUN=1, ylim=c(-1,1),
     col=rgb(colour(4/8),maxColorValue=255))
   
for (i in 2:30)
  plot(Corspectra[[i]], FUN=i, new=FALSE,
       col=rgb(colour(i/30), maxColorValue=255))

#############################################################################
# Tune Naive models and calculate the overfitting index NOIS
#############################################################################
# Tune the models increasing complexity
# 1 - example Partial Least Square regression (PLSR)
# 2 - example Support Vector Machine (SVM)
#############################################################################
nonControl <- trainControl(method = "none") # no cross-validation
set.seed(39)
kfold10 <- trainControl(method="repeatedcv", repeats=10) # K-fold cross-validation
#############################################################################
# 1 Partial Least Square regression (PLSR)
# Tune from 1 to 20 components (accumulated) without cross-validation
Plsfit1 <- c()
for (i in 1:30) {
  Plsfit1[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp = 1), # 1 component
                        trControl = nonControl)} # no cross-validation

Plsfit2 <- c()
for (i in 1:30) {
  Plsfit2[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp = 2),# 2 component
                        trControl = nonControl)}

Plsfit3 <- c()
for (i in 1:30) {
  Plsfit3[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp = 3),
                        trControl = nonControl)}

Plsfit4 <- c()
for (i in 1:30) {
  Plsfit4[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp = 4),
                        trControl = nonControl)}

Plsfit5 <- c()
for (i in 1:30) {
  Plsfit5[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp = 5),
                        trControl = nonControl)}

Plsfit6 <- c()
for (i in 1:30) {
  Plsfit6[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp = 6),
                        trControl = nonControl)}

Plsfit7 <- c()
for (i in 1:30) {
  Plsfit7[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp = 7),
                        trControl = nonControl)}

Plsfit8 <- c()
for (i in 1:30) {
  Plsfit8[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp = 8),
                        trControl = nonControl)}

Plsfit9 <- c()
for (i in 1:30) {
  Plsfit9[[i]] <- train(lai~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "pls", # package wrapped
                        type= "Regression",
                        tuneGrid = expand.grid(ncomp=9),
                        trControl = nonControl)}

Plsfit10 <- c()
for (i in 1:30) {
  Plsfit10[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 10),
                         trControl = nonControl)}

Plsfit11 <- c()
for (i in 1:30) {
  Plsfit11[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 11),
                         trControl = nonControl)}

Plsfit12 <- c()
for (i in 1:30) {
  Plsfit12[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 12),
                         trControl = nonControl)}

Plsfit13 <- c()
for (i in 1:30) {
  Plsfit13[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 13),
                         trControl = nonControl)}

Plsfit14 <- c()
for (i in 1:30) {
  Plsfit14[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 14),
                         trControl = nonControl)}

Plsfit15 <- c()
for (i in 1:30) {
  Plsfit15[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 15),
                         trControl = nonControl)}

Plsfit16 <- c()
for (i in 1:30) {
  Plsfit16[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 16),
                         trControl = nonControl)}

Plsfit17 <- c()
for (i in 1:30) {
  Plsfit17[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 17),
                         trControl = nonControl)}

Plsfit18 <- c()
for (i in 1:30) {
  Plsfit18[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 18),
                         trControl = nonControl)}

Plsfit19 <- c()
for (i in 1:30) {
  Plsfit19[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 19),
                         trControl = nonControl)}

Plsfit20 <- c()
for (i in 1:30) {
  Plsfit20[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneGrid = expand.grid(ncomp = 20),
                         trControl = nonControl)}

# With cross-validation and up 20 to component
Plsfitcv <- c()
for (i in 1:30) {
  Plsfitcv[[i]] <- train(lai~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "pls", # package wrapped
                         type= "Regression",
                         tuneLength = 20, # number of components tested
                         trControl = kfold10 )} # with cross-validation

# Original data With cross-validation and up to 20 component
Pls000original <- c()
for (i in 1:30) {
  Pls000original[[i]] <- train(lai~., 
                          data=data.frame(LAIspectra_0pSD[[i]]), #originaldata
                          method = "pls", # package wrapped
                          type= "Regression",
                          tuneLength = 20, #number of components tested
                          trControl = kfold10 )} # with cross-validation

###################################################################################################################
# calculating RMSE for the Naive Models - PLSR
###################################################################################################################
RMSE.000 <- t(data.frame(
  "1"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit1[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "2"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit2[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "3"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit3[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "4"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit4[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "5"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit5[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "6"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit6[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "7"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit7[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "8"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit8[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "9"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit9[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "10"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit10[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "11"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit11[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "12"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit12[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "13"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit13[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "14"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit14[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "15"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit15[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "16"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit16[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "17"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit17[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "18"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit18[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "19"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit19[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "20"=sapply(1:30, FUN=function(i) postResample(predict(Plsfit20[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',]))

###################################################################################################
########################## RMSE with the mean as prediction ########################################
###################################################################################################
RMSELAIav <- data.frame(RMSEy=sapply(1:30, FUN=function(i)
  (postResample(rep(mean(unlist(LAIspectra_0pSD[[i]]$lai)), 100), 
                unlist(LAIspectra_0pSD[[i]]$lai))['RMSE'])))# 100 sample size

############################################################################
# Naive overfitting index for the 20 components
NOI000 = data.frame(sapply(1:30, FUN=function(i) 
                     data.frame(NOI=1-(RMSE000[,i]/RMSELAIav[i,]))))
#############################################################################
## plot NOIS method
plot(rep(1:20), NOI000[[1]], type="b", col="green", new=F, 
     ylim=c(-0.1,1), xaxt='n', yaxt='n', cex.lab=1.3,
     main="NOIS Index - Partial Least Squares (PLSR)", 
     xlab = "Ncomp", ylab="overfitting index (NOIS)")
axis(side = 1, at=1:20, cex.axis = 1.1)
axis(side = 2, at=c(0,.05,.1,.2,.3,.4,0.5,.6,.7,.8,.9,1),las=2,cex.axis=1.1)
abline(h=0.05, col="red", lwd=2)
abline(h=0.0, col="grey", lty="dotted", lwd=2)

for (i in 1:30) { 
  lines(rep(1:20), NOI000[[i]], type="b", col="green", new=F)
  points(Plsfitcv[[i]]$bestTune$ncomp, 
         NOI000[Plsfitcv[[i]]$bestTune$ncomp,i], type="p", 
         col="black", new=F) 
  points(Pls000original[[i]]$bestTune$ncomp, 
         NOI000[Pls000original[[i]]$bestTune$ncomp,i], 
         type="p", col="red", new=F)}

###########################################################################

###############################################
############## Tune SVM #######################
###############################################
svmfit1 <- c()
for (i in 1:30) {
  svmfit1[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.00005)))}

svmfit2 <- c()
for (i in 1:30) {
  svmfit2[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.0001)))}
svmfit3 <- c()
for (i in 1:30) {
  svmfit3[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.00025)))}

svmfit4 <- c()
for (i in 1:30) {
  svmfit4[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.0005)))}

svmfit5 <- c()
for (i in 1:30) {
  svmfit5[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.001)))}

svmfit6 <- c()
for (i in 1:30) {
  svmfit6[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.0025)))}

svmfit7 <- c()
for (i in 1:30) {
  svmfit7[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.005)))}

svmfit8 <- c()
for (i in 1:30) {
  svmfit8[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.01)))}

svmfit9 <- c()
for (i in 1:30) {
  svmfit9[[i]] <- train(lai ~., 
                        data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                        method = "svmLinear2", 
                        trControl = nonControl,
                        tuneGrid = expand.grid(cost=c(0.05)))}

svmfit10 <- c()
for (i in 1:30) {
  svmfit10[[i]] <- train(lai ~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "svmLinear2", 
                         trControl = nonControl,
                         tuneGrid = expand.grid(cost=c(0.1)))}

svmfit11 <- c()
for (i in 1:30) {
  svmfit11[[i]] <- train(lai ~., 
                         data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                         method = "svmLinear2", 
                         trControl = nonControl,
                         tuneGrid = expand.grid(cost=c(0.25)))}

# with cross-validation and cost from .00005 to .25
costgrid <- expand.grid(cost=c(0.00005,0.0001,0.00025,0.0005, 0.001,
                               0.0025, 0.005, 0.01, 0.05, 0.1, 0.25))

svmfitcv <- c()
for (i in 1:30) {
  svmfitcv[[i]] <- train(lai ~., 
                          data=data.frame(lai=LAIspectra_0pSD[[i]]$lai, GEdata000[[i]]),
                          method = "svmLinear2", 
                          trControl = kfold10,
                          tuneGrid = costgrid)}

# original data with cross-validation and cost from .00005 to .25
svm000original <- c()
for (i in 1:30) {
  svm000original[[i]] <- train(lai ~., 
                          data=data.frame(LAIspectra_0pSD[[i]]),
                          method = "svmLinear2", 
                          trControl = kfold10,
                          tuneGrid = costgrid)}

###################################################################################################################
# RMSE for the Naive Models - SVMR
###################################################################################################################
RMSEsvm <- t(data.frame(
  "0.000005"=sapply(1:30, FUN=function(i) postResample(predict(svmfit1[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.00001"=sapply(1:30, FUN=function(i) postResample(predict(svmfit2[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.00005"=sapply(1:30, FUN=function(i) postResample(predict(svmfit3[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.0001"=sapply(1:30, FUN=function(i) postResample(predict(svmfit4[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.00025"=sapply(1:30, FUN=function(i) postResample(predict(svmfit5[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.0005"=sapply(1:30, FUN=function(i) postResample(predict(svmfit6[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.001"=sapply(1:30, FUN=function(i) postResample(predict(svmfit7[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.0025"=sapply(1:30, FUN=function(i) postResample(predict(svmfit8[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.005"=sapply(1:30, FUN=function(i) postResample(predict(svmfit9[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.01"=sapply(1:30, FUN=function(i) postResample(predict(svmfit10[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',],
  "0.05"=sapply(1:30, FUN=function(i) postResample(predict(svmfit11[[i]]), unlist(LAIspectra_0pSD[[i]]$lai)))['RMSE',]))

############################################################################
# Naive overfitting index for the 11 levels of cost
############################################################################
NOI000svm = data.frame(sapply(1:30, FUN=function(i) 
  data.frame(NOI=1-(RMSEsvm[,i]/RMSELAIav[i,]))))
#############################################################################

costnames<-c(0.00005,0.0001,0.00025,0.0005, 0.001,
             0.0025, 0.005, 0.01, 0.05, 0.1, 0.25)

cost <- sapply(1:30, FUN = function(i) svmfitcv[[i]]$bestTune$cost)
cost[cost==0.00005] <- 1
cost[cost==0.0001]  <- 2
cost[cost==0.00025] <- 3
cost[cost==0.0005]  <- 4
cost[cost==0.001]   <- 5
cost[cost==0.0025]  <- 6
cost[cost==0.005]   <- 7
cost[cost==0.01]    <- 8
cost[cost==0.05]    <- 9
cost[cost==0.1]     <- 10
cost[cost==0.25]    <- 11

cost2 <- sapply(1:30, FUN = function(i) svm000original[[i]]$bestTune$cost)
cost2[cost2==0.00005] <- 1
cost2[cost2==0.0001]  <- 2
cost2[cost2==0.00025] <- 3
cost2[cost2==0.0005]  <- 4
cost2[cost2==0.001]   <- 5
cost2[cost2==0.0025]  <- 6
cost2[cost2==0.005]   <- 7
cost2[cost2==0.01]    <- 8
cost2[cost2==0.05]    <- 9
cost2[cost2==0.1]    <- 10
cost2[cost2==0.25]   <- 11
##########################################################################
###### plot NOIS method - SVM ############################################
##########################################################################
plot(rep(1:11),NOI000svm[[1]], type="b", col="green", new=F, 
     ylim=c(-0.1,1), xaxt='n', yaxt='n', cex.lab=1.3,
     main="NOIS Index - Support Vector Machine (SVM)", 
     xlab = "Cost", ylab="overfitting index (NOIS)")
axis(side = 1, at=1:11, cex.axis = 1, labels=costnames)
axis(side = 2, at=c(0,.05,.1,.2,.3,.4,0.5,.6,.7,.8,.9,1),las=2,cex.axis=1.1)
abline(h=0.05, col="red", lwd=2) # tolerance baseline
abline(h=0.0, col="grey", lty="dotted", lwd=2)

for (i in 1:30) { 
    lines(rep(1:11), NOI000svm[[i]], type="b", col="green", new=F)
    points(cost[i], NOI000svm[cost[i],i], # complexity tuned generated data model
           type="p", col="black", new=F)
    points(cost2[i], NOI000svm[cost2[i],i],# complexity tuned original data 
           type="p", col="red", new=F)}

