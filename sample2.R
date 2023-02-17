# Earnings Forecast/Fit
# Dr.V.
# VERSION : 0.1
#


setwd("~/Desktop/predicting_rpm")
install.packages(c("matrixcalc", "parallel", "snow", "caret","stringr","dummies","e1071","doParallel","ada","LiblineaR","nnet","matrixStats","FSelector","xlsx","klaR"))

require(matrixcalc)
library(snow)
library(parallel)
library(caret)
require(stringr)
library(dummies)
require(BioPhysConnectoR)
require(e1071)
library(doParallel)
require(ada)
library(LiblineaR)
library(matrixStats)
require(nnet)
library(FSelector)
require(xlsx)
require(klaR)
#registerDoParallel(cores=2)

gaming_earnings <- read.csv("sameplefile1.csv", stringsAsFactors=FALSE)
nongaming_earnings <- read.csv("sameplefile2.csv", stringsAsFactors=FALSE)

data2<-gaming_earnings
data2<-data2[order(data2$name)[!duplicated(sort(data2$name))],]
data2<-data2[data2$t_earnings>20,]
data2<-data2[data2$t_earnings<5000,]

data2$diff_21<-data2$w2_views-data2$w1_views
data2$diff_32<-data2$w3_views-data2$w2_views
data2$diff_43<-data2$w4_views-data2$w3_views


data2$YY[data2$t_earnings >= 1000 ] <- "SUPERHIGH"
# data2$YY[data2$t_earnings < 7500 & data2$t_earnings >= 1000] <- "HIGH"
data2$YY[data2$t_earnings < 1000 & data2$t_earnings >= 100.0] <- "MED"
# data2$YY[data2$t_earnings < 100.0 & data2$t_earnings >= 50.0] <- "MED-LOW"
data2$YY[data2$t_earnings < 100] <- "LOW"

data2$YY <- factor(data2$YY)
data2$YY <- ordered(data2$YY, levels = c("LOW","MED","SUPERHIGH"))
#data$YY <- relevel(data$YY, ref = "LOW")


nongaming_earnings_testset <- read.csv("sameplefile1_test.csv", stringsAsFactors=FALSE)
gaming_earnings_testset <- read.csv("sameplefile2_test.csv", stringsAsFactors=FALSE)



data2_test<-nongaming_earnings_testset
data2_test<-data2_test[order(data2_test$name)[!duplicated(sort(data2_test$name))],]
data2_test<-data2_test[data2_test$t_earnings>20,]
data2_test<-data2_test[data2_test$t_earnings<10000,]


data2_test$diff_21<-data2_test$w2_views-data2_test$w1_views
data2_test$diff_32<-data2_test$w3_views-data2_test$w2_views
data2_test$diff_43<-data2_test$w4_views-data2_test$w3_views


data2_test$YY[data2_test$t_earnings >= 1000 ] <- "SUPERHIGH"
#data2$YY[data2$t_earnings < 1000 & data2$t_earnings >= 600] <- "HIGH"
data2_test$YY[data2_test$t_earnings < 1000 & data2_test$t_earnings >= 100] <- "MED"
#data2$YY[data2$t_earnings < 50.0 & data2$t_earnings >= 5.0] <- "LOW"
data2_test$YY[data2_test$t_earnings < 100] <- "LOW"

data2_test$YY <- factor(data2_test$YY)
data2_test$YY <- ordered(data2_test$YY, levels = c("LOW","MED","SUPERHIGH"))
#data$YY <- relevel(data$YY, ref = "LOW")



# # Bucketize Views_
# buckets <- c(0,100000,500000,1000000,10000000,Inf)
# data2$past1_views_buck <- cut(data2$past1_views,buckets,include.lowest=TRUE)
# data2 <- cbind(data2,model.matrix(~data2$past1_views_buck+0))

# # Bucketize Views_
# buckets <- c(0,100000,500000,1000000,10000000,Inf)
# data2$past1_t_emw_buck <- cut(data2$past1_t_emw,buckets,include.lowest=TRUE)
# data2 <- cbind(data2,model.matrix(~data2$past1_t_emw_buck+0))

# buckets <- c(0,1000,2500,5000,100000,Inf)
# data2$past1_likes_buck <- cut(data2$past1_likes,buckets,include.lowest=TRUE)
# data2 <- cbind(data2,model.matrix(~data2$past1_likes_buck+0))

#fmla <- as.formula(paste("YY ~ 0+past1_views_buck+past1_t_emw_buck+past1_likes_buck"))

fmla <- as.formula(paste("YY ~ 0+I(asinh(past1_subscribers_gained-past1_subscribers_lost))+I(asinh(past2_subscribers_gained-past2_subscribers_lost))+I(asinh(past1_views))+I(asinh(past1_t_emw))+I(asinh(past1_likes))+I(asinh(past1_a_avd))+I(asinh(past1_comments))+I(asinh(past2_views))+I(asinh(past2_t_emw))+I(asinh(past2_likes))+I(asinh(past2_a_avd))+I(asinh(past2_comments))+I(asinh(w1_views))+I(asinh(w1_comments))+I(asinh(w1_shares))+I(asinh(w1_likes))+I(asinh(w1_dislikes))+I(asinh(w1_t_emw))+I(asinh(w1_a_avd))+I(asinh(w1_subscribers_gained-w1_subscribers_lost))+I(asinh(w2_views))+I(asinh(w2_comments))+I(asinh(w2_shares))+I(asinh(w2_likes))+I(asinh(w2_dislikes))+I(asinh(w2_t_emw))+I(asinh(w2_a_avd))+I(asinh(w2_subscribers_gained-w2_subscribers_lost))"))

fmla <- as.formula(paste("YY ~ 0+I(asinh(past1_earnings))+I(asinh(past1_views))+I(asinh(past1_comments))+I(asinh(past1_shares))+I(asinh(past1_likes))+I(asinh(past1_dislikes))+I(asinh(past1_t_emw))+I(asinh(past1_a_avd))+I(asinh(past2_earnings))+I(asinh(past2_views))+I(asinh(past2_comments))+I(asinh(past2_shares))+I(asinh(past2_likes))+I(asinh(past2_dislikes))+I(asinh(past2_t_emw))+I(asinh(past2_a_avd))+I(asinh(w1_earnings))+I(asinh(w1_views))+I(asinh(w1_comments))+I(asinh(w1_shares))+I(asinh(w1_likes))+I(asinh(w1_dislikes))+I(asinh(w1_t_emw))+I(asinh(w1_a_avd))+I(asinh(w2_earnings))+I(asinh(w2_views))+I(asinh(w2_comments))+I(asinh(w2_shares))+I(asinh(w2_likes))+I(asinh(w2_dislikes))+I(asinh(w2_t_emw))+I(asinh(w2_a_avd))+I(asinh(w3_earnings))+I(asinh(w3_views))+I(asinh(w3_comments))+I(asinh(w3_shares))+I(asinh(w3_likes))+I(asinh(w3_dislikes))+I(asinh(w3_t_emw))+I(asinh(w3_a_avd))+I(asinh(past1_subscribers_gained-past1_subscribers_lost))+I(asinh(past2_subscribers_gained-past2_subscribers_lost))+I(asinh(w1_subscribers_gained-w1_subscribers_lost))+I(asinh(w2_subscribers_gained-w2_subscribers_lost))+I(asinh(w3_subscribers_gained-w3_subscribers_lost))"))


fmla <- as.formula(paste("YY ~ 0+I(asinh(past1_earnings))+I(asinh(past1_views))+I(asinh(past1_comments))+I(asinh(past1_shares))+I(asinh(past1_likes))+I(asinh(past1_dislikes))+I(asinh(past1_t_emw))+I(asinh(past1_a_avd))+I(asinh(past2_earnings))+I(asinh(past2_views))+I(asinh(past2_comments))+I(asinh(past2_shares))+I(asinh(past2_likes))+I(asinh(past2_dislikes))+I(asinh(past2_t_emw))+I(asinh(past2_a_avd))+I(asinh(w2_earnings))+I(asinh(w2_views))+I(asinh(w2_comments))+I(asinh(w2_shares))+I(asinh(w2_likes))+I(asinh(w2_dislikes))+I(asinh(w2_t_emw))+I(asinh(w2_a_avd))+I(asinh(w3_earnings))+I(asinh(w3_views))+I(asinh(w3_comments))+I(asinh(w3_shares))+I(asinh(w3_likes))+I(asinh(w3_dislikes))+I(asinh(w3_t_emw))+I(asinh(w3_a_avd))+I(asinh(past1_subscribers_gained-past1_subscribers_lost))+I(asinh(past2_subscribers_gained-past2_subscribers_lost))+I(asinh(w1_subscribers_gained-w1_subscribers_lost))+I(asinh(w2_subscribers_gained-w2_subscribers_lost))+I(asinh(w3_subscribers_gained-w3_subscribers_lost))"))



## multi classes
smp_size <- floor(0.75 * nrow(data2))
train_ind <- sample(seq_len(nrow(data2)), size = smp_size)
mdtrain <- data2[train_ind, ]
mdtest <- data2[-train_ind, ]


m <- multinom(fmla, data = mdtrain)
exp(coef(m))
yhat<-predict(m,newdata=mdtest)

xtab <- table(yhat, mdtest$YY)
confusionMatrix(xtab)


write.csv(coef(m),file = 'coeff_gaming_v4.csv')
## multi classes

yhat<-predict(m,newdata=data2_test,type='probs')


vvv<-cbind(data2_test$youtube_id,data2_test$YY,yhat*100)


### CARET SVM MODEL ###
ctrl <- trainControl(method="cv",number = 10)

# svmfit<- train(fmla,data=data2,method="logreg",tuneLength=10,trControl=ctrl,intercept = TRUE)
# svmfit<- train(fmla,data=data2,method="rf",tuneLength=10,trControl=ctrl)
# rfmodel<-randomForest(fmla,data=data2)
# nBayesmodel<- NaiveBayes(fmla,data=data2,usekernel=TRUE,fL=2)


##neural network
# nnetmodel<-nnet(fmla,data=ibcpm_gaming_one_day,size=3,decay=.1,entropy=TRUE)
# yhat<-predict(nnetmodel,newdata=ibcpm_gaming_one_day,type='class')
# xtab <- table(yhat, ibcpm_gaming_one_day$YY)
# confusionMatrix(xtab)
##neural network


# lm
lmfit<-train(fmla,data=data2,method="lm",trControl=ctrl,has_intercept=FALSE)
print(summary(lmfit))

#trueviewdata[is.na(trueviewdata)]=0
data<-Publishing
title='Publishing'
trueviewdata <- data[(data$vertical==title),]
trueviewdata<-trueviewdata[!(is.na(trueviewdata$Clicks)),]

trueviewdata$vid_level.Clicks<-as.numeric(sub(",", "", trueviewdata$vid_level.Clicks, fixed = TRUE))
trueviewdata$vid_level.Views<-as.numeric(sub(",", "", trueviewdata$vid_level.Views, fixed = TRUE))
trueviewdata$vid_level.Earned.views<-as.numeric(sub(",", "", trueviewdata$vid_level.Earned.views, fixed = TRUE))
trueviewdata$vid_level.View.Rate<-as.numeric(sub("%", "", trueviewdata$vid_level.View.Rate, fixed = TRUE))

# convert all interests and topics NA to 0
cols_with_i <-  grep("i_", colnames(trueviewdata))
trueviewdata[,cols_with_i][is.na(trueviewdata[,cols_with_i])] <- 0

cols_with_t <-  grep("t_", colnames(trueviewdata))
trueviewdata[,cols_with_t][is.na(trueviewdata[,cols_with_t])] <- 0
# convert all interests and topics NA to 0

data2<-trueviewdata
data2<-data2[!(is.na(data2$duration)),]

data2$CTR_FS <- 100*data2$Clicks/(data2$Views+1)
#data2$numTopics <- length(grep("t_", colnames(data2)))
#data2$numInterests <- length(grep("i_", colnames(data2)))
data2$earned_norm<-100*data2$Earned.views/(data2$Views+1)

#data2$KPI <- (1/3)*data2$CTR_FS + (1/3)*(data2$View.Rate) + (1/3)*data2$earned_norm

# data2$YY[data2$Y >=10 ] <- "HIGH"
# data2$YY[data2$Y < 10 & data2$Y >= 7] <- "GOOD"
# data2$YY[data2$Y < 7] <- "BAD"
# data2$YY<-factor(data2$YY)

data2$gender1[(data2$male==1) & (data2$female==1)] <- 'both'
data2$gender1[(data2$male==1) & (data2$female==0)] <- 'male only'
data2$gender1[(data2$male==0) & (data2$female==1)] <- 'female only'
data2$gender1[(data2$male==0) & (data2$female==0)] <- 'both'


#  data3<-data2[,c(1:66,grep("KPI",colnames(data2)))]
#  write.csv(data3,paste(title,'_withKPI.csv'))


#
# data2<-data2[!(data2$CTR_FS>0.10),]
# data2<-data2[!(data2$c_views30days<0),]
# data2<-data2[!(data2$c_comments30days<0),]
# data2<-data2[!(data2$c_shares30days<0),]
# data2<-data2[!(data2$c_mp30days<0),]
#
# data2<-data2[!(data2$X.day7_earnings<0),]
# data2<-data2[!(data2$X.day7_monetizedPlaybacks<0),]
# data2<-data2[!(data2$X.day7_averageViewDuration<0),]
# data2<-data2[!(data2$X.day7_estimatedMinutesWatched<0),]
# data2<-data2[!(data2$X.day7_grossRevenue<0),]
#
# data2<-data2[!(data2$X.lt_earnings<0),]
# data2<-data2[!(data2$X.lt_monetizedPlaybacks<0),]
# data2<-data2[!(data2$X.lt_grossRevenue<0),]
# data2<-data2[!(data2$X.lt_estimatedMinutesWatched<0),]
# data2<-data2[!(data2$X.lt_averageViewDuration<0),]
#
# data2$sub_7[(data2$sub_7<0)] <- 0
# data2$sub_15[(data2$sub_15<0)] <- 0
# data2$sub_30[(data2$sub_30<0)] <- 0
#
#
# data2<-data2[!((is.na(data2$DATES_1))),]
# data2<-data2[!((is.na(data2$LIKES_1))),]
# data2<-data2[!((is.na(data2$DISLIKES_1))),]
# data2<-data2[!((is.na(data2$COMMENTS_1))),]
# data2<-data2[!((is.na(data2$VIEWS_1))),]
#
# data2<-data2[!((is.na(data2$sub_7))),]
# data2<-data2[!((is.na(data2$sub_15))),]
# data2<-data2[!((is.na(data2$sub_30))),]
#
# # Bucketize sub_7
# buckets <- c(0,60,150,250,Inf)
# data2$duration_buck <- cut(data2$duration,buckets,include.lowest=TRUE)
# data2 <- cbind(data2,model.matrix(~data2$duration_buck+0))
#
# # Bucketize DATES_
# buckets <- c(-1,0,3,5,Inf)
# data2$DATES_1buck <- cut(data2$DATES_1,buckets,include.lowest=FALSE)
# data2 <- cbind(data2,model.matrix(~data2$DATES_1buck+0))
#
#
# # Bucketize Views_
# buckets <- c(0,1000,10000,100000,Inf)
# data2$VIEWS_1buck <- cut(data2$VIEWS_1,buckets,include.lowest=TRUE)
# data2 <- cbind(data2,model.matrix(~data2$VIEWS_1buck+0))


# fmla=as.formula(paste("Y ~ 0+",paste(colnames(data2)[grep("i_", colnames(data2))],collapse="+"),'+',paste(colnames(data2)[grep("i_", colnames(data2))],collapse="+")))
# print(fmla)
# #weights <- information.gain(fmla, data2)
# weights <- gain.ratio(fmla,data2)
# subset <- cutoff.k(weights,5)
# print(subset)




#MODEL1
# fmla <- as.formula(paste("YY ~ 0 + female + male",'+',paste(colnames(data2)[grep("ii_", colnames(data2))],collapse="+"),'+',paste(colnames(data2)[grep("tt_", colnames(data2))],collapse="+"),'+',paste(colnames(data2)[grep("agegroup_", colnames(data2))],collapse="+"),'+',paste(colnames(data2)[grep("Video.played.to", colnames(data2))],collapse="+")))

#MODEL2
#fmla <- as.formula(paste("(Y) ~ -1+numTopics  + female + male +Video.played.to..25.+Video.played.to..50.+Video.played.to..75.+Video.played.to..100.",'+',paste(colnames(data2)[grep("agegroup_", colnames(data2))],collapse="+")))


#MODEL3
# fmla <- as.formula(paste("(Y) ~gender1+I(fb+tw)+I(vine+insta+tum)+count_25+count_50+count_75+count_100",'+',paste(colnames(data2)[grep("^t_", colnames(data2))],collapse="+"),'+',paste(colnames(data2)[grep("^agegroup_", colnames(data2))],collapse="+")))

# fmla <- as.formula(paste("(Y) ~ 0+gender1+I(count_25+count_50)+I(count_75+count_100)+I(agegroup_1824+agegroup_2534)+I(agegroup_3544+agegroup_4554+agegroup_5564)+t_Mail...Package.Delivery+I(t_Solar.Power+t_Wind.Power+t_Hydropower)+t_Business...Industrial+I(t_Computers...Electronics +t_Engineering...Technology +t_Science)"))

# ddd<-data2[,grep("^t_",colnames(data2))]
# View(cor(ddd))

#nonprofit
# fmla <- as.formula(paste("(Y) ~ 0+gender1+I(fb+tw)+I(vine+insta+tum)+I(duration-mean(duration))+count_25+count_50+count_75+count_100+t_Arts...Entertainment+t_People...Society+t_Beauty...Fitness+t_Travel+t_Shopping+t_Food...Drink+t_Health+t_Home...Garden+t_Health.Conditions+t_Financial.Planning...Management+I(t_Retirement...Pension+t_Insurance)+t_Health.News"))

#publishing
# fmla <- as.formula(paste("(Y) ~ 0+gender1+I(fb+tw)+I(vine+insta+tum)+I(duration-mean(duration))+count_25+count_50+count_75+count_100+agegroup_1824+agegroup_3544+agegroup_4554+agegroup_5564+t_Travel+t_News+t_Business...Industrial"))

#entertainment
data2$CTR_FS_scale <- asinh(data2$CTR_FS)
data2$earned_norm_scale <- asinh(data2$earned_norm)
data2$VR_scale <- asinh(data2$View.Rate)
Ytype='CTR_FS' #earned_norm #CTR_FS #vid_level.View.Rate

fmla <- as.formula(paste(Ytype," ~ 0+I(fb+tw)+I(vine+insta+tum)+I(duration-mean(duration))+count_25+count_50+count_75+count_100+agegroup_1824+agegroup_3544"))



### CARET SVM MODEL ###
ctrl <- trainControl(method="cv",number = 10)

#svmfit<- train(fmla,data=data2,method="logreg",tuneLength=10,trControl=ctrl,intercept = TRUE)
# svmfit<- train(fmla,data=data2,method="rf",tuneLength=10,trControl=ctrl)
# rfmodel<-randomForest(fmla,data=data2)
lmfit<- train(fmla,data=data2,method="lm",trControl=ctrl,has_intercept=FALSE)
print(summary(lmfit))

dddd<-summary(lmfit)
filename=paste(title,Ytype,sep = '_')

wb <- loadWorkbook("/Users/vincentseah/Desktop/PTV/analysis/R_trueview/aab.xlsx")
#wb <- createWorkbook()
sheet <- createSheet(wb, sheetName=filename)

xlsxdata<-(rbind(dddd$coefficients,c(dddd$r.squared,'','',''),c(dddd$sigma,'','',''),c(mean(data2$duration),'','','')))
whichempty<-which(row.names(xlsxdata)=='')
additional_rownames <- rbind('r^2','sigma','mean(duration)')
for (i in 1:3){
  row.names(xlsxdata)[whichempty[i]]<-additional_rownames[i]
}

#data <- data.frame(rbind(dddd$coefficients))
addDataFrame(xlsxdata, sheet, startRow=3, startColumn=2)
saveWorkbook(wb, "aab.xlsx")

###########################################################

yhat<-predict(svmfit,data2)

yhat_probs<-predict(svmfit,data2,type="prob")
### CARET SVM MODEL ###


### LIBLINEAR MODEL ###
colnumbers<-grep("\\<female\\>|\\<male\\>|\\<agegroup_|Video.played.to", colnames(data2))

model_lib<-LiblineaR(data2[,colnumbers],data2$YY,type=0,cost=4,bias=0)

### LIBLINEAR MODEL ###




######## R LM MODEL
for (j in 1:1) {
  ## 75% of the sample size
  smp_size <- floor(0.75 * nrow(data2))
  ## set the seed to make your partition reproductible
  #set.seed(445)
  train_ind <- sample(seq_len(nrow(data2)), size = smp_size)
  mdtrain <- data2[train_ind, ]
  mdtest <- data2[-train_ind, ]
  modeltrain <- glm(fmla, data = mdtrain,family=gaussian)
  if (j==1) {
    coef<-modeltrain$coeff
  }
  else {
    coef<-coef+modeltrain$coeff
  }
}

avg_coef=coef/j
modeltrain$coefficients <- avg_coef
View((modeltrain$coeff))

modeltrain_evo<-step(modeltrain,direction = "both", trace=0, scope = list(lower = ~ male+female+Video.played.to..25.+Video.played.to..50.+Video.played.to..75.+Video.played.to..100.))
print(summary(modeltrain_evo))
View(modeltrain_evo$coeff)


y_hat <- predict(modeltrain_evo,newdata=mdtest,interval="c", level=0.95)
y_val <- data.frame(obs=mdtest$Y,pred=y_hat[,1])
print(defaultSummary(y_val))
