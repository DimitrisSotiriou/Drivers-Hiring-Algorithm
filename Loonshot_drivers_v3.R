library (data.table)
library(randomForest)
library(dummies)
library(vcd)
library(caTools)
library(ROCR)
library(adabag)
################# load data and select variables
data<-fread('Loonshot_data_v3.csv')
t(names(data))
data_loon <-data [,c(10,11,18,19,25,26,27,28,29,30,31,32)]

################# pearson correlation - convert to dummy variables
t(names(data_loon))
train_dummy<-dummy.data.frame(data_loon, names = c("Age_Group","Source_of_Hire","Civil_Status", "Kids","NoOfKids","Companies_Group", "Time_to_comute" ,"Other_Source_of_Income", "Experience" ,"Experience_Driver", "Educational_level","Segment_Low") , sep = ".")
corrAttr<-cor(train_dummy, method="pearson")
corrAttr<-as.data.frame(corrAttr)
write.csv(corrAttr,file="corrAttr_test_v1.csv")
cutoff<-0.65
corrAttrIndex<-which(corrAttr > cutoff, arr.ind = TRUE)
print(corrAttrIndex)   ## print correlated variables greater than 65%

###################### correlation Chii-square: in order to get Chi and Contingency for each variable with Segment Low
assocstats(xtabs(~data_loon$Companies_Group+ data_loon$Source_of_Hire))

assocstats(xtabs(~data_loon$Recruiting_Source+ data_loon$Segment_Low))
assocstats(xtabs(~data_loon$Time_to_Comute+ data_loon$Segment_Low))
assocstats(xtabs(~data_loon$Years_of_Experience_Driver+ data_loon$Segment_Low))

################################################# split data to train and test 

spl = sample.split(data_loon$Segment_Low, SplitRatio = 0.8)
train = subset(data_loon, spl==TRUE)
test = subset(data_loon, spl==FALSE)

################################################glm
t(names(data_loon))
str(data_loon)
train<-train[,-c(5,6,11)]  ### select variables to run the model
test<-test[,-c(5,6,11)]  ### select variables to run the model
str(data_loon)

train<-data_loon[,c(12,3,8,5,9)]
train<-data_loon[,c(1,3,4,7,8,9)]
model_glm = glm(Segment_Low ~ . , family="binomial", data = train)
summary(model_glm)


model_glm = glm(Segment_Low ~ . , family="binomial", data = data_loon)
summary(model_glm)


prop.table(table(train$Segment_Low))

# Predictions on the training set
predictTrain = predict(model_glm, data = train, type = "response")

# Confusion matrix on training data
table(train$Segment_Low, predictTrain >= 0.5)
(87+8)/nrow(train) #Accuracy - 81%

#Predictions on the test set
predictTest = predict(model_glm, newdata = test, type = "response")

# Confusion matrix on test set
table(test$Segment_Low, predictTest >= 0.5)
(7+1)/nrow(test) #Accuracy - 73%


############################################## random forest 

data<-fread('Loonshot_data_v2.csv')
t(names(data))
data_loon <-data [,c(5,6,8,9,10,11,18,19,22,23,25,26,28)]
#data_loon <-data [,c(22,23,24,25,27)]
t(names(data_loon))
data_loon[,eval(cols) := lapply(.SD,as.factor), .SD = cols]
str(data_loon)

####################### run all data and variables in order to get the importance of variables
rf_movie=randomForest(Segment_Low~.,data=data_loon,localImp = TRUE)
importance(rf_movie) 


RF_Importance = importance(rf_movie,type = 1,scale = F)

predict(rf_movie,DataKeep,type = "prob")

# Imbalanced Dataset

DataKeepMelt[, .N , by = .(variable,value)][,Prop:= 100*round(N/sum(N),digits = 2), by = .(variable)][variable == "Segment_Low"]

table(rf_movie$y,rf_movie$predicted)

#######################
str(data_loon)
t(names(data_loon))
train<-data_loon[,c(12,3,8,5,9)] ### run the model with the most important vaariables
#train<-data_loon[,c(10,3,8,12,4,9)]
##train<-data_loon[,-c(4,6,12)]
rf_movie=randomForest(Segment_Low~.,data=train,localImp = TRUE,sampsize=c(10,20)) ##classwt=c(1e-02, 1e+02))

rf_movie=randomForest(Segment_Low~.,data=train,localImp = TRUE,classwt=c(1,0.000001 ))

rf_movie=randomForest(Segment_Low~.,data=train,localImp = TRUE,strata = train$Segment_Low, sampsize = ceiling(.832*nrow(train)))


print(rf_movie)
importance(rf_movie) 
varImpPlot(rf_movie)
plot(rf_movie)
getTree(rf_movie)
getTree(rf_movie, k=1, labelVar=T)
grow(rf_movie,5)
plot.rf.tree(rf_movie)
str(train)

###################### check the optimal no of varibles to split the tree the based on OOB error 
############################################################################################################
rf_movie <-randomForest(Segment_Low~.,data=train, ntree=500) 
print(rf_movie)
t(names(train))
### exclude y variable from the below line  
mtry_mov <- tuneRF(train[,-6],train$Segment_Low, ntreeTry=500,
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m_mov <- mtry_mov[mtry_mov[, 2] == min(mtry_mov[, 2]), 1]
print(mtry_mov)
print(best.m_mov)
############################################################################################################
pred_mov=predict(rf_movie,type = "prob") ### get the probs per driver
print(pred_mov)  ### print the probs per driver

#################################### graph for ROC Curve
perf_mov = prediction(pred_mov[,2], train$Segment_Low)
# 1. Area under curve
auc_mov = performance(perf_mov, "auc")
auc_mov
# 2. True Positive and Negative Rate
pred3_mov = performance(perf_mov, "tpr","fpr")
# 3. Plot the ROC curve
plot(pred3_mov,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


################################################################# boosting

set.seed(100)


spl = sample.split(data_loon$Segment_Low, SplitRatio = 0.8)
train = subset(data_loon, spl==TRUE)
test = subset(data_loon, spl==FALSE)


a5=boosting(Segment_Low~.,data_loon,coeflearn="Breiman")
a5$importance      ### get the importance of variables 
p=predict(a5,test)


###################################### bagging
a4=bagging(Segment_Low~.,data_loon,mfinal=60)
summary(a4)
a4$importance    ### get the importance of variables
p=predict(a4,test)
(t=table(p$class,test[,1]))
100*sum(diag(t))/sum(t)
compute.all(t)
