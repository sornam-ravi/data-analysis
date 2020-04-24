setwd("D:/Data_Analytics")
getwd()

Dataset <- read.csv(file="D:/Data_Analytics/Airbag and other influences on accident fatalities.csv", header=TRUE, sep=",")
summary(Dataset)
Dataset$X <- NULL
Dataset$weight <- NULL
Dataset$dead <- NULL
Dataset$yearacc <- NULL
Dataset$yearVeh <- NULL
Dataset$abcat <- NULL
Dataset$caseid <- NULL

summary(Dataset)
colnames(Dataset)[1] <- "VehicleSpeed"
colnames(Dataset)[2] <- "Airbag"
colnames(Dataset)[3] <- "Seatbelt"
colnames(Dataset)[4] <- "Frontal"
colnames(Dataset)[5] <- "Sex"
colnames(Dataset)[6] <- "OccupantAge"
colnames(Dataset)[7] <- "OccupantRole"
colnames(Dataset)[8] <- "AirbagStatus"
colnames(Dataset)[9] <- "InjurySeverity"

#Changing column datatypes
Dataset$Frontal <- as.factor(Dataset$Frontal)
Dataset$AirbagStatus <- as.factor(Dataset$AirbagStatus)
Dataset$InjurySeverity <- as.factor(Dataset$InjurySeverity)

#Data preprocessing for Vehicle.Speed column
levels(Dataset$VehicleSpeed) <- c(levels(Dataset$VehicleSpeed), "1-9")
Dataset$VehicleSpeed[Dataset$VehicleSpeed=="1-9km/h"]  <- "1-9"

levels(Dataset$VehicleSpeed) <- c(levels(Dataset$VehicleSpeed), "10-24")
Dataset$VehicleSpeed[Dataset$VehicleSpeed=="24-Oct"]  <- "10-24"

Dataset$VehicleSpeed <- factor(Dataset$VehicleSpeed)

Dataset$VehicleSpeed <- factor(Dataset$VehicleSpeed, levels=c("1-9", "10-24", "25-39", "40-54", "55+"))

#Data preprocessing for Airbag column
levels(Dataset$Airbag) <- c(levels(Dataset$Airbag), "Airbag")
Dataset$Airbag[Dataset$Airbag=="airbag"]  <- "Airbag"

levels(Dataset$Airbag) <- c(levels(Dataset$Airbag), "No Airbag")
Dataset$Airbag[Dataset$Airbag=="none"]  <- "No Airbag"

Dataset$Airbag <- factor(Dataset$Airbag)

#Data preprocessing for Seatbelt column
levels(Dataset$Seatbelt) <- c(levels(Dataset$Seatbelt), "Belted")
Dataset$Seatbelt[Dataset$Seatbelt=="belted"]  <- "Belted"

levels(Dataset$Seatbelt) <- c(levels(Dataset$Seatbelt), "Not Belted")
Dataset$Seatbelt[Dataset$Seatbelt=="none"]  <- "Not Belted"

Dataset$Seatbelt <- factor(Dataset$Seatbelt)

#Data preprocessing for Sex column
levels(Dataset$Sex) <- c(levels(Dataset$Sex), "M")
Dataset$Sex[Dataset$Sex=="m"]  <- "M"

levels(Dataset$Sex) <- c(levels(Dataset$Sex), "F")
Dataset$Sex[Dataset$Sex=="f"]  <- "F"

Dataset$Sex <- factor(Dataset$Sex)

#Data preprocessing for Occupant.Role column
levels(Dataset$OccupantRole) <- c(levels(Dataset$OccupantRole), "Driver")
Dataset$OccupantRole[Dataset$OccupantRole=="driver"]  <- "Driver"

levels(Dataset$OccupantRole) <- c(levels(Dataset$OccupantRole), "Passanger")
Dataset$OccupantRole[Dataset$OccupantRole=="pass"]  <- "Passanger"

Dataset$OccupantRole <- factor(Dataset$OccupantRole)

#Data preprocessing for Injury.Severity column (Target variable)
Dataset <- Dataset[!Dataset$InjurySeverity %in% c(5), ]
Dataset$InjurySeverity[Dataset$InjurySeverity==6] <- 5
Dataset$InjurySeverity<- factor(Dataset$InjurySeverity)

#Command to remove NA's from the dataset
Dataset <- na.omit(Dataset)

summary(Dataset)
str(Dataset)

#Plotting preprocessing

barplot(table(Dataset$dvcat), main="Speed Distribution", xlab="dvcat", 
        ylab="Count", col="blue", border="red")

barplot(table(Dataset$airbag), main="Airbag", xlab="airbag", 
        ylab="Count", col="red", border="black")

barplot(table(Dataset$seatbelt), main="seatbelt", xlab="seatbelt", 
        ylab="Count", col="cyan", border="black")

barplot(table(Dataset$frontal), main="Frontal", xlab="frontal", 
        ylab="Count", col="yellow", border="black")

barplot(table(Dataset$sex), main="Gender", xlab="sex", 
        ylab="Count", col="violet", border="black")

barplot(table(Dataset$occRole), main="Occupant Role", xlab="occRole", 
        ylab="Count", col="green", border="black")

barplot(table(Dataset$deploy), main="Airbag Status", xlab="deploy", 
        ylab="Count", col="orange", border="black")

barplot(table(Dataset$injSeverity), main="Injury Severity", xlab="injSeverity", 
        ylab="Count", col="grey", border="black")


#Plotting after processing

hist(Dataset$Occupant.Age, main="Histogram for Occupant Age", xlab="Occupants.Age",
     border="black", col="lightblue") 

barplot(table(Dataset$Vehicle.Speed), main="Speed Distribution",
        xlab="Vehicle.Speed", ylab="Count", col="blue", border="red")

barplot(table(Dataset$Airbag), main="Airbag",
        xlab="Airbag", ylab="Count", col="red", border="black")

barplot(table(Dataset$Seatbelt), main="Seatbelt",
        xlab="Seatbelt", ylab="Count", col="cyan", border="black")

barplot(table(Dataset$Frontal), main="Frontal",
        xlab="Frontal", ylab="Count", col="yellow", border="black")

barplot(table(Dataset$Sex), main="Gender",
        xlab="Sex", ylab="Count", col="violet", border="black")

barplot(table(Dataset$Occupant.Role), main="Occupant Role",
        xlab="Occupant.Role", ylab="Count", col="green", border="black")

barplot(table(Dataset$Airbag.Status), main="Airbag Status",
        xlab="Airbag.Status", ylab="Count", col="orange", border="black")

barplot(table(Dataset$Injury.Severity), main="Injury Severity",
        xlab="Injury.Severity", ylab="Count", col="gray", border="black")

#Reassigned the value of Injury.Severity = 5 to Injury.Severity = 4 to remove compilation error

Dataset$InjurySeverity[Dataset$InjurySeverity==5]  <- 4
Dataset$InjurySeverity <- factor(Dataset$InjurySeverity)

#Randomizing Dataset

Dataset <- Dataset[sample(1:nrow(Dataset)), ]
head(Dataset)

#Splitting the data into training and test data in the ratio 70:30

data_train = Dataset[1:18151,]
data_test = Dataset[18152:25931,]

nrow(data_test)
nrow(data_train)

summary(Dataset)

#Linear Discriminant Analysis
require(ISLR)
require(MASS)

fit = lda(InjurySeverity~. , data=data_train)
fit
pred = predict(fit, data_test)
pred
names(pred)
pred_class = pred$class



table(pred_class, data_test$InjurySeverity)
mean(pred_class == data_test$InjurySeverity)

#Quadratic Discriminant Analysis

fit = qda(InjurySeverity~. , data = data_train)
pred = predict(fit, data_test)
pred
pred_class = pred$class

#Confusion Matrix - LDA

table(pred_class, data_test$InjurySeverity)
mean(pred_class == data_test$InjurySeverity)

#Resampling with Caret package

install.packages("caret")
library(caret)

#LDA with bootstrapping

trctrl <- trainControl(method="boot")
set.seed(333)
lda_boot_fit <- train(InjurySeverity~., data=data_train, method="lda",
                      trControl=trctrl,
                      preProcess=c("center", "scale"),
                      tuneLength=10)
lda_boot_fit
test_pred <- predict(lda_boot_fit, newdata=data_test)
test_pred
confusionMatrix(test_pred, data_test$InjurySeverity)

densityplot(lda_boot_fit, pch = "|")

#LDA with 5-fold

trctrl <- trainControl(method="repeatedcv", number=5)
set.seed(333)
lda_5fold_fit <- train(InjurySeverity~., data=data_train, method="lda",
                       trControl=trctrl,
                       preProcess=c("center", "scale"),
                       tuneLength=10)
lda_5fold_fit
test_pred <- predict(lda_5fold_fit, newdata=data_test)
test_pred
confusionMatrix(test_pred, data_test$InjurySeverity)

densityplot(lda_5fold_fit, pch = "|")

#LDA with 10-fold

library(caret)
trctrl <- trainControl(method="repeatedcv", number=10)
set.seed(333)
lda_10fold_fit <- train(InjurySeverity~., data=data_train, method="lda",
                        trControl=trctrl,
                        preProcess=c("center", "scale"),
                        tuneLength=10)


lda_10fold_fit
test_pred <- predict(lda_10fold_fit, newdata=data_test)
test_pred
confusionMatrix(test_pred, data_test$InjurySeverity)

#Density plot for 10-fold approach

densityplot(lda_10fold_fit, pch = "|")

#LDA with LOOCV

trctrl <- trainControl(method="LOOCV", verboseIter = T)
set.seed(333)
lda_loocv_fit <- train(InjurySeverity~., data=data_train, method="lda",
                       trControl=trctrl,
                       preProcess=c("center", "scale"),
                       tuneLength=20)
lda_loocv_fit
test_pred <- predict(lda_loocv_fit, newdata=data_test)
test_pred
confusionMatrix(test_pred, data_test$InjurySeverity)


#Alternate for LOOCV with small dataset

attach(Dataset)
part=0.95
index <- createDataPartition(Dataset$InjurySeverity, p=part, list=FALSE)
test1 <- Dataset[index,]
train1 <- Dataset[-index,]
nrow(test1)
nrow(train1)

#LDA with LOOCV

trctrl <- trainControl(method="LOOCV", verboseIter = T)
set.seed(333)
lda_loocv_fit <- train(InjurySeverity~., data=train1, method="lda",
                       trControl=trctrl,
                       preProcess=c("center", "scale"),
                       tuneLength=20)
lda_loocv_fit
test_pred <- predict(lda_loocv_fit, newdata=data_test)
test_pred
confusionMatrix(test_pred, data_test$InjurySeverity)

densityplot(lda_loocv_fit, pch = "|")

#Correlation matrix using GoodmanKrusal package

install.packages("GoodmanKruskal")
library(GoodmanKruskal)
Cor_Matrix <- GKtauDataframe(Dataset)
plot(Cor_Matrix, backgroundColor = "white", diagColor = "blue", diagSize = 1)

#####Deliverable 5

#Forward Selection:

library (ISLR)
library (leaps)

#Predict function
predict.regsubsets =function (object ,newdata ,id){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

#Creating folds matrix 
folds=sample(rep(1:10,length=nrow(Dataset)))
folds
table(folds)

cv.errors=matrix(NA,10,8,dimnames=list(NULL, paste(1:8)))
cv.errors

#10 Cross Validation - forward
for(k in 1:10){
  fwd=regsubsets(InjurySeverity~. , data=Dataset[folds!=k,], nvmax=8, method = "forward")
  for(i in 8:1){
    pred=predict.regsubsets(fwd,Dataset[folds==k,],id=i)
    #cv.errors[k,i]=mean((Dataset$InjurySeverity[folds==k]-pred)^2)
    
  }
  
}

fwd.summary<-summary(fwd)
fwd.summary
fwd.summary$rsq

##########Backward Selection

#Creating folds matrix 
folds=sample(rep(1:10,length=nrow(Dataset)))
folds
table(folds)

cv.errors=matrix(NA,10,8,dimnames=list(NULL, paste(1:8)))
cv.errors

#10 Cross Validation - forward
for(k in 1:10){
  bwd=regsubsets(InjurySeverity~. , data=Dataset[folds!=k,], nvmax=8, method = "backward")
  for(i in 8:1){
    pred=predict.regsubsets(bwd,Dataset[folds==k,],id=i)
    #cv.errors[k,i]=mean((Dataset$InjurySeverity[folds==k]-pred)^2)
    
  }
  
}

bwd.summary<-summary(bwd)
bwd.summary
bwd.summary$rsq

######Alternative method for forward and backward selection

set.seed(7)
library(mlbench)
library(caret)
attach(train1)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train1[,1:8], train1[,9], sizes=c(1:8), rfeControl=control)
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))



#Ridge Regression
library(glmnet)
x=model.matrix(InjurySeverity~.,Dataset )[,-1]
y=Dataset$InjurySeverity
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid,family="multinomial")
plot(ridge.mod,xvar="lambda",label=TRUE)
coef(ridge.mod)
ridge.mod$lambda[50] 

#coef(ridge.mod)[50]
#sqrt(sum(coef(ridge.mod)[-1,50]^2))
#ridge.mod$lambda [60]
#coef(ridge.mod)[,60] says incorrect number of dimensions

predict(ridge.mod,s=50,type="coefficients")

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod =glmnet(x[train ,],y[train],alpha =0,lambda=grid,thresh=1e-12,family="multinomial")
plot(ridge.mod,xvar="lambda",label=TRUE)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
plot(ridge.pred)
#mean((ridge.pred-y.test)^2)

#obtaining Same results for large value of lambda
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
#mean(( ridge.pred -y.test)^2)

#implementing csross validation to find out best value of lambda
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha =0,family="multinomial")
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#reffitting rigression model on full dataset using lambda chosen by cross validation
out=glmnet(x,y,alpha =0,family="multinomial")
predict(out,type="coefficients",s=bestlam)


# Lasso Regression

lasso.mod =glmnet(x[train,],y[train],alpha =1,lambda=grid,family="multinomial")
plot(lasso.mod,xvar="lambda",label=TRUE)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1,family="multinomial")
plot(cv.out)
bestlam =cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
plot(lasso.pred)
#mean(( lasso.pred -y.test)^2)

out=glmnet(x,y,alpha =1,lambda =grid,family="multinomial")
lasso.coef=predict(out,type="coefficients",s=bestlam )
bestlam
lasso.coef


##GAM
data_train = Dataset[1:18151,]
data_test = Dataset[18152:25931,]

library(gam)



gam.lr.s0=gam(I(InjurySeverity==0)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=2) ,family =binomial ,data=data_train)
gam.lr.s1=gam(I(InjurySeverity==1)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=2) ,family =binomial ,data=data_train)
gam.lr.s2=gam(I(InjurySeverity==2)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=2) ,family =binomial ,data=data_train)
gam.lr.s3=gam(I(InjurySeverity==3)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=2) ,family =binomial ,data=data_train)
gam.lr.s4=gam(I(InjurySeverity==5)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=2) ,family =binomial ,data=data_train)


gam1.lr.s0=gam(I(InjurySeverity==0)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=4) ,family =binomial ,data=data_train)
gam1.lr.s1=gam(I(InjurySeverity==1)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=4) ,family =binomial ,data=data_train)
gam1.lr.s2=gam(I(InjurySeverity==2)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=4) ,family =binomial ,data=data_train)
gam1.lr.s3=gam(I(InjurySeverity==3)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=4) ,family =binomial ,data=data_train)
gam1.lr.s4=gam(I(InjurySeverity==5)~Sex+VehicleSpeed+Seatbelt+s(OccupantAge ,df=4) ,family =binomial ,data=data_train)





anova(gam.lr.s0,gam1.lr.s0, test="Chisq")
anova(gam.lr.s1,gam1.lr.s1, test="Chisq")
anova(gam.lr.s2,gam1.lr.s2, test="Chisq")
anova(gam.lr.s3,gam1.lr.s3, test="Chisq")
anova(gam.lr.s4,gam1.lr.s4, test="Chisq")


par(mfrow =c(2,2))

plot(gam1.lr.s0,se=T,col='red')
plot(gam1.lr.s1,se=T,col='green')
plot(gam1.lr.s2,se=T,col='blue')
plot(gam1.lr.s3,se=T,col='orange')
plot(gam1.lr.s4,se=T,col='black')


a <-predict(gam1.lr.s3, newdata=data_test)

a




















