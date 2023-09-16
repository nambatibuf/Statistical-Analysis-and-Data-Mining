library(corrr)
library(corrplot)
library("FactoMineR")
library(ggplot2)
library(rpart) # for fitting CART-like decision trees
library(randomForest) # for fitting RFs
library(hydroGOF)
library(corrplot)
library(stats)
library(dplyr)
library(ggplot2)
library(vip)

csv_form_data <- read.csv("eas508_sp23_exam1_data1.csv")
csv_form_data=na.omit(csv_form_data)
csv_form_data2<-csv_form_data
data_frame2<-csv_form_data[-c(1:10),]
df1<-csv_form_data[c(11:40),]
df2<-csv_form_data[c(11:40),]
df3 <- df2[ ,unlist(lapply(df2,is.numeric))]
df1<-df1[,c(1)]
df1 <- as.numeric(df1)
colnames(goal_prop)[1] <- "PC1"
colnames(goal_prop)[2] <- "PC2"
colnames(goal_prop)[3] <- "PC3"
colnames(goal_prop)[4] <- "PC4"
colnames(goal_prop)[5] <- "PC5"
colnames(goal_prop)[6] <- "PC6"
colnames(goal_prop)[7] <- "PC7"
colnames(goal_prop)[8] <- "PC8"
colnames(goal_prop)[9] <- "PC9"
colnames(goal_prop)[10] <- "PC10"
colnames(goal_prop)[11] <- "PC11"
colnames(goal_prop)[12] <- "PC12"
colnames(goal_prop)[13] <- "PC13"
colnames(goal_prop)[14] <- "PC14"

data_frame3<-csv_form_data[c(1:10),]
goal_prop<-data_frame3[c(-1)]

data_frame2 <- data_frame2[ ,unlist(lapply(data_frame2,is.numeric))]
data_frame2=as.data.frame(scale(data_frame2))


#CORR
data_matrix=data.matrix(data_frame2)
corr_matrix <- cor(data_matrix)
corrplot(corr_matrix, tl.col = "red", bg = "White", tl.srt = 35, 
         title = "Properties Variable Selection",
         addCoef.col = "black", type = "full")
corrplot(corr_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#PCA
pca1 <- prcomp(data_frame2)
var_per_pc<- (pca1$sdev)^2/sum(pca1$sdev^2)
round(var_per_pc, 3)   #rounding to 3 digits
loads <- pca1$rotation
score <- pca1$x
qplot(c(1:16), var_per_pc) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance") +
  ggtitle("Scree-Plot") +
  ylim(0, 0.5)

#VIP Calculation
#update loadings w/ reduced no. of PCs
loads_vip=loads[,-c(4,8,9,11,12,13,14,15,16)]
property_vip<-loads_vip[1,]
features_vip<-loads_vip[2:16,]
weight_vip<-property_vip*features_vip
#no. of weights should be equal to number of PCs included
vip<-weight_vip[,1]+weight_vip[,2]+weight_vip[,3]+weight_vip[,4]+weight_vip[,5]+weight_vip[,6]+weight_vip[,7]
barplot(vip)

#Multi linear
set.seed(508)
Property <- df1
data1_mlr <- cbind(Property,data_frame2[,-c(8,11,12,13,14,15,16)])
data_sort_mlr <- sample(1:nrow(data1_mlr), nrow(data1_mlr)*.8)
train_mlr <- data1_mlr[data_sort_mlr,]
test_mlr <- data1_mlr[-data_sort_mlr,]
mdl_mlr <- lm(Property~., data=train_mlr)
pred_train_mlr <- predict(mdl_mlr, train_mlr)
pred_test_mlr <- predict(mdl_mlr, test_mlr)
rmse_mlr_train <- rmse(pred_train_mlr, train_mlr$Property)
rmse_mlr_test <- rmse(pred_test_mlr, test_mlr$Property)
plot(train_mlr$Property, pred_train_mlr, xlab = "Actual", ylab = "Predicted")
sst <- sum((train_mlr$Property - mean(train_mlr$Property))^2)
sse <- sum((pred_train_mlr - train_mlr$Property)^2)
rsq_mlr<- 1-sse/sst
rsq_mlr
pred_mlr=predict(mdl_mlr,goal_prop)
pred_mlr

#PCR
prop_PCR <- df2$Property
prop_PCR<-as.numeric(prop_PCR)
feature_PCR <- df2[, !names(df2)%in%c("Property")]
feature_PCR_std <- as.data.frame(scale(feature_PCR))
pca<- prcomp(feature_PCR_std)
plot(pca$sdev)
scores<-pca$x[, 1:10]
loads<- pca$rotation[, 1:10]
prop_vip <- loads[1,]
features_vip <- loads[2:16,]
weight_vip<- prop_vip*features_vip
vip<-weight_vip[,1]+weight_vip[,2]+weight_vip[,3]+weight_vip[,4]+weight_vip[,5]+weight_vip[,6]+weight_vip[,7]+weight_vip[,8]+weight_vip[,9]+weight_vip[,10]
barplot(vip)
pcr_data<- cbind(prop_PCR, scores)
pcr_data<- as.data.frame(pcr_data)
set.seed(712)
data1_sort_pcr<- sample(1:nrow(pcr_data), nrow(pcr_data)*.8)
train_pcr <- pcr_data[data1_sort_pcr,]
test_pcr <- pcr_data[-data1_sort_pcr,]

mdl_pcr <- lm(prop_PCR~., data=train_pcr)
pred_train_pcr <- predict(mdl_pcr, train_pcr)
pred_test_pcr <- predict(mdl_pcr, test_pcr)

rmse_pcr_train <- rmse(pred_train_pcr, train_pcr$prop_PCR)
rmse_pcr_test <- rmse(pred_test_pcr, test_pcr$prop_PCR)
plot(train_pcr$prop_PCR, pred_train_pcr, xlab = "Actual", ylab = "Predicted")
sst <- sum((train_pcr$prop_PCR - mean(train_pcr$prop_PCR))^2)
sse <- sum((pred_train_pcr - train_pcr$prop_PCR)^2)
rsq_pcr<- 1-sse/sst
rsq_pcr
pred_pcr1=predict(mdl_pcr,goal_prop)
plot(train)

plot(train_pcr$prop_PCR, pred_train_pcr, xlab = "Actual", ylab = "Predicted")

#Linear Regression

#Identify the most correlated feature and use that for LR
#For cars data, wt is most correlated (both from cor and VIP)
Property <- df1
df9 <-data1_mlr[,c(1,11)]
data_lr<-df9
#Divide into training and testing data
mtcars_datasort_lr<-sample(1:nrow(data_lr),nrow(data_lr)*.8)
train_lr<-data_lr[mtcars_datasort_lr,]
test_lr<-data_lr[-mtcars_datasort_lr,]
#Build the model and predict the values
mdl_lr<-lm(Property~Feature13,data=train_lr)
pred_train_lr<-predict(mdl_lr,train_lr)
pred_test_lr<-predict(mdl_lr,test_lr)
#Get RMSE values
rmse_lr_train<-rmse(pred_train_lr,train_lr$Property)
rmse_lr_train
rmse_lr_test<-rmse(pred_test_lr,test_lr$Property)
rmse_lr_test
#R2 value for training data
sst<-sum((train_lr$Property-mean(train_lr$Property))^2)
sse<-sum((pred_train_lr-train_lr$Property)^2)
rsqlr<-1-sse/sst
rsq
pred_lr=predict(mdl_lr,goal_prop)
pred_mlr
plot(train_lr$Property,pred_train_lr,xlab="Actual",ylab="Predicted")


