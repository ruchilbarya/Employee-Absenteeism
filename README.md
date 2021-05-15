# Employee-Absenteeism
Machine learnig


---
title: "Employee Absenteeism"
output:
  word_document: default
  html_document:
    df_print: paged
---
#Task 1 

```{r}

libraries = c("dummies","caret","rpart.plot","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine","xlsx")

lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

library(readr)
X_trainwofeature <- read_csv("_trainwofeature")
train <- X_trainwofeature[,-c(1:2)]




X_testwofeature <- read_csv("_testwofeature")
test <- X_testwofeature[,-c(1:2)]


```

#factor
```{r}

train$Transportation.expense[243] = 155
train$Work.load.Average.day[train$ID ==17][13] = 313532
train$Work.load.Average.day[train$ID ==3][108] = 222196
train$Absenteeism.time.in.hours[train$Reason.for.absence == 27][15] = 1
summary(train)
summary(test)
test$Absenteeism.time.in.hours <- cut(
  test$Absenteeism.time.in.hours,
  breaks = c(0,0.1,6, Inf),
  labels = c("Group 1", "Group 2", "Group 3" ),
  right  = FALSE
)


train$Absenteeism.time.in.hours <- cut(
  train$Absenteeism.time.in.hours,
  breaks = c(0,0.1,6, Inf),
  labels = c("Group 1", "Group 2", "Group 3" ),
  right  = FALSE
)
summary(train$Absenteeism.time.in.hours)
```



```{r}
library(BBmisc)

train <- normalize(train)
test <- normalize(test)

```


# PCA 
```{r}
library(psych)


online.pca <- prcomp(train[,-20],5, center=TRUE, scale. = TRUE)
summary(online.pca)


comp1 = principal(train[,-20], nfactors=5, rotate = 'oblimin')
plot(comp1$values[1:19])

loadings6 = as.data.frame(comp1$loadings[1:19,1:5])
theme1 = subset(loadings6[order(loadings6$TC1, decreasing = T),], TC1 >= 0.3, select = c(TC1))
theme2 = subset(loadings6[order(loadings6$TC2, decreasing = T),], TC2 >= 0.3, select = c(TC2))
theme3 = subset(loadings6[order(loadings6$TC3, decreasing = T),], TC3 >= 0.3, select = c(TC3))
theme4 = subset(loadings6[order(loadings6$TC4, decreasing = T),], TC4 >= 0.3, select = c(TC4))
theme5 = subset(loadings6[order(loadings6$TC5, decreasing = T),], TC5 >= 0.3, select = c(TC5))

theme1
theme2
theme3
theme4
theme5

# After running PCA I understand all numeric variables are important

features_PCA = c('Service.time','Age','Month.of.absence','Seasons','Disciplinary.failure','Distance.from.Residence.to.Work','Transportation.expense','Social.drinker','Son','Body.mass.index','Weight','Absenteeism.time.in.hours')
```


# Knn after PCA 
```{r}
library(class)
set.seed(12345)

train_pca <- train[features_PCA]
test_pca <- test[features_PCA]

knnmodel1 <- knn(train_pca[,-12], test_pca[,-12], cl=train_pca$Absenteeism.time.in.hours, k = 25)
table(knnmodel1)
knnmodel1
library(gmodels)
CrossTable(test_pca$Absenteeism.time.in.hours, knnmodel1, prop.chisq=F, prop.c=F, prop.r=F, dnn=c("Actual", "Predicted (KNN)"))


p <- table(knnmodel1, test_pca$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100

Accuracy

prec.knn = 49/66
prec.knn

```


# Kmeans analysis
```{r}

#Start the k-Means analysis using the variable "nc" for the number of clusters
library(NbClust)
set.seed(12345)
nc <- NbClust(train[,-20], min.nc=2, max.nc = 15, method = "kmeans")
print(table(nc$Best.n[1,]))

barplot(table(nc$Best.n[1,]), xlab = "Number of Clusters", ylab = "Number of Criteria", main =
"Number of Clusters Chosen by 26 Criteria")

#Conduct the k-Means analysis using the best number of clusters
set.seed(1234)
n= 2# As per the above bar graph and wssplot
fit.km <- kmeans(train[,-20], n, nstart=25)
print(fit.km$size)
print(fit.km$centers)
#print(aggregate(test[-20], by=list(cluster=fit.km$cluster), mean))
#Use a confusion or truth table to evaluate how well the k-Means analysis performed
ct.km <- table(train$Absenteeism.time.in.hours, fit.km$cluster)
#print(ct.km)

Accuracy <- sum(diag(ct.km))/sum(ct.km)*100

Accuracy

library(Metrics)
#rmse(test$Absenteeism.time.in.hours,ct.km)

```




# Knn without Clustering
```{r}
set.seed(12345)
library(class)
knnmodel <- knn(train[,-20], test[,-20], cl=train$Absenteeism.time.in.hours, k = 25)
table(knnmodel)

library(gmodels)
CrossTable(test$Absenteeism.time.in.hours, knnmodel, prop.chisq=F, prop.c=F, prop.r=F, dnn=c("Actual", "Predicted (KNN)"))

p <- table(knnmodel, test$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100

Accuracy

prec.knn = 52/68
prec.knn

plot(knnmodel)

```

# data with factors 
```{r}

train <- X_trainwofeature

test <- X_testwofeature[,-c(1:2)]
train$Transportation.expense[243] = 155
train$Work.load.Average.day[train$ID ==17][13] = 313532
train$Work.load.Average.day[train$ID ==3][108] = 222196
train$Absenteeism.time.in.hours[train$Reason.for.absence == 27][15] = 1

train <- train[,-c(1:2)]

test$Absenteeism.time.in.hours <- cut(
  test$Absenteeism.time.in.hours,
  breaks = c(0,0.1,6, Inf),
  labels = c("Group 1", "Group 2", "Group 3" ),
  right  = FALSE
)


train$Absenteeism.time.in.hours <- cut(
  train$Absenteeism.time.in.hours,
  breaks = c(0,0.1,6, Inf),
  labels = c("Group 1", "Group 2", "Group 3" ),
  right  = FALSE
)


  train$Reason.for.absence <- as.factor(train$Reason.for.absence)
  train$Day.of.the.week <- as.factor(train$Day.of.the.week)
  train$Seasons <- as.factor(train$Seasons)
  train$Disciplinary.failure <- as.factor(train$Disciplinary.failure)
  train$Social.drinker <- as.factor(train$Social.drinker)
  train$Social.smoker <- as.factor(train$Social.smoker)
  train$Son <- as.factor(train$Son)
  train$Education <- as.factor(train$Education)
  train$Pet <- as.factor(train$Pet)
 
  
  test$Reason.for.absence <- as.factor(test$Reason.for.absence)
  test$Day.of.the.week <- as.factor(test$Day.of.the.week)
  test$Seasons <- as.factor(test$Seasons)
  test$Disciplinary.failure <- as.factor(test$Disciplinary.failure)
  test$Social.drinker <- as.factor(test$Social.drinker)
  test$Social.smoker <- as.factor(test$Social.smoker)
  test$Son <- as.factor(test$Son)
  test$Education <- as.factor(test$Education)
  test$Pet <- as.factor(test$Pet)
 
  
  
train_nu <- train[,c(2,5:10,17:19)]
test_nu <- test[,c(2,5:10,17:19)]
library(BBmisc)

train <- normalize(train)
test <- normalize(test)

```







# Decision trees  

```{r}
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

Dec_model = rpart(Absenteeism.time.in.hours~ .,data = train)

library(gmodels)
library(factoextra)
library(ggplot2)
fancyRpartPlot(Dec_model)
#Now use the predict() function to see how well the model works
pred <- predict(Dec_model, test, type="class")
CrossTable(test$Absenteeism.time.in.hours, pred, prop.chisq=F, prop.c=F, prop.r=F, dnn=c("Actual", "Predicted (RT)"))


Dec_model$variable.importance


p <- (table(pred, test$Absenteeism.time.in.hours))
Accuracy <- sum(diag(p))/sum(p)*100

Accuracy

library(pROC)
pred.dtr = as.numeric(predict(Dec_model, test, type="class"))
roc.multi = multiclass.roc(test$Absenteeism.time.in.hours, pred.dtr)
auc.dtr = auc(roc.multi)
auc.dtr

rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))


ggplot(data=test, aes(x=Absenteeism.time.in.hours, y=74, fill=factor(pred))) + geom_bar(stat="identity")+
    scale_fill_manual(values = c("light yellow", "light pink","light blue"),
                    labels = c("Group 1", "Group 2", "Group 3"),
                    name = "Predicted ") +   xlab("Actual") +
  ylab("Predicted") + ggtitle("Decision Trees")
```




# Random forest  Model  
```{r}

 test <- rbind(train[1, ] , test)
test <- test[-1,]

library(e1071)
library(randomForest)
library(Metrics)
random_model <- randomForest(Absenteeism.time.in.hours ~ . , data= train)
random_model

absent_pred <- predict(random_model, test) 
p <- table(absent_pred, test$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100
Accuracy
hist(random_model$importance)


print(postResample(absent_pred,test$Absenteeism.time.in.hours))


library(pROC)
pred.rf = as.numeric(predict(random_model, test, type = 'response'))
roc.multi = multiclass.roc(test$Absenteeism.time.in.hours, pred.rf)
auc.rf = auc(roc.multi)
auc.rf

rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
plot(random_model)

ggplot(data=test, aes(x=Absenteeism.time.in.hours, y=74, fill=factor(absent_pred))) + geom_bar(stat="identity")+
    scale_fill_manual(values = c("light yellow", "light pink","light blue"),
                    labels = c("Group 1", "Group 2", "Group 3"),
                    name = "Predicted ") +   xlab("Actual") +
  ylab("Predicted") + ggtitle("Random Forest ")
```

# Random Forest with feature Selection
```{r}

features_rm <- c('Reason.for.absence','Work.load.Average.day','Transportation.expense','Day.of.the.week','Month.of.absence','Age','Service.time','Absenteeism.time.in.hours')

trainf <- train[features_rm]
testf <- test[features_rm]

random_model2 <- randomForest(Absenteeism.time.in.hours ~ . , data= trainf)



absent_pred2 <- predict(random_model2, testf) 
p2 <- table(absent_pred2, testf$Absenteeism.time.in.hours)
Accuracy2 <- sum(diag(p2))/sum(p2)*100
Accuracy2
#importance(random_model2)

plot(random_model2,main="Random Forest with Feature selection")
pred.rfs = as.numeric(predict(random_model2, testf, type = 'response'))
roc.multi = multiclass.roc(testf$Absenteeism.time.in.hours, pred.rfs)
auc.rfs = auc(roc.multi)
auc.rfs

CrossTable(testf$Absenteeism.time.in.hours, absent_pred2, prop.chisq=F, prop.c=F, prop.r=F, dnn=c("Actual", "Predicted (KNN)"))

rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

ggplot(data=testf, aes(x=Absenteeism.time.in.hours, y=74, fill=factor(absent_pred2))) + geom_bar(stat="identity")+
    scale_fill_manual(values = c("light yellow", "light pink","light blue"),
                    labels = c("Group 1", "Group 2", "Group 3"),
                    name = "Predicted ") +   xlab("Actual") +
  ylab("Predicted") + ggtitle("Random Forest with Feature Selection")
```



#Naive Bayes

```{r}

library(naivebayes)
naive_model <- naive_bayes(Absenteeism.time.in.hours ~ . , data= train)

naive_pred <- predict(naive_model, test) 
p <- table(naive_pred, test$Absenteeism.time.in.hours)

CrossTable(test$Absenteeism.time.in.hours, naive_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('Actual  ', 'Predicted '))


Accuracy <- sum(diag(p))/sum(p)*100

Accuracy

pred.nb = as.numeric(predict(naive_model, test))
roc.multi = multiclass.roc(test$Absenteeism.time.in.hours, pred.nb)
auc.nb = auc(roc.multi)
auc.nb

rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

 ggplot(data=test, aes(x=Absenteeism.time.in.hours, y=74, fill=factor(naive_pred))) + geom_bar(stat="identity")+
    scale_fill_manual(values = c("light yellow", "light pink","light blue"),
                    labels = c("Group 1", "Group 2", "Group 3"),
                    name = "Predicted ") +   xlab("Actual") +
  ylab("Predicted") + ggtitle("Naive Bayes ")
 naive_model

```

# Naive Bayes with classifier 

```{r}

library(corrplot)
library(caret)

#Randomize the data
absent_rand <- train_nu[order(runif(nrow(train_nu))),]
#Scale the data
absentDataScaled <- scale(absent_rand, center=TRUE, scale = TRUE)
#Compute the correlation matrix (note that this does not include the class variable)
m <- cor(absentDataScaled)

highlycor <- findCorrelation(m,.5)
highlycor
train
filteredData <- train[, -c(7,19)] 
filteredtest <- test[, -c(7,19)]
nb_model <- naive_bayes(Absenteeism.time.in.hours ~ ., data=filteredData)
nb_model
filteredTestPred <- predict(nb_model, newdata = filteredtest)
p <- table(filteredTestPred, filteredtest$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100

Accuracy

pred.nbc = as.numeric(predict(nb_model, filteredtest))
roc.multi = multiclass.roc(filteredtest$Absenteeism.time.in.hours, pred.nbc)
auc.nbc = auc(roc.multi)
auc.nbc

rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

# BMI and Service time


ggplot(data=filteredtest, aes(x=Absenteeism.time.in.hours, y=74, fill=factor(filteredTestPred))) + geom_bar(stat="identity")+
    scale_fill_manual(values = c("light yellow", "light pink","light blue"),
                    labels = c("Group 1", "Group 2", "Group 3"),
                    name = "Predicted ") +   xlab("Actual") +
  ylab("Predicted") + ggtitle("Naive Bayes with Classifier")

```


# SVM with vanilladot
```{r}

library(kernlab)

svm_classifier <- ksvm(Absenteeism.time.in.hours ~ ., data=train,kernel ="vanilladot")
svm_classifier

pred_svm <- predict(svm_classifier, test) 

p<-table(pred_svm, test$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100

Accuracy

pred.svm = as.numeric(predict(svm_classifier, test))
roc.multi = multiclass.roc(test$Absenteeism.time.in.hours, pred.svm)
auc.svm = auc(roc.multi)
auc.svm

rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))


ggplot(data=test, aes(x=Absenteeism.time.in.hours, y=74, fill=factor(pred_svm))) + geom_bar(stat="identity")+
    scale_fill_manual(values = c("light yellow", "light pink","light blue"),
                    labels = c("Group 1", "Group 2", "Group 3"),
                    name = "Predicted ") +   xlab("Actual") +
  ylab("Predicted") + ggtitle("SVM with vanilladot ")


```




# SVM with rbf

```{r}



svm_rbf <- ksvm(Absenteeism.time.in.hours ~ ., data=train,kernel ="rbfdot")
svm_classifier

pred_svm3 <- predict(svm_rbf, test) 

p<-table(pred_svm3, test$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100

Accuracy

pred.rbf = as.numeric(predict(svm_rbf, test))
roc.multi = multiclass.roc(test$Absenteeism.time.in.hours, pred.rbf)
auc.rbf = auc(roc.multi)
auc.rbf

rs <- roc.multi[['rocs']]
plot.roc(rs[[1]])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

ggplot(data=test, aes(x=Absenteeism.time.in.hours, y=74, fill=factor(pred_svm3))) + geom_bar(stat="identity")+
    scale_fill_manual(values = c("light yellow", "light pink","light blue"),
                    labels = c("Group 1", "Group 2", "Group 3"),
                    name = "Predicted ") +   xlab("Actual") +
  ylab("Predicted") + ggtitle("SVM with rbfdot")

```

# Task 2 
```{r}

train <- X_trainwofeature

test <- X_testwofeature[,-c(1:2)]
train$Transportation.expense[243] = 155
train$Work.load.Average.day[train$ID ==17][13] = 313532
train$Work.load.Average.day[train$ID ==3][108] = 222196
train$Absenteeism.time.in.hours[train$Reason.for.absence == 27][15] = 1

train <- train[,-c(1:2)]

test <- X_testwofeature[,-c(1:2)]
colnames(train)


```



# variables
```{r}

  train$Reason.for.absence <- as.factor(train$Reason.for.absence)
  train$Month.of.absence <- as.factor(train$Month.of.absence)
  train$Day.of.the.week <- as.factor(train$Day.of.the.week)
  train$Seasons <- as.factor(train$Seasons)
  train$Disciplinary.failure <- as.factor(train$Disciplinary.failure)
  train$Social.drinker <- as.factor(train$Social.drinker)
  train$Social.smoker <- as.factor(train$Social.smoker)
  train$Son <- as.factor(train$Son)
  train$Education <- as.factor(train$Education)
  train$Pet <- as.factor(train$Pet)
 
  
  test$Reason.for.absence <- as.factor(test$Reason.for.absence)
  test$Month.of.absence <- as.factor(test$Month.of.absence)
  test$Day.of.the.week <- as.factor(test$Day.of.the.week)
  test$Seasons <- as.factor(test$Seasons)
  test$Disciplinary.failure <- as.factor(test$Disciplinary.failure)
  test$Social.drinker <- as.factor(test$Social.drinker)
  test$Social.smoker <- as.factor(test$Social.smoker)
  test$Son <- as.factor(test$Son)
  test$Education <- as.factor(test$Education)
  test$Pet <- as.factor(test$Pet)
 
summary(train)
summary(test)

```
# Assumptions
```{r}
train_nu <- train[,-c(1:4,11:16)]
###Assumptions for linear regression 

modeltime = lm(Absenteeism.time.in.hours~.,train)
summary(modeltime)

#Additivity 
correl = cor(train_nu)
symnum(correl)
#High Correlation between Weight and BMI so removing BMI from analysis,Age and Service time also have high correlation but it is less than 0.7 
#linearity 
standardized = rstudent(modeltime)
fitted = scale(modeltime$fitted.values)
qqnorm(standardized)
abline(0,1)
# yes , it met the assumption of linearity  as the dots are close to line

#Normality
hist(standardized)

library(moments)
skewness(train_nu)
kurtosis(train_nu)
#Kurtosis ,Height , Weight , Disciplinaryfailure , pet , Social Smoker, Work.load.Average.day, education 
shapiro.test(modeltime$residuals)

# Height and WorkloadAverage day have high values , so we will do boots strap to check the results if it is ithin coinfidence interval or we will use transformation

#does not met the assumption of normality as the p value is less 0.05

#Heterocadasticity

plot(fitted, standardized)
abline(0,0)
abline(v = 0)
library(car)
#vif(modeltime)
#Height ,Weight ,BMI should be removed
#It does meet the assumption of Homogeneity and Homoscedasticity 
#as the variance is spread evenly across y axis from -2 to +2,also VIF is less than 5

#Removing Height , BMI ,Service time 
```



# Stepwise Selection before PCA 
```{r}

model <- lm(Absenteeism.time.in.hours~.-Height-Body.mass.index-Disciplinary.failure,train)
summary(model)

model2.bw = step(model, direction = 'backward')
summary(model2.bw)
#Step:  AIC=-152.44
#AIC=-193.42
#lm(formula = Absenteeism.time.in.hours ~ Reason.for.absence + 
#    Distance.from.Residence.to.Work + Social.drinker, data = train)
#Absenteeism.time.in.hours ~ Reason.for.absence + Month.of.absence + 
#    Distance.from.Residence.to.Work + Hit.target + Son + Social.drinker
#Residual standard error: 0.846 on 636 degrees of freedom
#Multiple R-squared:  0.2418,	Adjusted R-squared:  0.2072 
#F-statistic: 6.994 on 29 and 636 DF,  p-value: < 2.2e-16

#[,-c(19,18,11)]
absent_pred <- predict(model2.bw, test[,-c(18,19,11)]) 
p <- table(absent_pred, test[,-c(18,19,11)]$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100
Accuracy
AIC(model2.bw)
print(postResample(pred = absent_pred, obs = test$Absenteeism.time.in.hours))

plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="blue")
lines(absent_pred,col="red")
title(main= "Linear Regression")

```

# Normalize
```{r}

total <- rbind(train,test)
train <- total[1:666,] 
test <- total[667:740,] 


```








# Random forest before pca
```{r}

trainnew <- train[,-c(18,19,11)]
testnew <- test[,-c(18,19,11)]
colnames(trainnew)
colnames(test)

random_model <- randomForest(Absenteeism.time.in.hours ~ . , data= trainnew)
random_model
absent_pred <- predict(random_model, testnew) 
p <- table(absent_pred, testnew$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100
Accuracy
random_model$importance



print(postResample(pred = absent_pred, obs = testnew$Absenteeism.time.in.hours))

plot(testnew$Absenteeism.time.in.hours,type="l",lty=2,col="blue")
lines(absent_pred2,col="red")

x <- c("Hit.target","Work.load.Average.day","Weight","Age","Day.of.the.week","Month.of.absence","Reason.for.absence","Absenteeism.time.in.hours")

train2 <- train[x]
test2 <- test[x]
random_model2 <- randomForest(Absenteeism.time.in.hours ~., data= train2)
random_model2
absent_pred2 <- predict(random_model2, test2)
p <- table(absent_pred2, test$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100
Accuracy

print(postResample(pred = absent_pred2, obs = test$Absenteeism.time.in.hours))

plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="blue")
lines(absent_pred2,col="red")
title(main= "Random forest with feature selection")

```


# PCA
```{r}

train <- X_trainwofeature

test <- X_testwofeature[,-c(1:2)]
train$Transportation.expense[243] = 155
train$Work.load.Average.day[train$ID ==17][13] = 313532
train$Work.load.Average.day[train$ID ==3][108] = 222196
train$Absenteeism.time.in.hours[train$Reason.for.absence == 27][15] = 1

train <- train[,-c(1:2)]

test <- X_testwofeature[,-c(1:2)]

  train$Reason.for.absence <- as.factor(train$Reason.for.absence)
  train$Month.of.absence <- as.factor(train$Month.of.absence)
  train$Day.of.the.week <- as.factor(train$Day.of.the.week)
  train$Seasons <- as.factor(train$Seasons)
  train$Disciplinary.failure <- as.factor(train$Disciplinary.failure)
  train$Social.drinker <- as.factor(train$Social.drinker)
  train$Social.smoker <- as.factor(train$Social.smoker)
  train$Son <- as.factor(train$Son)
  train$Education <- as.factor(train$Education)
  train$Pet <- as.factor(train$Pet)
 
  
  test$Reason.for.absence <- as.factor(test$Reason.for.absence)
  test$Month.of.absence <- as.factor(test$Month.of.absence)
  test$Day.of.the.week <- as.factor(test$Day.of.the.week)
  test$Seasons <- as.factor(test$Seasons)
  test$Disciplinary.failure <- as.factor(test$Disciplinary.failure)
  test$Social.drinker <- as.factor(test$Social.drinker)
  test$Social.smoker <- as.factor(test$Social.smoker)
  test$Son <- as.factor(test$Son)
  test$Education <- as.factor(test$Education)
  test$Pet <- as.factor(test$Pet)
 
summary(train)
summary(test)

library(fastDummies)

trainnew <- train[,-c(18,19,11)]
testnew <- test[,-c(18,19,11)]

total <- rbind(trainnew,testnew)
dummynew <- fastDummies::dummy_cols(total,remove_selected_columns=TRUE)

len <- nrow(train)
train <- dummynew[1:666,] 
test <- dummynew[667:740,] 

train <- normalize(train)
test <- normalize(test)

pcatrain <- train[,-8]
pcatest <- test[,-8]
#principal component analysis
prin_comp <- prcomp(pcatrain, center = TRUE)
prin_comp$rotation
dim(prin_comp$x)
std_dev <- prin_comp$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
             ylab = "Proportion of Variance Explained",
             type = "b")

plot(cumsum(prop_varex), xlab = "Principal Component",
              ylab = "Cumulative Proportion of Variance Explained",
              type = "b")

train.data <- data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, prin_comp$x)

train.data <- train.data[,1:60]

test.data <- predict(prin_comp, newdata = test)
test.data <- as.data.frame(test.data)


test.data <- test.data[,1:60]
colnames(test.data)

```



#Random Model 
```{r}

#Train the model using training data
rf_model = randomForest(Absenteeism.time.in.hours~., data = train.data, ntrees = 10000)
rf_model
#Predict the test cases
rf_predictions = predict(rf_model,test.data)


#Calcuate MAE, RMSE, R-sqaured for testing data 
print(postResample(pred = rf_predictions, obs = test$Absenteeism.time.in.hours))

#Plot a graph for actual vs predicted values
plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="green")
lines(rf_predictions,col="red")
rf_model$importance

x<- c('PC57','PC44','PC42','PC40','PC39','PC34','PC31','PC32','PC26','PC29','PC27','PC28','PC21','PC19','PC16','PC14','PC11','PC7','PC5','Absenteeism.time.in.hours')

train2 <- train.data[x]
x<- c('PC57','PC44','PC42','PC40','PC39','PC34','PC31','PC32','PC26','PC29','PC27','PC28','PC21','PC19','PC16','PC14','PC11','PC7','PC5')
test2 <- test.data[x]
random_model2 <- randomForest(Absenteeism.time.in.hours ~., data= train2)
random_model2
absent_pred2 <- predict(random_model2, test2)
p <- table(absent_pred2, test$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100
Accuracy

print(postResample(pred = absent_pred2, obs = test$Absenteeism.time.in.hours))

plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="blue")
lines(absent_pred2,col="red")
title(main= "Random forest with feature selection")

```

# Linear regression 
```{r}


lr_model = lm(Absenteeism.time.in.hours ~ ., data = train.data)

model2.bw = step(lr_model, direction = 'backward')
absent_pred <- predict(model2.bw, test.data) 
p <- table(absent_pred, test$Absenteeism.time.in.hours)
Accuracy <- sum(diag(p))/sum(p)*100
Accuracy
AIC(model2.bw)
print(postResample(pred = absent_pred, obs = test$Absenteeism.time.in.hours))

plot(test$Absenteeism.time.in.hours,type="l",lty=2,col="blue")
lines(absent_pred,col="red")
title(main= "Linear Regression")
```


