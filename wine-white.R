###数据####
whiteWine = read.csv("/home/yangsanhe/桌面/winequality-white.csv", sep = ";" , stringsAsFactors = F)
ncol(whiteWine) #12列
nrow(whiteWine) #4898行
table(whiteWine$quality)

###多元线性回归###  
whiteWine.lm = lm(quality~., data = whiteWine[-(1:10),])
summary(whiteWine.lm)
whiteWine.lm = step(whiteWine.lm)
summary(whiteWine.lm)
options(digits = 0)
predict(whiteWine.lm, newdata = whiteWine[1:10,] ,level = 0.95, interval = "prediction")
whiteWine[1:10,]$quality
options(digits = 6)
whiteWine[1:10,1:11]
###逻辑回归###
whiteWine2 = whiteWine[,]
# INPUT : wineDataSet - data.frame of wine data set
# OUTPUT: glmList - list of glm's result
getGlm<-function(wineDataSet){
  glmList = list()
  for (i in as.integer(names(table(wineDataSet$quality)))){
    wineDataSet = transform(wineDataSet, temp = ifelse(wineDataSet$quality == i, 1, 0))
    glmList[[i]] = glm(temp~., data = wineDataSet[,c(1:11,13)], family = binomial(link = "logit"), control = list(maxit = 100))
    glmList[[i]] = step(glmList[[i]])
    wineDataSet = transform(wineDataSet, temp = NULL)
  }
  return (glmList)
}

whiteWine.glm = getGlm(whiteWine2)

getSummary<-function(wineDataSet,glmList){
  sum = list()
  for (i in as.integer(names(table(wineDataSet$quality)))){
    sum[[i]] = summary(glmList[[i]])
  }
  return (sum)
}

sum = getSummary(whiteWine2,whiteWine.glm)
#sum[[9]]
# INPUT : glmList - list of glm's result
# INPUT : newData - data.frame of new datas
# OUTPUT: vector of predicted result
glmPredictor<-function(glmList, newData){
  zero = rep(0,nrow(newData))
  res = data.frame(zero, check.names=FALSE)
  for (i in as.integer(names(table(redWine2$quality)))){
    temp = as.vector(predict(glmList[[i]], newData , type="response"))
    res = transform(res, tempcol = temp, check.names=FALSE)
    names(res)[ncol(res)] = i;
  }
  as.integer(apply(res, 1, function(x){names(which.max(x))}))
}
  
glmPredictor(whiteWine.glm, whiteWine2[1:10,])
whiteWine[1:10,1:11]

#######聚类#######
getJC<-function(result,real){
  a=b=c=d=0;
  for(i in 1:(length(result)-1)){
    for( j in i:length(result)){
      if(result[[i]]==result[[j]]&&real[[i]]==real[[j]]){
        a=a+1
      }else if(result[i]==result[[j]]&&real[[i]]!=real[[j]]){
        b=b+1
      }else if(result[i]!=result[[j]]&&real[[i]]==real[[j]]){
        c=c+1
      }else{
        d=d+1
      }
    }
  }
  return (a/(a+b+c))
}

##kmeans
library("clv")
classAmount = 10  
whiteWine2 = whiteWine[,1:11]
whiteWine.kmeans = kmeans(whiteWine2, classAmount)
table(whiteWine$quality, whiteWine.kmeans$cluster)
whiteWine.kmeans.jc = getJC(whiteWine.kmeans$cluster, whiteWine$quality)
whiteWine.kmeans.dbi = clv.Davies.Bouldin(cls.scatt.data(whiteWine2, whiteWine.kmeans$cluster, dist="euclidean"), c("average"), c("aveToCent"))


##层次聚类
whiteWine.dist = dist(whiteWine2)
whiteWine.hclust = cutree(hclust(whiteWine.dist), k=classAmount)
table(whiteWine$quality, whiteWine.hclust)
whiteWine.hclust.jc = getJC(whiteWine.hclust, whiteWine$quality)
whiteWine.hclust.dbi = clv.Davies.Bouldin(cls.scatt.data(whiteWine2, whiteWine.hclust, dist="euclidean"), c("average"), c("aveToCent"))

###PCA
whiteWine.pca = princomp(whiteWine2, cor = T)
summary(whiteWine.pca)
jpeg(file="whiteWine-pca.jpg")
screeplot(whiteWine.pca, type = "lines")
dev.off()
selectAmount = 8  
tempWhiteWine = as.data.frame(round(predict(whiteWine.pca), 3))[,1:selectAmount]

##kmeans
whiteWine.pca.kmeans = kmeans(tempWhiteWine, classAmount)
table(whiteWine$quality, whiteWine.pca.kmeans$cluster)
redWine.pca.kmeans.jc = getJC(whiteWine.pca.kmeans$cluster, whiteWine$quality)
whiteWine.pca.kmeans.dbi = getDBI(tempWhiteWine, whiteWine.pca.kmeans$cluster)


##层次聚类
tempWhiteWine.dist = dist(tempWhiteWine)
whiteWine.pca.hclust = cutree(hclust(tempWhiteWine.dist), k=classAmount)
table(whiteWine$quality, whiteWine.pca.hclust)
whiteWine.pca.hclust.jc = getJC(whiteWine.pca.hclust, whiteWine$quality)
whiteWine.pca.hclust.dbi = clv.Davies.Bouldin(cls.scatt.data(tempWhiteWine, whiteWine.pca.hclust, dist="euclidean"), c("average"), c("aveToCent"))


#####划分训练集测试集#####
library(sampling)
whiteWine = whiteWine[order(whiteWine$quality),]
sizes=c()
for (i in 1:length(table(whiteWine$quality))){
  sizes=c(sizes, round(table(whiteWine$quality)[i]*0.7))
}
trainSample   = strata(whiteWine, stratanames = c("quality"), size = sizes, method = "srswor")
whiteWine_train = whiteWine[trainSample$ID_unit,]
whiteWine_test  = whiteWine[-trainSample$ID_unit,]
table(whiteWine_train$quality)
table(whiteWine_test$quality)
  
#####判别分析#####
##fisher判别评判指标
# INPUT : cm - table of confusion matrix
# OUTPUT: correct rate
getCR<-function(cm){
  res = as.matrix(cm)
  correctRate = sum(res[row(res) == col(res)]) / sum(res)
  return(correctRate)
}

library(MASS)
whiteWine.lda = lda(quality~., data = whiteWine_train)
whiteWine.lda
whiteWine.lda.pred = predict(whiteWine.lda, whiteWine_test[,1:11])$class
table(whiteWine.lda.pred, whiteWine_test$quality)
whiteWine.lda.CR = getCR(table(whiteWine.lda.pred, whiteWine_test$quality))

##应用主成分分析，重新进行fisher判别
  cor(whiteWine_train[,1:11])
  whiteWine.train.pca = princomp(whiteWine_train[,1:11], cor = T)
  whiteWine.test.pca = princomp(whiteWine_test[,1:11], cor = T)
  summary(whiteWine.train.pca)
  summary(whiteWine.test.pca)
  jpeg(file="whiteWine-train-pca.jpg")
  screeplot(whiteWine.train.pca, type = "lines")
  dev.off()
  jpeg(file="whiteWine-test-pca.jpg")
  screeplot(whiteWine.test.pca, type = "lines")
  dev.off()
selectAmount = 8 
temp_whiteWine_train = as.data.frame(round(predict(whiteWine.train.pca), 3))[,1:selectAmount]
temp_whiteWine_test = as.data.frame(round(predict(whiteWine.test.pca), 3))[,1:selectAmount]
temp_whiteWine_train$quality = whiteWine_train$quality


whiteWine.pca.lda = lda(quality~., data = temp_whiteWine_train)
whiteWine.pca.lda
whiteWine.pca.lda.pred = predict(whiteWine.pca.lda, temp_whiteWine_test)$class
table(whiteWine.pca.lda.pred, whiteWine_test$quality)
whiteWine.pca.lda.CR = getCR(table(whiteWine.pca.lda.pred, whiteWine_test$quality))

#####分类#####
whiteWine_train$quality = as.factor(whiteWine_train$quality)
whiteWine_test$quality = as.factor(whiteWine_test$quality)

#多分类评判指标 召回率 平均召回率 精度 平均精度 宏观F1 微观F1
# INPUT : pre - vector :predicted result
# INPUT : real - table of confusion matrix
# OUTPUT: Recall AVG(Recall) Precision AVG(Precision) macro-F1 micro-F1
getEffect = function(pre,real){
  types = sort(unique(real))
  tp = fp = fn= tn = NA
  for(i in 1:length(types)){
    tp[i] = sum(pre==types[i] & real==types[i])
    fp[i] = sum(pre==types[i] & real!=types[i])
    fn[i] = sum(pre!=types[i] & real==types[i])
    tn[i] = sum(pre!=types[i] & real!=types[i])
  }
  recall = tp / (tp + fn)
  precision = tp / (tp + fp)
  f1 = 2 * tp / (2 * tp + fp + fn)
  names(f1) = names(recall) = names(precision) = names(tp) = names(fp) = names(tn) = names(fn) = types
  print("tp:")
  print(tp)
  print("fp:")
  print(fp)
  print("tn:")
  print(tn)
  print("fn:")
  print(fn)
  print("Recall:")
  print(recall)
  print(paste("AVG(Recall): ", mean(recall,na.rm = TRUE)))
  print("Precision:")
  print(precision)
  print(paste("AVG(Precision): ", mean(precision,na.rm = TRUE)))
  print(paste("macro-F1: ", mean(f1)))
  print(paste("micro-F1: ",2 * sum(tp) / (2 * sum(tp) + sum(fp) + sum(fn))))
}

###SVM###
library(e1071)
whiteWine.svm = svm(quality~., data = whiteWine_train, scale = T)  
whiteWine.svm.pred = predict(whiteWine.svm, whiteWine_test[,1:11], decision.values = T)
summary(whiteWine.svm)
table(whiteWine.svm.pred, whiteWine_test$quality)
getEffect(whiteWine.svm.pred, whiteWine_test$quality)

###神经网络###
library(nnet)
whiteWine.nn = nnet(quality~., data = whiteWine_train, decay = 0.001, size = 9, maxit = 1000)
whiteWine.nn.pred = predict(whiteWine.nn, whiteWine_test[,1:11], type = "class")
table(whiteWine.nn.pred, whiteWine_test$quality)
getEffect(whiteWine.nn.pred, whiteWine_test$quality)

###随机森林###
library(randomForest)
#whiteWine.rf = randomForest(quality~., data=whiteWine_train, keep.forest=FALSE, importance=TRUE, ntree=1000)
whiteWine.rf = randomForest(quality~., data=whiteWine_train, importance=TRUE, ntree=1000)
print(whiteWine.rf)
importance(whiteWine.rf, scale = T)
weight = importance(whiteWine.rf, scale = T, type=1)
selectFeature<-function(weight){
  m = colMeans(weight)
  fs = c()
  for (i in 1:length(weight)) {
    if(weight[i,1] >= m){
      fs = c(as.vector(row.names(weight)[i]),fs)
    }
  }
  return (fs)
}
features = selectFeature (weight)
print (features)
#特征选择 "alcohol"  "pH"  "free.sulfur.dioxide"  "volatile.acidity"    
whiteWine.rf.pred = predict(whiteWine.rf, whiteWine_test[,1:11])
table (whiteWine.rf.pred, whiteWine_test$quality)
getEffect (whiteWine.rf.pred, whiteWine_test$quality)

###决策树###
library(rpart)
whiteWine.dt =rpart(quality~., data = whiteWine_train, method = "class")
print(whiteWine.dt)
printcp(whiteWine.dt)
whiteWine.dt = prune(whiteWine.dt, cp = whiteWine.dt$cptable[which.min(whiteWine.dt$cptable[,"xerror"]), "CP"])#剪枝

#特征选择  alcohol   density   sulphates    total.sulfur.dioxide    volatile.acidity 
whiteWine.dt.pred = predict(whiteWine.dt, whiteWine_test[,1:11], type="class")
table(whiteWine.dt.pred, whiteWine_test$quality)
getEffect(whiteWine.dt.pred, whiteWine_test$quality)

##########特征选择后优化###########
features = c(features, "quality")
whiteWine_train2 = whiteWine_train[, features]

###SVM
library(e1071)
whiteWine.svm = svm(quality~., data = whiteWine_train2, scale = T)  
whiteWine.svm.pred = predict(whiteWine.svm, whiteWine_test, decision.values = T)
summary(whiteWine.svm)
table(whiteWine.svm.pred, whiteWine_test$quality)
getEffect(whiteWine.svm.pred, whiteWine_test$quality)  

###神经网络
library(nnet)
whiteWine.nn = nnet(quality~., data = whiteWine_train2, decay = 0.001, size = 9, maxit = 1000)
whiteWine.nn.pred = predict(whiteWine.nn, whiteWine_test[,1:11], type = "class")
table(whiteWine.nn.pred, whiteWine_test$quality)
getEffect(whiteWine.nn.pred, whiteWine_test$quality)  

###随机森林###
library(randomForest)
whiteWine.rf = randomForest(quality~., data=whiteWine_train2, importance=TRUE, ntree=1000)
#特征选择    
whiteWine.rf.pred = predict(whiteWine.rf, whiteWine_test[,1:11])
table (whiteWine.rf.pred, whiteWine_test$quality)
getEffect (whiteWine.rf.pred, whiteWine_test$quality)

###决策树###
library(rpart)
whiteWine.dt =rpart(quality~., data = whiteWine_train2, method = "class")
whiteWine.dt = prune(whiteWine.dt, cp = whiteWine.dt$cptable[which.min(whiteWine.dt$cptable[,"xerror"]), "CP"])#剪枝
#特征选择  
whiteWine.dt.pred = predict(whiteWine.dt, whiteWine_test[,1:11], type="class")
table(whiteWine.dt.pred, whiteWine_test$quality)
getEffect(whiteWine.dt.pred, whiteWine_test$quality)

