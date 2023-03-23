###数据####
redWine = read.csv("/home/yangsanhe/桌面/winequality-red.csv", sep = ";" , stringsAsFactors = F)
ncol(redWine) #12列
nrow(redWine) #1599行
table(redWine$quality)

###多元线性回归###  
redWine.lm = lm(quality~., data = redWine[-(1:10),])
summary(redWine.lm)
  redWine.lm = step(redWine.lm)
  summary(redWine.lm)
options(digits = 0)
predict(redWine.lm, newdata = redWine[1:10,] ,level = 0.95, interval = "prediction")
redWine[1:10,]$quality
options(digits = 6)
redWine[1:10,1:11]

###逻辑回归###
redWine2 = redWine[,]
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

getSummary<-function(wineDataSet,glmList){
  sum = list()
  for (i in as.integer(names(table(wineDataSet$quality)))){
    sum[[i]] = summary(glmList[[i]])
  }
  return (sum)
}

redWine.glm = getGlm(redWine2)
sum = getSummary(redWine2,redWine.glm)
#sum[[8]]
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
  
glmPredictor(redWine.glm, redWine2[1:10,])
redWine[1:10,1:11]

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
redWine2 = redWine[,1:11]
redWine.kmeans = kmeans(redWine2, classAmount)
table(redWine$quality, redWine.kmeans$cluster)
redWine.kmeans.jc = getJC(redWine.kmeans$cluster, redWine$quality)
redWine.kmeans.dbi = clv.Davies.Bouldin(cls.scatt.data(redWine2, redWine.kmeans$cluster, dist="euclidean"), c("average"), c("aveToCent"))


##层次聚类
redWine.dist = dist(redWine2)
redWine.hclust = cutree(hclust(redWine.dist), k=classAmount)
table(redWine$quality, redWine.hclust)
redWine.hclust.jc = getJC(redWine.hclust, redWine$quality)
redWine.hclust.dbi = clv.Davies.Bouldin(cls.scatt.data(redWine2, redWine.hclust, dist="euclidean"), c("average"), c("aveToCent"))

###PCA
redWine.pca = princomp(redWine2, cor = T)
summary(redWine.pca)
jpeg(file="redWine-pca.jpg")
screeplot(redWine.pca, type = "lines")
dev.off()
selectAmount = 7  
tempRedWine = as.data.frame(round(predict(redWine.pca), 3))[,1:selectAmount]

##kmeans
redWine.pca.kmeans = kmeans(tempRedWine, classAmount)
table(redWine$quality, redWine.pca.kmeans$cluster)
redWine.pca.kmeans.jc = getJC(redWine.pca.kmeans$cluster, redWine$quality)
redWine.pca.kmeans.dbi = getDBI(tempRedWine, redWine.pca.kmeans$cluster)


##层次聚类
tempRedWine.dist = dist(tempRedWine)
redWine.pca.hclust = cutree(hclust(tempRedWine.dist), k=classAmount)
table(redWine$quality, redWine.pca.hclust)
redWine.pca.hclust.jc = getJC(redWine.pca.hclust, redWine$quality)
redWine.pca.hclust.dbi = clv.Davies.Bouldin(cls.scatt.data(tempRedWine, redWine.pca.hclust, dist="euclidean"), c("average"), c("aveToCent"))


#####划分训练集测试集#####
library(sampling)
redWine = redWine[order(redWine$quality),]
sizes=c()
for (i in 1:length(table(redWine$quality))){
  sizes=c(sizes, round(table(redWine$quality)[i]*0.7))
}
trainSample   = strata(redWine, stratanames = c("quality"), size = sizes, method = "srswor")
redWine_train = redWine[trainSample$ID_unit,]
redWine_test  = redWine[-trainSample$ID_unit,]
table(redWine_train$quality)
table(redWine_test$quality)
  
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
redWine.lda = lda(quality~., data = redWine_train)
redWine.lda
redWine.lda.pred = predict(redWine.lda, redWine_test[,1:11])$class
table(redWine.lda.pred, redWine_test$quality)
redWine.lda.CR = getCR(table(redWine.lda.pred, redWine_test$quality))

##应用主成分分析，重新进行fisher判别
cor(redWine_train[,1:11])
redWine.train.pca = princomp(redWine_train[,1:11], cor = T)
redWine.test.pca = princomp(redWine_test[,1:11], cor = T)
summary(redWine.train.pca)
summary(redWine.test.pca)
jpeg(file="redWine-train-pca.jpg")
screeplot(redWine.train.pca, type = "lines")
dev.off()
jpeg(file="redWine-test-pca.jpg")
screeplot(redWine.test.pca, type = "lines")
dev.off()
selectAmount = 8 
temp_redWine_train = as.data.frame(round(predict(redWine.train.pca), 3))[,1:selectAmount]
temp_redWine_test = as.data.frame(round(predict(redWine.test.pca), 3))[,1:selectAmount]
temp_redWine_train$quality = redWine_train$quality


redWine.pca.lda = lda(quality~., data = temp_redWine_train)
redWine.pca.lda
redWine.pca.lda.pred = predict(redWine.pca.lda, temp_redWine_test)$class
table(redWine.pca.lda.pred, redWine_test$quality)
redWine.pca.lda.CR = getCR(table(redWine.pca.lda.pred, redWine_test$quality))

#####分类#####
redWine_train$quality = as.factor(redWine_train$quality)
redWine_test$quality = as.factor(redWine_test$quality)

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
redWine.svm = svm(quality~., data = redWine_train, scale = T)  
redWine.svm.pred = predict(redWine.svm, redWine_test[,1:11], decision.values = T)
summary(redWine.svm)
table(redWine.svm.pred, redWine_test$quality)
getEffect(redWine.svm.pred, redWine_test$quality)

###神经网络###
library(nnet)
redWine.nn = nnet(quality~., data = redWine_train, decay = 0.001, size = 9, maxit = 1000)
redWine.nn.pred = predict(redWine.nn, redWine_test[,1:11], type = "class")
table(redWine.nn.pred, redWine_test$quality)
getEffect(redWine.nn.pred, redWine_test$quality)

###随机森林###
library(randomForest)
#redWine.rf = randomForest(quality~., data=redWine_train, keep.forest=FALSE, importance=TRUE, ntree=1000)
redWine.rf = randomForest(quality~., data=redWine_train, importance=TRUE, ntree=1000)
print(redWine.rf)
importance(redWine.rf, scale = T)
weight = importance(redWine.rf, scale = T, type=1)
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
#特征选择 "alcohol"  "sulphates"  "density"  "total.sulfur.dioxide"  "volatile.acidity"    
redWine.rf.pred = predict(redWine.rf, redWine_test[,1:11])
table (redWine.rf.pred, redWine_test$quality)
getEffect (redWine.rf.pred, redWine_test$quality)

###决策树###
library(rpart)
redWine.dt =rpart(quality~., data = redWine_train, method = "class")
print(redWine.dt)
printcp(redWine.dt)
redWine.dt = prune(redWine.dt, cp = redWine.dt$cptable[which.min(redWine.dt$cptable[,"xerror"]), "CP"])#剪枝

#特征选择  alcohol   density   sulphates    total.sulfur.dioxide    volatile.acidity 
redWine.dt.pred = predict(redWine.dt, redWine_test[,1:11], type="class")
table(redWine.dt.pred, redWine_test$quality)
getEffect(redWine.dt.pred, redWine_test$quality)

##########特征选择后优化###########
features = c(features, "quality")
redWine_train2 = redWine_train[, features]

###SVM
library(e1071)
redWine.svm = svm(quality~., data = redWine_train2, scale = T)  
redWine.svm.pred = predict(redWine.svm, redWine_test, decision.values = T)
summary(redWine.svm)
table(redWine.svm.pred, redWine_test$quality)
getEffect(redWine.svm.pred, redWine_test$quality)  

###神经网络
library(nnet)
redWine.nn = nnet(quality~., data = redWine_train2, decay = 0.001, size = 9, maxit = 1000)
redWine.nn.pred = predict(redWine.nn, redWine_test[,1:11], type = "class")
table(redWine.nn.pred, redWine_test$quality)
getEffect(redWine.nn.pred, redWine_test$quality)  



