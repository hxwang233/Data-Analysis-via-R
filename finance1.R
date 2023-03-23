finance = read.csv("data3.csv", sep = "," , stringsAsFactors = F, fileEncoding = 'utf-8')
finance[1:5,]
table(finance$所属模块)
table(finance$地域)
#which(names(finance)=="净资产收益率加权")
#which(names(finance)=="所属模块")

###多元线性回归###
finance1 = finance[,(which(names(finance)=="净资产收益率加权")):(which(names(finance)=="所属模块")-1)]
finance.lm = lm(净资产收益率加权~., data = finance1[-(1:10),])
summary(finance.lm)
finance.lm = step(finance.lm)
predict(finance.lm, newdata = finance[1:10,] ,level = 0.95, interval = "prediction")
finance[1:10,]$净资产收益率加权


###典型相关分析###
finance2 = finance[,3:(which(names(finance)=="所属模块")-1)]
#names(finance2)
c1 = which(names(finance2)=="净资产收益率加权")
c2 = ncol(finance2)
finance2 = scale(finance2)  #标准化
finance.ca = cancor(finance2[,(c1+1):c2],finance2[,1:c1])
finance.ca

###逻辑回归###
finance3 = finance[,(which(names(finance)=="净资产收益率加权")+1):(which(names(finance)=="所属模块"))]
#ncol(finance3)
getGlm<-function(financeDataSet){
  n = ncol(financeDataSet)
  glmList = list()
  for (i in names(table(financeDataSet$所属模块))){
    financeDataSet = transform(financeDataSet, temp = ifelse(financeDataSet$所属模块 == i, 1, 0))
    glmList[[i]] = glm(temp~., data = financeDataSet[,c(1:n-1,n+1)], family = binomial(link = "logit"), control = list(maxit = 100))
    glmList[[i]] = step(glmList[[i]])
    financeDataSet = transform(financeDataSet, temp = NULL)
  }
  return (glmList)
}
finance.glm = getGlm(finance3)

glmPredictor<-function(glmList, newData){
  zero = rep(0,nrow(newData))
  res = data.frame(zero, check.names=FALSE)
  for (i in names(table(finance3$所属模块))){
    temp = as.vector(predict(glmList[[i]], newData , type="response")) #在所属模块模型中，预测属于某模块概率
    res = transform(res, tempcol = temp, check.names=FALSE)
    names(res)[ncol(res)] = i;  #贴模块标签（列名）
  }
  apply(res, 1, function(x){names(which.max(x))}) #在所属模块模型中，对行求最大，返回列名
}
glmPredictor(finance.glm, finance3[1:10,]) #预测
finance3[1:10,]$所属模块

###聚类分析###
#install.packages("clv")
library("clv")
##kmeans
classAmount = 4
finance3 = finance[,(which(names(finance)=="净资产收益率加权")+1):(which(names(finance)=="所属模块"))]
length(names(finance3))
finance.kmeans = kmeans(finance3[,1:length(names(finance3))-1], classAmount)
table(finance3$所属模块, finance.kmeans$cluster)
finance.kmeans.jc = getJC(finance.kmeans$cluster, finance3$所属模块)
#finance.kmeans.dbi = getDBI(finance3[,1:length(names(finance3))-1], finance.kmeans$cluster)
finance.kmeans.dbi = clv.Davies.Bouldin(cls.scatt.data(finance3[,1:length(names(finance3))-1],  finance.kmeans$cluster, dist="euclidean"), c("average"), c("aveToCent"))


##层次聚类
finance.dist = dist(finance3[,1:length(names(finance3))-1])
finance.hclust = cutree(hclust(finance.dist), k=classAmount)
table(finance3$所属模块, finance.hclust)
finance.hclust.jc = getJC(finance.hclust, finance3$所属模块)
#finance.hclust.dbi = getDBI(finance3[,1:length(names(finance3))-1], finance.hclust)
finance.hclust.dbi = clv.Davies.Bouldin(cls.scatt.data(finance3[,1:length(names(finance3))-1], finance.hclust, dist="euclidean"), c("average"), c("aveToCent"))

###PCA
cor(finance3[,1:length(names(finance3))-1])
finance.pca = princomp(finance3[,1:length(names(finance3))-1], cor = T)
summary(finance.pca)
options(digits = 6)
jpeg(file="finance-pca.jpg")
screeplot(finance.pca, type = "lines")
dev.off()
#selectAmount = length(summary(finance.pca)[[1]])  
selectAmount = 23
tempFinance = as.data.frame(round(predict(finance.pca), 3))[,1:selectAmount]

##kmeans
classAmount = 4
finance.pca.kmeans = kmeans(tempFinance, classAmount)
table(finance3$所属模块, finance.pca.kmeans$cluster)
finance.pca.kmeans.jc = getJC(finance.pca.kmeans$cluster, finance3$所属模块)
#finance.pca.kmeans.dbi = getDBI(tempFinance, finance.pca.kmeans$cluster)
finance.pca.kmeans.dbi = clv.Davies.Bouldin(cls.scatt.data(tempFinance, finance.pca.kmeans$cluster, dist="euclidean"), c("average"), c("aveToCent"))


##层次聚类
classAmount = 4
finance.pca.dist = dist(tempFinance)
finance.pca.hclust = cutree(hclust(finance.pca.dist), k=classAmount)
table(finance3$所属模块, finance.pca.hclust)
finance.pca.hclust.jc = getJC(finance.pca.hclust, finance3$所属模块)
#finance.pca.hclust.dbi = getDBI(tempFinance, finance.pca.hclust)
finance.pca.hclust.dbi = clv.Davies.Bouldin(cls.scatt.data(tempFinance, finance.pca.hclust, dist="euclidean"), c("average"), c("aveToCent"))

#####划分训练集测试集#####
library(sampling)
finance3 = finance3[order(finance3$所属模块),]
sizes=c()
for (i in 1:length(table(finance3$所属模块))){
  sizes=c(sizes, round(table(finance3$所属模块)[i]*0.7))
}
trainSample   = strata(finance3, stratanames = c("所属模块"), size = sizes, method = "srswor")
finance_train = finance3[trainSample$ID_unit,]
finance_test  = finance3[-trainSample$ID_unit,]
nrow(finance_train)
ncol(finance_test)
table(finance_train$所属模块)
table(finance_test$所属模块)

#####判别分析#####
library(MASS)
finance.lda = lda(所属模块~., data = finance_train)
finance.lda
finance.lda.pred = predict(finance.lda, finance_test[,1:length(names(finance_test))-1])$class
table(finance.lda.pred, finance_test$所属模块)
finance.lda.CR = getCR(table(finance.lda.pred, finance_test$所属模块))

##应用主成分分析，重新进行fisher判别
finance.train.pca = princomp(finance_train[,1:length(names(finance_train))-1],cor = T)
finance.test.pca = princomp(finance_test[,1:length(names(finance_test))-1], cor = T)
summary(finance.train.pca)
summary(finance.test.pca)
jpeg(file="finance-train-pca.jpg")
screeplot(finance.train.pca, type = "lines")
dev.off()
jpeg(file="finance-test-pca.jpg")
screeplot(finance.test.pca, type = "lines")
dev.off()
#selectAmount = length(summary(finance.train.pca)[[1]]) 
selectAmount = 15
temp_finance_train = as.data.frame(round(predict(finance.train.pca), 3))[,1:selectAmount]
temp_finance_test = as.data.frame(round(predict(finance.test.pca), 3))[,1:selectAmount]
temp_finance_train$所属模块 = finance_train$所属模块
nrow(temp_finance_test)
nrow(temp_finance_train)

finance.pca.lda = lda(所属模块~., data = temp_finance_train)
finance.pca.lda
finance.pca.lda.pred = predict(finance.pca.lda, temp_finance_test)$class
table(finance.pca.lda.pred, finance_test$所属模块)
finance.pca.lda.CR = getCR(table(finance.pca.lda.pred, finance_test$所属模块))

#####分类#####
finance_train$所属模块 = as.factor(finance_train$所属模块)
finance_test$所属模块 = as.factor(finance_test$所属模块)

###SVM###
library(e1071)
finance.svm = svm(所属模块~., data =finance_train, scale = T)  
finance.svm.pred = predict(finance.svm, finance_test[,1:length(names(finance_test))-1], decision.values = T)
summary(finance.svm)
table(finance.svm.pred, finance_test$所属模块)
getEffect(finance.svm.pred, finance_test$所属模块)

###神经网络###
library(nnet)
finance.nn = nnet(所属模块~., data = finance_train, decay = 0.001, size = 9, maxit = 1000) #参数待调整
finance.nn.pred = predict(finance.nn, finance_test[,1:length(names(finance_test))-1], type = "class")
table(finance.nn.pred, finance_test$所属模块)
getEffect(finance.nn.pred, finance_test$所属模块)

###随机森林###
library(randomForest)
#redWine.rf = randomForest(所属模块~., data=finance_train, keep.forest=FALSE, importance=TRUE, ntree=1000)
finance.rf = randomForest(所属模块~., data=finance_train, importance=TRUE, ntree=1000)
print(finance.rf)
importance(finance.rf, scale = T)
weight = importance(finance.rf, scale = T, type=1)
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
#特征选择
# "经营现金净流量对销售收入比率" "流动资产周转天数"             "流动资产周转率"               "总资产周转天数"              
# "存货周转天数"                 "总资产周转率"                 "存货周转率"                   "应收账款周转天数"            
# "应收账款周转率"               "资本固定化比率"               "速动比率"                     "流动比率"                    
# "主营利润比重"                 "三项费用比重"                 "主营业务成本率"               "营业利润率"                  
# "主营业务利润率"  
finance.rf.pred = predict(finance.rf, finance_test[,1:length(names(finance_test))-1])
table (finance.rf.pred, finance_test$所属模块)
getEffect (finance.rf.pred, finance_test$所属模块)

###决策树###
library(rpart)
finance.dt =rpart(所属模块~., data = finance_train, method = "class")
print(finance.dt)
printcp(finance.dt)
finance.dt = prune(finance.dt, cp = finance.dt$cptable[which.min(finance.dt$cptable[,"xerror"]), "CP"])#剪枝
finance.dt.pred = predict(finance.dt, finance_test[,1:length(names(finance_test))-1], type="class")
table(finance.dt.pred, finance_test$所属模块)
getEffect(finance.dt.pred, finance_test$所属模块)

##########特征选择后优化###########
features = c(features, "所属模块")
finance_train2 = finance_train[, features]
ncol(finance_train2)
names(finance_train2)
ncol(finance_train)
table(finance_train2$所属模块)

###SVM
library(e1071)
finance.svm = svm(所属模块~., data =finance_train2, scale = T)  
finance.svm.pred = predict(finance.svm, finance_test[,features], decision.values = T)
summary(finance.svm)
table(finance.svm.pred, finance_test$所属模块)
getEffect(finance.svm.pred, finance_test$所属模块)

###神经网络
library(nnet)
finance.nn = nnet(所属模块~., data = finance_train2, decay = 0.001, size = 9, maxit = 1000) #参数待调整
finance.nn.pred = predict(finance.nn, finance_test[,features], type = "class")
table(finance.nn.pred, finance_test$所属模块)
getEffect(finance.nn.pred, finance_test$所属模块)