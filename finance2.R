finance = read.csv("data3.csv", sep = "," , stringsAsFactors = F, fileEncoding = 'utf-8')
finance[1:5,]
table(finance$所属模块)
table(finance$地域)

###逻辑回归###
finance3 = finance[,(which(names(finance)=="净资产收益率加权")+1):ncol(finance)]
getTwoGlm<-function(financeDataSet){
  n = ncol(financeDataSet)
  glmList1 = list() #所属模块
  glmList2 = list() #地域
  for (i in names(table(financeDataSet$所属模块))){
    financeDataSet = transform(financeDataSet, temp = ifelse(financeDataSet$所属模块 == i, 1, 0))
    glmList1[[i]] = glm(temp~., data = financeDataSet[,c(1:(n-2),n+1)], family = binomial(link = "logit"), control = list(maxit = 100))
    glmList1[[i]] = step(glmList1[[i]])
    financeDataSet = transform(financeDataSet, temp = NULL)
  }
  for (i in names(table(financeDataSet$地域))){
    financeDataSet = transform(financeDataSet, temp = ifelse(financeDataSet$地域 == i, 1, 0))
    glmList2[[i]] = glm(temp~., data = financeDataSet[,c(1:(n-2),n+1)], family = binomial(link = "logit"), control = list(maxit = 100))
    glmList2[[i]] = step(glmList2[[i]])
    financeDataSet = transform(financeDataSet, temp = NULL)
  }
  glmList = list()
  glmList[["所属模块"]] = glmList1
  glmList[["地域"]] = glmList2
  return (glmList)
}
finance.glm2 = getTwoGlm(finance3) #训练模型

glmPredictor2<-function(glmList, newData){
  zero = rep(0,nrow(newData))
  glmList1 = glmList[["所属模块"]]
  res1 = data.frame(zero, check.names=FALSE)
  for (i in names(table(finance3$所属模块))){
    temp = as.vector(predict(glmList1[[i]], newData , type="response")) #在所属模块模型中，预测属于某模块概率
    res1 = transform(res1, tempcol = temp, check.names=FALSE)
    names(res1)[ncol(res1)] = i; #贴模块标签（列名）
  }
  glmList2 = glmList[["地域"]]
  res2 = data.frame(zero, check.names=FALSE)
  for (i in names(table(finance3$地域))){
    temp = as.vector(predict(glmList2[[i]], newData , type="response")) #在地域模型中，预测属于某地域概率
    res2 = transform(res2, tempcol = temp, check.names=FALSE)
    names(res2)[ncol(res2)] = i; #贴地域标签（列名）
  }
  v1 = apply(res1, 1, function(x){names(which.max(x))})  #在所属模块模型中，对行求最大，返回列名
  v2 = apply(res2, 1, function(x){names(which.max(x))})  #在地域模型中，对行求最大，返回列名
  paste(v2,v1,sep="-")  #组合模型拼接结果
}
glmPredictor2(finance.glm2, finance3[1:3,]) #预测
paste(finance3[1:3,]$地域, finance3[1:3,]$所属模块,sep = "-")

###聚类分析###
#install.packages("clv")
getDBI<-function(data, res){
  library("clv")
  intraclust = c("average")   #簇内  属于相同簇的所有样本之间的平均距离(平均直径)
  interclust = c("aveToCent") #簇间  两类中簇中心点的距离(平均质心联动)
  clv.Davies.Bouldin(cls.scatt.data((data), res, dist="euclidean"), intraclust, interclust)[1] #以欧氏距离为距离度量的 dbi指数
}

finance3 = finance[,(which(names(finance)=="净资产收益率加权")+1):ncol(finance)]
real = paste(finance3$地域, finance3$所属模块,sep = "-")  #真实的 模块-地域
finance3 = transform(finance3, tempcol = real, check.names=FALSE) 
classAmount = length(table(finance3$tempcol))  # 实际个股所在 板块-地域 数量(数据集预处理后)
classAmount
table(finance3$tempcol)
names(table(finance3$tempcol))[which(table(finance3$tempcol) < nrow(finance3)/classAmount)]  #样本数小于平均值的类别名
length(names(table(finance3$tempcol))[which(table(finance3$tempcol) < nrow(finance3)/classAmount)])   #样本数小于平均值的类别数
finance3 = transform(finance3, tempcol = NULL)


##kmeans
finance.kmeans = kmeans(finance3[,1:(ncol(finance3)-2)], classAmount)  #应用Kmeans，聚为98类
table(real, finance.kmeans$cluster)   #聚类结束后得到的混淆矩阵
finance.kmeans.jc = getJC(finance.kmeans$cluster, real)  #Jaccard系数
finance.kmeans.dbi = getDBI(finance3[,1:(ncol(finance3)-2)], finance.kmeans$cluster)  #DBI指数


##层次聚类
finance.dist = dist(finance3[,1:(ncol(finance3)-2)])
finance.hclust = cutree(hclust(finance.dist), k=classAmount)  #应用层次聚类，聚为98类
table(real, finance.hclust) #聚类结束后得到的混淆矩阵
finance.hclust.jc = getJC(finance.hclust, real)  #Jaccard系数
finance.hclust.dbi = getDBI(finance3[,1:(ncol(finance3)-2)], finance.hclust)  #DBI指数

###PCA
finance.pca = princomp(finance3[,1:(ncol(finance3)-2)], cor = T)
summary(finance.pca)
jpeg(file="finance-pca2.jpg")
screeplot(finance.pca, npcs = 40, type = "lines")
dev.off()

selectAmount = 18 
tempFinance = as.data.frame(round(predict(finance.pca), 3))[,1:selectAmount] #选择前18个主成分，并进行数据转化
tempFinance [1:5,]

##kmeans
finance.pca.kmeans = kmeans(tempFinance, classAmount) #主成分分析后重新进行kmeans
table(real, finance.pca.kmeans$cluster) #输出混淆矩阵
finance.pca.kmeans.jc = getJC(finance.pca.kmeans$cluster, real)       #Jaccard系数
finance.pca.kmeans.dbi = getDBI(tempFinance, finance.pca.kmeans$cluster)  #DBI指数

##层次聚类
finance.pca.dist = dist(tempFinance) 
finance.pca.hclust = cutree(hclust(finance.pca.dist), k=classAmount) #主成分分析后重新进行层次聚类
table(real, finance.pca.hclust) #输出混淆矩阵
finance.pca.hclust.jc = getJC(finance.pca.hclust, real)  #Jaccard系数
finance.pca.hclust.dbi = getDBI(tempFinance, finance.hclust)  #DBI指数

#####划分训练集测试集#####
library(sampling)
finance3 = finance[,(which(names(finance)=="净资产收益率加权")+1):ncol(finance)]
real = paste(finance3$地域, finance3$所属模块,sep = "-")
finance3 = transform(finance3, tempcol = real, check.names=FALSE)
finance3 = finance3[order(finance3$tempcol),] #按 地域-模块 排序
onlyOneRecord = c()
for (i in names(table(real))[table(real)==1]){  #获取所有仅有1个样本的类别 若样本数为2, 2*0.7=1.4, round(1.4)=1 训练集和测试集都会有该类型
  onlyOneRecord = c(onlyOneRecord, which(finance3$tempcol == i))  #获取该类别仅有的样本index
}
length(onlyOneRecord)
sizes = c()
for (i in 1:length(table(real))){
  sizes = c(sizes, round(table(real)[i]*0.7)) #分层抽样数目
}
trainSample   = strata(finance3, stratanames = c("tempcol"), size = sizes, method = "srswor") #不放回分层抽样
finance_train = finance3[trainSample$ID_unit,]  #训练集
finance_test  = finance3[-trainSample$ID_unit,] #测试集
finance_test = rbind(finance_test,finance3[onlyOneRecord,])  #测试集添加上述仅有1个样本的类别的样本 最终测试集

length(table(finance_train$tempcol))
length(table(finance_test$tempcol))

#####判别分析#####
library(MASS)
finance.lda1 = lda(所属模块~., data = finance_train[,1:(ncol(finance_test)-2)])  #模块的lda模型
finance.lda1.pred = predict(finance.lda1, finance_test[,1:(ncol(finance_test)-2),])$class  #模块预测值
finance.lda2 = lda(地域~., data = finance_train[,c(1:(ncol(finance_test)-3),ncol(finance_test)-1)]) #地域的lda模型
finance.lda2.pred = predict(finance.lda2, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test)-1)])$class #地域预测值
res = paste(finance.lda2.pred, finance.lda1.pred, sep="-")  #两个模型进行结果拼接
finance.lda.CR = getCR(table(res, finance_test$tempcol))  #组合模型的准确率

finance.lda = lda(tempcol~., data = finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train))]) #训练单个模型 地域-模块
options(digits = 6)
finance.lda
finance.lda.pred = predict(finance.lda, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test))])$class #模型预测值
finance.lda.CR = getCR(table(finance.lda.pred, finance_test$tempcol)) #模型准确率
# 0.007518797  分开做准确率
# 0.1804511 合并做准确率

##应用主成分分析，重新进行fisher判别
finance.train.pca = princomp(finance_train[,1:(ncol(finance_train)-3)],cor = T)  #训练集做主成分分析
summary(finance.train.pca)
jpeg(file="finance-train-pca.jpg")
screeplot(finance.train.pca, npcs = 40 ,type = "lines")
dev.off()
 
selectAmount = 16
temp_finance_train = as.data.frame(round(predict(finance.train.pca), 3))[,1:selectAmount]
finance.test.pca = princomp(finance_test[,1:(ncol(finance_test)-3)], cor = T)    #测试集做主成分分析且进行转换，维度按训练集PCA模型所定
temp_finance_test = as.data.frame(round(predict(finance.test.pca), 3))[,1:selectAmount]
temp_finance_train$real = finance_train$tempcol #为转化后的训练集数据贴标签
temp_finance_test$real = finance_test$tempcol   #为转化后的测试集数据贴标签
temp_finance_train[1:5,]
temp_finance_test[1:5,]
finance.pca.lda = lda(real~., data = temp_finance_train)  
finance.pca.lda.pred = predict(finance.pca.lda, temp_finance_test)$class
table(finance.pca.lda.pred, temp_finance_test$real) #输出混淆矩阵
finance.pca.lda.CR = getCR(table(finance.pca.lda.pred, temp_finance_test$real))

#####分类#####
finance_train$所属模块 = as.factor(finance_train$所属模块)
finance_train$地域 = as.factor(finance_train$地域)
finance_train$tempcol = as.factor(finance_train$tempcol)
finance_test$所属模块 = as.factor(finance_test$所属模块)
finance_test$地域 = as.factor(finance_test$地域)
finance_test$tempcol = as.factor(finance_test$tempcol)

###SVM###
#合并做#
library(e1071)
finance.svm = svm(tempcol~., data =finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train))], scale = T) #训练SVM模型 标签:地域-模块
finance.svm.pred = predict(finance.svm, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test))], decision.values = T) #预测结果
table(finance.svm.pred, finance_test$tempcol) #输出混淆矩阵
getEffect(finance.svm.pred, finance_test$tempcol) #评判指标 由于是多分类问题，在此转换为n个二分类问题后取平均值


#分开做#
finance.svm1 = svm(所属模块~., data =finance_train[,1:(ncol(finance_train)-2)], scale = T)  
finance.svm1.pred = predict(finance.svm1, finance_test[,1:(ncol(finance_test)-2)], decision.values = T)
finance.svm2 = svm(地域~., data =finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train)-1)], scale = T)  
finance.svm2.pred = predict(finance.svm2, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test)-1)], decision.values = T)
res = paste(finance.svm2.pred, finance.svm1.pred, sep="-")
table(res, finance_test$tempcol)
getEffect(res, finance_test$tempcol)


###神经网络###

#合并做#
library(nnet)
finance.nn = nnet(tempcol~., data=finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train))], decay=0.001, size=5, maxit=1000)#训练神经网络模型 标签:地域-模块
finance.nn.pred = predict(finance.nn, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test))], type = "class") #预测结果
table(finance.nn.pred, finance_test$tempcol) #输出混淆矩阵
getEffect(finance.nn.pred, finance_test$tempcol)

#分开做#
finance.nn1 = nnet(所属模块~., data = finance_train[,1:(ncol(finance_train)-2)], decay = 0.001, size = 5, maxit = 1000) #参数待调整
finance.nn1.pred = predict(finance.nn1, finance_test[,1:(ncol(finance_test)-2)], type = "class")
finance.nn2 = nnet(地域~., data = finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train)-1)], decay = 0.001, size = 5, maxit = 1000) #参数待调整
finance.nn2.pred = predict(finance.nn2, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test)-1)], type = "class")
res = paste(finance.nn2.pred, finance.nn1.pred, sep="-")
table(res, finance_test$tempcol)
getEffect(res, finance_test$tempcol)


###随机森林###
library(randomForest)
#合并做 +特征选择#
#redWine.rf = randomForest(tempcol~., data=finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train))], keep.forest=FALSE, importance=TRUE, ntree=1000)
finance.rf = randomForest(tempcol~., data=finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train))], importance=TRUE, ntree=1000)
finance.rf.pred = predict(finance.rf, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test))])
table (finance.rf.pred, finance_test$tempcol)
getEffect (finance.rf.pred, finance_test$tempcol)


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
#分开做#
finance.rf1 = randomForest(所属模块~., data=finance_train[,1:(ncol(finance_train)-2)], importance=TRUE, ntree=1000)
finance.rf1.pred = predict(finance.rf1, finance_test[,1:(ncol(finance_test)-2)])
finance.rf2 = randomForest(地域~., data=finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train)-1)], importance=TRUE, ntree=1000)
finance.rf2.pred = predict(finance.rf2, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test)-1)])
res = paste(finance.rf2.pred, finance.rf1.pred, sep="-")
table(res, finance_test$tempcol)
getEffect(res, finance_test$tempcol)

###决策树###
#合并做#
library(rpart)
finance.dt =rpart(tempcol~., data = finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train))], method = "class")
finance.dt = prune(finance.dt, cp = finance.dt$cptable[which.min(finance.dt$cptable[,"xerror"]), "CP"])#剪枝
finance.dt.pred = predict(finance.dt, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test))], type="class")
table(finance.dt.pred, finance_test$tempcol)
getEffect(finance.dt.pred, finance_test$tempcol)

#分开做#
finance.dt1 =rpart(所属模块~., data = finance_train[,1:(ncol(finance_train)-2)], method = "class")
print(finance.dt1)
printcp(finance.dt1)
finance.dt1 = prune(finance.dt1, cp = finance.dt1$cptable[which.min(finance.dt1$cptable[,"xerror"]), "CP"])#剪枝
finance.dt1.pred = predict(finance.dt1, finance_test[,1:(ncol(finance_test)-2)], type="class")
finance.dt2 =rpart(地域~., data = finance_train[,c(1:(ncol(finance_train)-3),ncol(finance_train)-1)], method = "class")
print(finance.dt2)
printcp(finance.dt2)
finance.dt2 = prune(finance.dt2, cp = finance.dt2$cptable[which.min(finance.dt2$cptable[,"xerror"]), "CP"])#剪枝
finance.dt2.pred = predict(finance.dt2, finance_test[,c(1:(ncol(finance_test)-3),ncol(finance_test)-1)], type="class")
res = paste(finance.dt2.pred, finance.dt1.pred, sep="-")
table(res, finance_test$tempcol)
getEffect(res, finance_test$tempcol)

##########特征选择后优化###########



