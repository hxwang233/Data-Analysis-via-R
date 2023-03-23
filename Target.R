# INPUT : data - data.frame 训练数据
# INPUT : res - vector :predicted result
# OUTPUT: Jccard
getDBI<-function(data, res){
  library("clv")
  intraclust = c("average")   #簇内  属于相同簇的所有样本之间的平均距离(平均直径)
  interclust = c("aveToCent") #簇间  两类中簇中心点的距离(平均质心联动)
  clv.Davies.Bouldin(cls.scatt.data((data), res, dist="euclidean"), intraclust, interclust)[1] #以欧氏距离为距离度量的 dbi指数
}

# INPUT : result - vector :predicted result
# INPUT : real - vector :real
# OUTPUT: Jccard
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

# INPUT : cm - table of confusion matrix
# OUTPUT: 准确率
getCR<-function(cm){
  res = as.matrix(cm)
  correctRate = sum(res[row(res) == col(res)]) / sum(res)
  return(correctRate)
}

#多分类评判指标 召回率 平均召回率 精度 平均精度 宏观F1 微观F1
# INPUT : pre - vector :predicted result
# INPUT : real - table of confusion matrix
# OUTPUT: Recall AVG(Recall) Precision AVG(Precision) macro-F1 micro-F1
getEffect = function(pre,real){
  types = sort(unique(real))
  tp = fp = fn = tn = NA
  for(i in 1:length(types)){
    tp[i] = sum(pre==types[i] & real==types[i])
    fp[i] = sum(pre==types[i] & real!=types[i])
    fn[i] = sum(pre!=types[i] & real==types[i])
    tn[i] = sum(pre!=types[i] & real!=types[i])
  }
  recall = tp / (tp + fn)
  precision = tp / (tp + fp)
  f1 = 2 * tp / (2 * tp + fp + fn)
  names(f1) = names(recall) = names(precision) = names(tp) = names(fp) = names(fn) = types
  print(paste("AVG(TP): ", round(mean(tp), digits = 4)))
  print(paste("AVG(FP): ", round(mean(fp), digits = 4)))
  print(paste("AVG(TN): ", round(mean(tn), digits = 4)))
  print(paste("AVG(FN): ", round(mean(fn), digits = 4)))
  #print("Recall:")
  #print(recall)
  print(paste("AVG(Recall): ", round(mean(recall, na.rm = T), digits = 4)))
  #print("Precision:")
  #print(precision)
  print(paste("AVG(Precision): ", round(mean(precision, na.rm = T), digits = 4)))
  print(paste("macro-F1: ", round(mean(f1), digits = 4)))
  print(paste("micro-F1: ",round(2 * sum(tp) / (2 * sum(tp) + sum(fp) + sum(fn)), digits = 4)))
}
