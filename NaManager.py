import pandas as pd
'''
data = pd.read_csv("D:\Program Files\R\Rworkspace\data.csv", sep = ",")
chooses1 = data["地域"]!="?"
chooses2 = data["所属模块"]!="?"
data = data[chooses1&chooses2]
data.replace("?", float('nan'), inplace = True)
data = data.loc[:,data.isna().sum()<50]
data.fillna(data.median(), inplace = True)
data
data.to_csv("D:\Program Files\R\Rworkspace\data2.csv",index=False)
'''

# 1.导入数据
data = pd.read_csv("./data.csv", na_values='?', header=0)

# print(data.iloc[:, 0:40].isnull().sum())
newdata = data.drop(['长期负债比率','长期债务与营运资金比率','长期资产与长期资金比率','资本化比率','每股净资产','清算价值比率', '每股经营活动产生的现金流量净额', '销售毛利率', '股东权益与固定资产比率', '固定资产净值率', '固定资产比重', '固定资产周转率'], axis=1)

# 2.处理缺失值
for header in newdata.iloc[:,2:59]:
    newdata[header].fillna(newdata[header].median(),inplace=True)

for header2 in newdata.iloc[:,59:]:
    newdata[header2].fillna(newdata[header2].mode()[0],inplace=True)



