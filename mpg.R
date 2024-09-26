## car's mpg--------------------------------------------------------------------

## 1. 数据探索------------------------------------------------------------------

# 1.1 读取数据
# 设置工作目录
setwd('/Users/zouyuan/RStudyZouYuan')

#导入数据文件
data <- read.csv('./汽车油耗mpg估算/auto.mpg.csv',stringsAsFactors = F)

# 1.2 查看数据整体信息
View(data) #查看数据集的整体信息

# 1.3 缺失值探索
all <- data
head(all) # 查看数据整体信息

#查看是否存在缺失值
anyNA(all) # 检测数据集是否存在缺失值
which(is.na(all),arr.ind = T) # 查看缺失值位置，arr.ind = T可以返回缺失值的对应行列坐标

#用众数填补horsepower变量
# 求取众数
tmp <- table(all$horsepower) #计算出horsepower列中每个值出现的次数
index <- which.max(tmp) # 找出最多次数的索引
tmp[index] # 输出对应的数据及次数
all$horsepower[is.na(all$horsepower)] <- all[150,4] # 用众数填补缺失值
anyNA(all) # 检测数据集是否存在缺失值
all$horsepower <- as.integer(all$horsepower)#转换数据类型
all_mpg <- all$mpg

# 1.4数据可视化

# 绘制条形图
counts <- table(all$cylinders) # 将mtcars的挡数制成一个频数统计表
counts # 查看counts的信息
barplot(counts, main="Car Cylinders",xlab="Number of Cylinders")

#随机选取20条数据绘制点图
all_sample <- all[sample(nrow(all),20), ]

#对排量和mpg绘制点图
# 提取mpg和displacement列
mpg_displacement <- all_sample[, c("mpg", "displacement")]
# 按照displacement进行排序
mpg_displacement <- mpg_displacement[order(mpg_displacement$displacement), ]
# 绘制点图
dotchart(mpg_displacement$mpg, labels = mpg_displacement$displacement,
         cex = 0.7, main = "Gas Mileage for Car displacement", xlab = "Miles Per Gallon")

#对气缸数和mpg绘制点图
# 提取mpg和cylinders列
mpg_cylinders <- all_sample[, c("mpg", "cylinders")]
# 按照displacement进行排序
mpg_cylinders <- mpg_cylinders[order(mpg_cylinders$cylinders), ]
# 绘制点图
dotchart(mpg_cylinders$mpg, labels = mpg_cylinders$cylinders,
         cex = 0.7, main = "Gas Mileage for Car cylinders", xlab = "Miles Per Gallon")

## 2. 相关性分析----------------------------------------------------------------

states <- all[,1:5]
cor(states) #计算Pearson积差相关系数
cor(states,method = "spearman") #计算Spearman等级相关系数

## 3. 异常值探索----------------------------------------------------------------

par(mfrow = c(2,2))
plot(all[1:398,2],all_mpg,xlab='cylinders')
plot(all[1:398,3],all_mpg,xlab='displacement')
plot(all[1:398,4],all_mpg,xlab='horsepower')
plot(all[1:398,5],all_mpg,xlab='weight')

## 4. 描述性分析----------------------------------------------------------------

# mpg
ggplot(data.frame(Train_mpg),aes(Train_mpg,color=I("white"),fill=I("darkred")))+geom_histogram(bins=20)+theme(panel.background = element_blank(),axis.line = element_line())

## 5. 数据集构造----------------------------------------------------------------

#划分训练集和测试集
set.seed(1)
train <- sample(1:nrow(all),nrow(all)*3/4) #取75%做训练集
Train <- all[train,]
Test <- all[-train,]

Test = Test[1:6] #构造测试集
Train = Train[1:6] #构造训练集

Train_mpg <- Train$mpg # 把mpg从train数据集中拆分出来

## 6. 实证分析------------------------------------------------------------------

#基于随机森林模型的mpg预测模型构建
set.seed(1234)
# 验证性训练模型
rf_ntree<-randomForest(x=all,y=all_mpg,ntree=800,importance=TRUE,proximity=TRUE)
plot(rf_ntree)

# 正式训练模型
rf_ntree<-randomForest(x=Train,y=Train_mpg,ntree=500,importance=TRUE,proximity=TRUE)
rf_ntree



