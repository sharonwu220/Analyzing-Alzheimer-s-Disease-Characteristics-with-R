# Libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("factoextra")
install.packages("tidyr")
install.packages("faraway")
library(faraway)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(gridExtra)

# loading data
Alz_data<-read.csv("/Users/User/Documents/Essex/curriculum/SpringMA335Modelling experimental/final_project/project data.csv")

# convert M/F into numeric values
Alz_data$M.F <- ifelse(Alz_data$M.F == "M", 1, 0)

# remove rows with Group = “Converted” and missing values
Alz_data <- Alz_data %>%
  filter(!is.na(Group) & Group != "Converted")

####### task1
summary(Alz_data)
# boxplots
ggplot(Alz_data, aes(x = Group, y = Age)) +
  geom_boxplot()

# histograms
ggplot(Alz_data, aes(x = Group, fill = Group)) +
  geom_bar()   # 使用geom_bar()绘制计数直方图

# scatterplots
ggplot(Alz_data, aes(x = nWBV, y = Age, color = Group)) +
  geom_point()

####### task2 clustering algorithms
# check value
str(Alz_data) 
# turn value into numerical
Alz_data$Group <- ifelse(Alz_data$Group == "Nondemented", 1, 0)
Alz_data$Age <- as.numeric(Alz_data$Age)
Alz_data$EDUC <- as.numeric(Alz_data$EDUC)
Alz_data$SES <- as.numeric(Alz_data$SES)
Alz_data$MMSE <- as.numeric(Alz_data$MMSE)
Alz_data$eTIV <- as.numeric(Alz_data$eTIV)
str(Alz_data)
Alz_data2 <- na.omit(Alz_data) #clean NA in dataset
head(Alz_data2)
data1 <- scale(Alz_data2) #standardise data

set.seed(123)
kmeans2 <- kmeans(data1, centers = 2, nstart = 20)
kmeans3 <- kmeans(data1, centers = 3, nstart = 20)
kmeans4 <- kmeans(data1, centers = 4, nstart = 20)
kmeans2
str(kmeans2)

fviz_cluster(kmeans2, data = data1)
fviz_cluster(kmeans3, data = data1)
fviz_cluster(kmeans4, data = data1)

f1 <- fviz_cluster(kmeans2, geom = "point", data = data1) + ggtitle("k = 2")
f2 <- fviz_cluster(kmeans3, geom = "point", data = data1) + ggtitle("k = 3")
f3 <- fviz_cluster(kmeans4, geom = "point", data = data1) + ggtitle("k = 4")
grid.arrange(f1, f2, f3, nrow = 2)

####### task3 logistic regression
attach(Alz_data2)#将数据集Alz_data2加载到当前的工作环境中，以便可以直接引用数据集中的变量。
names(Alz_data2)
summary(Alz_data2) #描述性统计摘要。它会为每个数值变量显示最小值、第一四分位数、中位数、均值、第三四分位数和最大值，以及非数值变量的频数和因子水平
dim(Alz_data2) #行数和列数
pairs(Alz_data2) #数值变量之间的散点图矩阵。它可以帮助你观察变量之间的相关性和分布。
cor(Alz_data2) #所有数值变量的相关系数矩阵。它可以帮助你了解变量之间的线性关系强度和方向。
plot(Alz_data2$Group) #绘制单变量图。它可以帮助你观察单个变量的分布情况。
plot(Alz_data2$M.F)
plot(Alz_data2$Age)
plot(Alz_data2$EDUC)
plot(Alz_data2$SES)
plot(Alz_data2$MMSE)
plot(Alz_data2$CDR)
plot(Alz_data2$eTIV)
plot(Alz_data2$nWBV)
plot(Alz_data2$ASF)
# logistic regression
glm_Alz <- glm(Group ~ M.F + Age + EDUC + MMSE + eTIV + nWBV, data = Alz_data2, family = binomial)
    #因變數:Group，自變數:M.F,Age,EDUC,SES,MMSE,CDR,eTIV,nWBV,ASF
summary(glm_Alz)

# test with train set #Making predictions
glm.probs <- predict(glm_Alz, type = "response") # 預測Pr(Group=Demented|其他變數)
glm.predicted <- rep(0, nrow(Alz_data2))
glm.predicted[glm.probs > 0.5]= 1 
table(glm.predicted, Alz_data2$Group) # 顯示預測結果與實際Group的交叉表
mean(glm.predicted == Alz_data2$Group) # 計算預測準確率


####### task4 Feature selection method

# 進行向前選擇
Alz_model <- lm(Group ~ 1, data = Alz_data2)# 建立初始模型
step1 <- step(Alz_model,scope=~M.F + Age + EDUC + MMSE + eTIV + nWBV, method = 'forward')
summary(step1)# 顯示選擇後的模型摘要統計資訊

#進行向後選擇
Alz_model2 <- lm(Group ~ 1, data = Alz_data2)# 建立初始模型
step2 <- step(Alz_model2,scope=~M.F + Age + EDUC + MMSE + eTIV + nWBV, method = 'backward')
summary(step2)# 顯示模型摘要統計資訊


