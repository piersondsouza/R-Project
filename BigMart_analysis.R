# 1. Import Train & Test DataSet from BigMart Dataset folder

trainSet <- read.csv("Train_UWu5bXk.csv", stringsAsFactors = F)

testSet <- read.csv("Test_u94Q5KV.csv", stringsAsFactors = F)

View(trainSet)
View(testSet)

# 2. Check dimensions (number of row & columns) & Structure in dataset.

dim(trainSet)
nrow(trainSet)
dim(testSet)

str(trainSet)
str(testSet)

# 3. Find Missing Values in the dataset.

table(is.na(trainSet))
table(is.na(testSet))

# 4. Find Missing Values according to Columns.

colSums(is.na(trainSet))
colSums(is.na(testSet))

# 5. Find Summary of DataSet & Draw Conclusions from it.

summary(trainSet)
summary(testSet)

# 6. ScatterPlots
# A. Plot a ScatterPlot using ggplot for Item_Visibility vs Item_Outlet_Sales & draw conclusion from which products visibility is more sales.

#install.packages("ggplot2")
library(ggplot2)

ggplot()+
  geom_point(aes(x = trainSet$Item_Visibility, y = trainSet$Item_Outlet_Sales), colour = 'violet')+
  ggtitle('Visibility VS Outlet Sales')+
  xlab('Visibilty')+
  ylab('Outlet Sales')

visi_vs_sales <- recordPlot()

visi_vs_sales

# B. Plot a Barplot using ggplot for Outlet_Identifier vs Item_Outlet_Sales & Draw conclusion who has contributed to majority of sales.

library(scales)

ggbarplot <- ggplot(trainSet, aes(trainSet$Outlet_Identifier, trainSet$Item_Outlet_Sales))+ geom_bar(stat = 'identity', colour = 'blue')+
  scale_y_continuous(labels = comma)+ggtitle("Outlet_Identifier vs Item_Outlet_Sales")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Outlet Identifier",y="Item Outlet Sales")

ggbarplot

# C. Plot a Barplot using ggplot for Item_Type vs Item_Outlet_Sales also draw conclusion which items are sold more.

ggbarplot2 <- ggplot(trainSet, aes(trainSet$Item_Type, trainSet$Item_Outlet_Sales))+
  geom_bar(stat = 'identity', colour = 'brown')+ggtitle('Item_Type vs Item_Outlet_Sales')+scale_y_continuous(labels = comma)+
  labs(x = 'Item Type', y = 'Item Outlet Sales')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggbarplot2

# D. Plot a Boxplot using ggplot for Item_Type vs Item_Outlet_Sales also draw conclusion which items are sold more.

ggboxplot <- ggplot(trainSet, aes(trainSet$Item_Type, trainSet$Item_Outlet_Sales))+
  geom_boxplot()+ggtitle('Item_Type vs Item_Outlet_Sales')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = 'Item Type', y = 'Item Outlet Sales')

ggboxplot

# 7. Manipulating Dataset to make it consistent
# A. Add Item_Outlet_Sales Column to test dataset which is'nt available & assign integer 1. Also Combine Both Train + Test Datasets.

testSet$Item_Outlet_Sales <- 1
View(testSet)
big_mart <- rbind(trainSet, testSet)
dim(big_mart)

# B. Impute missing value in Item_Weight using median because it is highly robust to Outliers.

big_mart$Item_Weight[is.na(big_mart$Item_Weight)] <- median(big_mart$Item_Weight, na.rm = TRUE)

table(is.na(big_mart$Item_Weight))

# C. We saw item visibility has zero value also, which is practically not feasible. Impute median value where item_visibility 0.

big_mart$Item_Visibility <- ifelse(big_mart$Item_Visibility == 0, median(big_mart$Item_Visibility), big_mart$Item_Visibility)

print(big_mart$Item_Visibility)

# D. Rename level in Outlet_Size to since mis-matched levels in variables needs to be corrected.

big_mart$Outlet_Size <- as.factor(big_mart$Outlet_Size)
nlevels(big_mart$Outlet_Size)
summary(big_mart$Outlet_Size)
levels(big_mart$Outlet_Size)[1] <- "Other"
summary(big_mart$Outlet_Size)

# E. Rename levels of Item_Fat_Content since value are "LF" / "low fat", so make them consistent.

library(plyr)
library(dplyr)
big_mart$Item_Fat_Content <- as.factor(big_mart$Item_Fat_Content)
summary(big_mart$Item_Fat_Content)
big_mart$Item_Fat_Content <- revalue(big_mart_sales$Item_Fat_Content, c("LF" = "Low Fat", "reg" = "Regular", "low fat" = "Low Fat"))
summary(big_mart$Item_Fat_Content)
# F. Create a new column 2013 - Year ( For Prediction ).

big_mart$Year <- 2018 - big_mart$Outlet_Establishment_Year
View(big_mart)

# G. Drop variables not required in modelling i.e. Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year as they aren't needed for prediction.

big_mart <- select(big_mart, -c(Item_Identifier, Outlet_Establishment_Year, Outlet_Identifier))
View(big_mart_sales)
# H. Divide data set into Train and Test.
# new_train <- df[1:nrow(train),]

big_mart_train <- big_mart[1:nrow(trainSet),]
big_mart_test <- big_mart[-(1:nrow(trainSet)),]
new_test = big_mart_test
View(big_mart_train)
summary(big_mart_train)
# I. Perform a Regression testing on training dataset

regress <- lm(big_mart_train$Item_Outlet_Sales~., data = big_mart_train)

# J. Plot Summary and Predict sales for Testing Dataset.

y_predicted <- predict(regress, new_test)

summary(relation)
print(y_predicted)

