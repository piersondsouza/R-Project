#getting the current working directory
getwd()
#setting the working directory
setwd('C:/Users/MUM/Desktop/DAUR/Data_Science/R project/iris_Dataset')

#-------1.  Import iris.csv file from folder iris_dataset.-----------

#importing and reading the data file
iris_data <- read.csv('iris.csv')
#Viewing the dataset
View(iris_data)

#------2.  Exploratory Analysis. -------------

#A. Explore / Print first 3 Records from Dataset. 

first_3records<-head(iris_data,3)
print(first_3records)

# B. Find Dimension of  Dataset.

#The follwing dim() command will show the dimensions of the data set ie the number of rows and columns
print(dim(iris_data))
#We see that the dataSet has 150 rows and 5 columns

# C. Find Names , Class of features in the Dataset. 

#names() gives you the attributes of the dataSet
names(iris_data)
#We observe that, there are 5 features namely
# 1. sepal_length, 2. sepal_width, 3. petal_length, 4. peatl_width, 5. species
#class() gives you the class of the features of the dataSet
class(iris_data)
#We observe that it belongs to the class 'data.frame'

#D. Find missing values (if any) & make the data consistent by removing it

colSums(is.na(iris_data))
#We observe that there are no missing values present in the dataSet

#E. Find Structure of Data

str(iris_data)
#We observe the follwing
# The dataSet belongs to class 'data.frame' and has 150 obs./rows and 5 variables/columns
# 1. sepal_length of numerical datatype
#2. sepal_width of numerical datatype
#3. petal_length of numerical datatype
#4. petal_width of numerical datatype
#5. species is factors with 3 levels

#F. Find mean, median, quartile, max, min data for every feature.

summary(iris_data)
#Follwing is the observation of the dataSet with its mea,median,quantiles and levels
#sepal_length    sepal_width     petal_length    petal_width     species (3 levels) 
#Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100   setosa    :50  
#1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300   versicolor:50  
#Median :5.800   Median :3.000   Median :4.350   Median :1.300   virginica :50  
#Mean   :5.843   Mean   :3.054   Mean   :3.759   Mean   :1.199                  
#3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800                  
#Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500

#G. Plot a Boxplot Graph, Pie chart respective to their Species
boxplot(sepal_length ~ species, data = iris_data, main = 'Sepal length boxplot wrt Species', xlab='<--Species-->', ylab = 'sepal_length', col='red')
boxplot(sepal_width ~ species, data = iris_data,main = 'Sepal width boxplot wrt Species', xlab='<--Species-->', ylab = 'sepal_width', col='blue')
boxplot(petal_length ~ species, data = iris_data,main = 'Petal length boxplot wrt Species',xlab='<--Species-->', ylab = 'petal_length', col='green')
boxplot(petal_width ~ species, data = iris_data,main = 'Petal width boxplot wrt Species',xlab='<--Species-->', ylab='petal_width', col='violet')
pie(table(iris_data$species),col=rainbow(3), main = 'Pie chart of Different Species')

#H. Subset tuples based on their species
setosa_subset = subset(iris_data,iris_data$species=='setosa')
print(setosa_subset)
versicolor_subset = subset(iris_data,iris_data$species=='versicolor')
print(versicolor_subset)
virginica_subset = subset(iris_data,iris_data$species=='virginica')
print(virginica_subset)

#I. Plot a BoxPlot Graph for Individual R-Object.

boxplot(iris_data$sepal_length,main = 'Sepal length boxplot', xlab='Sepal', ylab = 'sepal_length', col='red')
boxplot(iris_data$sepal_width,main = 'Sepal width boxplot', xlab='Sepal', ylab = 'sepal_width', col='blue')
boxplot(iris_data$petal_length, main = 'Petal length boxplot', xlab='Petal', ylab = 'petal_length', col='green')
boxplot(iris_data$petal_width,main = 'Petal width boxplot', xlab='Petal', ylab = 'petal_width', col='violet')

#J. Plot a Histogram on feature Petal lengths of iris dataset .

hist(iris_data$petal_length,col = 'yellow')

#K. Plot a Histogram for Petal Lengths of Different Species on different Graph.
pie(table(iris_data$species),col = rainbow(3))
hist(setosa_subset$petal_length, col = 'red')
hist(virginica_subset$petal_length, col = 'blue')
hist(versicolor_subset$petal_length, col = 'green')

#L. Find correlation between multiple features also plot a scatter plot for correlation.
#install.packages('car')
library(car)
cor(iris_data$sepal_length,iris_data$sepal_width)
scatterplot(iris_data$sepal_width~iris_data$sepal_length, main = "Scatterplot ofSepal width vs Sepal length")
cor(iris_data$sepal_length,iris_data$petal_length)
scatterplot(iris_data$sepal_length~iris_data$petal_length, main = "scatterplot of Sepal length vs Petal length")
cor(iris_data$sepal_length,iris_data$petal_width)
scatterplot(iris_data$sepal_length~iris_data$petal_width, main = "Scatterplot of Sepal length vs Petal width")
cor(iris_data$sepal_width,iris_data$petal_length)
scatterplot(iris_data$sepal_width~iris_data$petal_length, main = "Scatterplot of Sepal width vs Petal length")
cor(iris_data$sepal_width,iris_data$petal_width)
scatterplot(iris_data$sepal_width~iris_data$petal_width, main = "Scatterplot of Sepal width vs Petal width")
cor(iris_data$petal_length,iris_data$petal_width)
scatterplot(iris_data$petal_width~iris_data$petal_length, main = "Scatterplot of Petal width vs Petal length")

# 4. Classify Data based on iris Species and plot a Decision Tree.

library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
#install.packages("rattle")
library(rattle)

classify1 <- rpart(iris_data$species ~ . , data = iris_data ,  method = 'class')

fancyRpartPlot(classify1 , main = ' Classification of Iris dataset' )
