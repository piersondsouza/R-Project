#1. Import train.csv file from Titanic_dataset.
train_data = read.csv('C:/Users/MUM/Desktop/DAUR/Data_Science/R project/Titanic Dataset/train.csv', stringsAsFactors = F)
View(train_data)
str(train_data)
#2. Factors and Levels
#A. Find number of Passengers according to their Group Class: 1st , 2nd , 3rd
passengerInfo_class <- as.factor(train_data$Pclass)
print("Number of passengers in their respective classes: ")
summary(passengerInfo_class)

#B. Find number of Passengers according to their Group Sex: Male, Female.
passengerInfo_sex <- as.factor(train_data$Sex)
print('Passengers according to their group sex : ')
summary(passengerInfo_sex)

#c. Find stats of Passengers Age.
passengerInfo_age <- as.factor(train_data$Age)
summary(passengerInfo_age)
print(passengerInfo_age)

#D. Find number of Passengers according to their Group Embarked: Place where the passenger embarked their journey. One of Cherbourg, Queenstown or Southampton

passengerInfo_emb <- as.factor(train_data$Embarked)

summary(passengerInfo_emb)
# 3. Response Variables
# A. Validate number of passengers who survived / Not Survived

titanic_surv <- as.factor(train_data$Survived)

summary(titanic_surv)

# 4. Exploratory Data Analysis: A. Explore / Print first n Records from Dataset.

head(train_data)

# B. Find mean, median, quartile, max, min data for every feature.

summary(train_data)

# C. For the purposes of this study, we work with only four input variables and one response variable.
# Input variables : Passenger Class, Sex, Age, and Port of Embarkment.
# Response variable : Survived.

train_subset <- train_data[,c(2,3,5,6,12)]
View(train_subset)

# D. Perform data cleaning steps

library('dplyr')
table(is.na(train_data$survived))
table(is.na(train_data$Pclass))
table(is.na(train_data$Sex))
table(is.na(train_data$Embarked))
table(is.na(train_data$Age))

training_set <- na.omit(train_data)
class(training_set)
training_set <- training_set[!(training_set$Embarked == ""), ]
View(training_set)
nrow(training_set)
summary(training_set)


rownames(training_set) <- 1:nrow(training_set)

# E. Encode Data
# Make Age as a categorical variable as follows:
# If age <= 18, then age = child
# If 18 < age <= 60, then age = adult
# If age > 60, then age = senior

training_set$Age[training_set$Age <= 18] = "child" 

training_set$Age[(training_set$Age > 18) & (training_set$Age <= 60) & (training_set$Age != "child")] = "adult" 

training_set$Age[(training_set$Age != "child") & (training_set$Age != "adult")] = "senior" 

training_set$Age = as.factor(training_set$Age)

View(training_set)



# F. Validate above 2 steps.

head(training_set)


# 5. Data Analysis to perform
# A. Plot the barplot of all four input variables:

barplot(table(training_set$Pclass), xlab = "Class", ylab = "Frequency", col = "red", main = 'Barplot of class')

barplot(table(training_set$Sex), xlab = "Sex", ylab = "Frequency", col = "green", main = 'Barplot of Sex')

barplot(table(training_set$Age), xlab = "Age", ylab = "Frequency", col = "yellow", main = 'Barplot of Age')

barplot(table(training_set$Embarked), xlab = "Place embarked", ylab = "Frequency", col = "blue", main = 'Barplot of Embarkments')


# B. Convert the categorical dataframe into numeric dataframe.

# For Training Set
# Converting the female values to 0 and male values to 1

training_set$Sex[(training_set$Sex == "male")] = as.integer(1)
training_set$Sex[(training_set$Sex == "female") & (training_set$Sex != 1)] = as.integer(0)
summary(training_set)
# Converting the child to 0, adult to 1 and senior to 2
training_set$Age<-as.character(training_set$Age)
training_set$Age[training_set$Age == "child"] = as.integer(0)

training_set$Age[(training_set$Age == "adult") & (training_set$Age != 0)] = as.integer(1)

training_set$Age[(training_set$Age == "senior") & (training_set$Age != 0) & (training_set$Age != 1)] = as.integer(2)

# Converting the C values to 0, Q to 1 and S to 2

training_set$Embarked[training_set$Embarked == "C"] = as.integer(0)

training_set$Embarked[(training_set$Embarked == "Q") & (training_set$Embarked != 0)] = as.integer(1)

training_set$Embarked[(training_set$Embarked == "S") & (training_set$Embarked != 0) & (training_set$Embarked != 1)] = as.integer(2)


# 6. Statistical Analysis:
# A. Number of survivors on an average from Class & Plot a scatter plot

pclass_1 <- subset(training_set, training_set$Pclass == 1)
pclass_2 <- subset(training_set, training_set$Pclass == 2)
pclass_3 <- subset(training_set, training_set$Pclass == 3)


Class_var = c(0,0,0)
Class_var[1] = mean(pclass_1$Survived)
Class_var[2] = mean(pclass_2$Survived)
Class_var[3] = mean(pclass_3$Survived)
Class_var
plot(Class_var, type="o", xaxt = "n", main = 'Survival wrt class')
axis(1,at=c(1,2,3) , labels = c("1st","2nd","3rd"))


# B. Number of survivors on an average from Gender & Plot a scatter plot

female_training <- subset(training_set, training_set$Sex == 0)
male_training <- subset(training_set, training_set$Sex == 1)

Class_var1 = c(0,0,0)
Class_var1[1] = mean(female_training$Survived)
Class_var1[2] = mean(male_training$Survived)
Class_var1
plot(Class_var1, type="o", xaxt = "n", main = 'Survival wrt gender')
axis(1,at=c(1,2) , labels = c("female","male"))


# C. Number of survivors on an average from Every Port of Embarkment & Plot a scatter plot.

embark_C <- subset(training_set, training_set$Embarked == 0)
embark_Q <- subset(training_set, training_set$Embarked == 1)
embark_S <- subset(training_set, training_set$Embarked == 2)

Class_var2 = c(0,0,0)
Class_var2[1] = mean(embark_C$Survived)
Class_var2[2] = mean(embark_Q$Survived)
Class_var2[3] = mean(embark_S$Survived)

plot(Class_var2, type="o", xaxt = "n", main = 'Survival wrt to embarkment')
axis(1,at=c(1,2,3) , labels=c("Cherbourg", "Queenstown", "Southampton"))


# D. Validate above scatterplots using ANOVA ( 1 way Interaction )

dep1 = aov(training_set$Survived~training_set$Pclass)
anova(dep1)

dep2 = aov(training_set$Survived~training_set$Sex)
anova(dep2)

dep3 = aov(training_set$Survived~training_set$Embarked)
anova(dep3)

dep4 = aov(training_set$Survived~training_set$Pclass*training_set$Sex)
anova(dep4)

dep5 = aov(training_set$Survived~training_set$Sex*training_set$Embarked)
anova(dep5)

dep6 = aov(training_set$Survived~training_set$Pclass*training_set$Embarked)
anova(dep6)

dep7 = aov(training_set$Survived~training_set$Pclass*training_set$Age)
anova(dep7)

dep8 = aov(training_set$Survived~training_set$Sex*training_set$Age)
anova(dep8)

dep9 = aov(training_set$Survived~training_set$Age*training_set$Embarked)
anova(dep9)

dep10 = aov(training_set$Survived~training_set$Age)
anova(dep10)
