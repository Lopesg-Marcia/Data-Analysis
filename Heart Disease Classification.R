install.packages("readxl")
library("readxl")

# Import dataset, define empty as na, strings Factors T strings have domain

heart = read.csv("HeartDisease.csv", na.strings = "", stringsAsFactors = T)
head(heart)
summary(heart)

# Rename collumns
colnames(heart) = c("Age","Sex", "Chest_Pain", "Resting_blood_pressure", "Cholestoral", "Blood_sugar", "ECG", "Max_heart_rate", "exercise_induced_angina","Oldpeak", "Slope", " Vessels_colored", "thalassemia", "Heart_Disease")
head(heart)

# Exploratory Analysis - Categorical data
counts = table(heart$Sex)
barplot(counts, main = "Sex", xlab = "Genre")

counts = table(heart$Chest_Pain)
barplot(counts, main = "Chest Pain", xlab = "Types")

counts = table(heart$Blood_sugar)
barplot(counts, main = "Blood Sugar", xlab = "Values")

# Exploratory Analysis - Numerical data
summary(heart$Age)
boxplot(heart$Age)
boxplot(heart$Age, outline = F)
hist(heart$Age)

summary(heart$Max_heart_rate)
boxplot(heart$Max_heart_rate)
boxplot(heart$Max_heart_rate, outline = F)
hist(heart$Max_heart_rate)

summary(heart$Resting_blood_pressure)
boxplot(heart$Resting_blood_pressure)
boxplot(heart$Resting_blood_pressure, outline = F)
hist(heart$Resting_blood_pressure)

summary(heart$Cholestoral)
boxplot(heart$Cholestoral)
boxplot(heart$Cholestoral, outline = F)
hist(heart$Cholestoral)

# verify NAs
heart [!complete.cases(heart),]

# Treat NAs - numerical
median(heart$Age, na.rm = T) # NAs should be removed to calculate median
heart[is.na(heart$Age),]$Age = median(heart$Age, na.rm = T)
heart [!complete.cases(heart),]

# Treat NAs - categorical
unique(heart$thalassemia) # check NAs
summary(heart$thalassemia) # verify the moda
heart[is.na(heart$thalassemia),]$thalassemia = "Fixed Defect" # moda

unique(heart$Blood_sugar) # check NAs
summary(heart$Blood_sugar) # verify the moda
heart[is.na(heart$Blood_sugar),]$Blood_sugar = "Lower than 120 mg/ml" # moda

# Treat wrong standard
heart[heart$thalassemia =="Normal",]$thalassemia = "No"
heart$thalassemia = factor(heart$thalassemia) # remove level empties
summary(heart$thalassemia) # check the replacement

# Treat duplicate data for unique values column
heart[duplicated(heart$ID),] # check the duplicated line, ex line 1002
heart = heart [-c(1002)] # remove the duplicated line

# Treat values outside domain numerical
heart[heart$Resting_blood_pressure < 40 | heart$Resting_blood_pressure > 300,]$Resting_blood_pressure # set domain
median(heart$Resting_blood_pressure)
heart[heart$Resting_blood_pressure < 40 | heart$Resting_blood_pressure > 300,]$Resting_blood_pressure = median(heart$Resting_blood_pressure)
heart[heart$Resting_blood_pressure < 40 | heart$Resting_blood_pressure > 300,]$Resting_blood_pressure  # check the replacement
hist(heart$Resting_blood_pressure)

# Treat values outside domain categorical
unique(heart$ECG)
summary(heart$ECG)
heart[!heart$ECG %in% c("ST-T wave abnormality","Normal","Left ventricular hypertrophy"),]$ECG = "ST-T wave abnormality"
heart$ECG = factor(heart$ECG)
summary(heart$ECG)

# Treat outliers
desv = sd(heart$Resting_blood_pressure, na.rm = T)
heart[heart$Resting_blood_pressure >= 2 * desv , ]$Resting_blood_pressure # verify values higher than 2x SD
boxplot(heart$Resting_blood_pressure) # another way verify outliers
out = boxplot(heart$Resting_blood_pressure)$out
out # verify the value of each outlier
boxplot(heart$Resting_blood_pressure, outline = F)
median(heart$Resting_blood_pressure)
heart[heart$Resting_blood_pressure >= 2 * desv , ]$Resting_blood_pressure = median(heart$Resting_blood_pressure) # Replace verify values higher than with the median

# Decision tree Classification - divide train and test
install.packages("rpart", dependencies = T)
library(rpart)
sample = sample(2,1000, replace = T, prob = c(0.8,0.2))
diseasetrain = heart[sample == 1,] 
diseasetest = heart[sample ==2,]

# Decision tree Classification - model
treeheart = rpart(Heart_Disease ~., data = diseasetrain, method = "class")
treeheart
plot(treeheart)
text(treeheart, use.n = T, all = T, cex = .8)

test = predict(treeheart, newdata = diseasetest)
head(test)

# Decision tree Classification - metrics
comparison = cbind(diseasetest, test)
comparison

comparison["Result"] = ifelse(comparison$`0` >= 0.5, 0, 1)
comparison

confusion_matrix = table(comparison$Heart_Disease,comparison$Result)
confusion_matrix

accuracy_test = sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy_test

true_pos = (confusion_matrix[4]/(confusion_matrix[2] + confusion_matrix[4]))
true_pos  

true_neg = (confusion_matrix[1]/(confusion_matrix[1] + confusion_matrix[3]))
true_neg

# Naive bayes Classification 
install.packages("e1071")
library("e1071")

heart$Heart_Disease = as.factor(heart$Heart_Disease)   #requisite for naive bayes

# Naive bayes Classification - train and test
samplenaive = sample(2,1000, replace = T, prob = c(0.7,0.3))
train_naive = heart[sample == 1,] 
test_naive = heart[sample ==2,]

# Naive bayes Classification - model
model_naive = naiveBayes(Heart_Disease ~., train_naive ) # ~. uses all variables as independent
model_naive

# Naive bayes Classification - metrics
comparison_naive = predict(model_naive, test_naive)
comparison_naive

conf_matrix_naive = table(test_naive$Heart_Disease,comparison_naive)
conf_matrix_naive

accuracy_test_naive = sum(diag(conf_matrix_naive)) / sum(conf_matrix_naive)
accuracy_test_naive

true_pos_naive = (conf_matrix_naive[4]/(conf_matrix_naive[2] + conf_matrix_naive[4]))
true_pos_naive  

true_neg_naive = (conf_matrix_naive[1]/(conf_matrix_naive[1] + conf_matrix_naive[3]))
true_neg_naive

cat("Accuracy for Decision tree is", accuracy_test,", the true positve is", true_pos,"and the true negative is" ,true_neg )

cat("Accuracy for Naive is", accuracy_test_naive,", the true positve is", true_pos_naive,"and the true negative is" ,true_neg_naive )


