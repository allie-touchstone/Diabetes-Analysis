# Bagging
library(randomForest)
diabetes = read.csv("diabetes.csv", dec =',')
diabetes1 = factor(ifelse(diabetes$diabetes == "No diabetes", "No Diabetes", "Diabetes"))
diabetes = data.frame(diabetes, diabetes1)
train = sample(1:nrow(diabetes), nrow(diabetes)/2)
diabetes.train = diabetes[train,]
diabetes.test = diabetes[-train,]
diabetes1.test = diabetes1[-train]


set.seed(1)
bag.diabetes = randomForest(diabetes1~.-patient_number-diabetes-glucose,data=diabetes,subset=train,mtry=13,importance=TRUE)
bag.diabetes # Error Rate is 18.46% (increase of 5.64% from with glucose)
(2+157)/195 # 81.54% Success Rate (decrease of 5.63% from with glucose)

# Random Forest
set.seed(1)
rf.diabetes = randomForest(diabetes1~.-patient_number-diabetes-glucose,data=diabetes,subset=train,mtry=7,importance=TRUE)
rf.diabetes
(3+157)/195 # 82.05% Success Rate (decrease of 7.18% from with glucose)
importance(rf.diabetes)
# BMI, Hip, Age are the most important variables without glucose

