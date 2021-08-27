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
bag.diabetes = randomForest(diabetes1~.-patient_number-diabetes,data=diabetes,subset=train,mtry=14,importance=TRUE)
bag.diabetes # Error Rate is 12.82%
(16+154)/195 # 87.17% Success Rate

# Random Forest
set.seed(1)
rf.diabetes = randomForest(diabetes1~.-patient_number-diabetes,data=diabetes,subset=train,mtry=7,importance=TRUE)
rf.diabetes
(17+157)/195 # 89.23% Success Rate
importance(rf.diabetes)
# Glucose is the most important by a huge margin