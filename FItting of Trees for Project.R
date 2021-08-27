# Fitting Diabetes Classification Trees

library(tree)
diabetes = read.csv("diabetes.csv", dec =',')
diabetes1 = factor(ifelse(diabetes$diabetes == "No diabetes", "No Diabetes", "Diabetes"))
diabetes = data.frame(diabetes, diabetes1)
tree.diabetes = tree(diabetes1~.-patient_number-diabetes, diabetes)
summary(tree.diabetes) # Misclassification error rate is 3.59%
plot(tree.diabetes)
text(tree.diabetes,pretty=0)
tree.diabetes # glucose is most impactful variable

set.seed(2)
train = sample(1:nrow(diabetes), nrow(diabetes)/2)
diabetes.test = diabetes[-train,]
diabetes1.test = diabetes1[-train]
tree.diabetes = tree(diabetes1~.-patient_number-diabetes,diabetes,subset = train)
tree.pred = predict(tree.diabetes, diabetes.test, type = "class")
table(tree.pred,diabetes1.test)
(25+156)/195 # 92.82% Success Rate


# Cross Validation
set.seed(3)
cv.diabetes = cv.tree(tree.diabetes, FUN = prune.misclass)
names(cv.diabetes)
cv.diabetes
par(mfrow = c(1,2))
plot(cv.diabetes$size,cv.diabetes$dev, xlab = "Number of Nodes", ylab = "Deviation", main = "Nodes vs Deviation", type="b")
plot(cv.diabetes$k,cv.diabetes$dev, xlab = "K-Folds", ylab = "Deviation", main = "K-Folds vs Deviation", type="b")
prune.diabetes3 = prune.misclass(tree.diabetes, best = 3)
plot(prune.diabetes3)
text(prune.diabetes3, pretty = 0) # 3 was too simple!
tree.pred3 = predict(prune.diabetes3,diabetes.test,type="class")
table(tree.pred3,diabetes1.test)
(22+159)/195 # 92.82% Success Rate, Same accuracy as pre-pruning


prune.diabetes5 = prune.misclass(tree.diabetes, best = 5) # 5 includes more relevant variables and was only 1 deviation higher
plot(prune.diabetes5)
text(prune.diabetes5, pretty = 0)
tree.pred5 = predict(prune.diabetes5,diabetes.test,type="class")
table(tree.pred5,diabetes1.test)
(25+156)/195 # 92.82% Success Rate, Same accuracy as with 3 branches, and pre-pruning

prune.diabetes9 = prune.misclass(tree.diabetes, best = 9)
plot(prune.diabetes9)
text(prune.diabetes9, pretty = 0)
tree.pred9=predict(prune.diabetes9,diabetes.test,type="class")
table(tree.pred9,diabetes1.test) 
(24+156)/195 # 92.31% Success Rate, Decreased by 0.5%


