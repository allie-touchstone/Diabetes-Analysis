# Fitting Diabetes Classification Trees w/o Glucose
library(tree)
diabetes = read.csv("diabetes.csv", dec =',')
diabetes1 = factor(ifelse(diabetes$diabetes == "No diabetes", "No Diabetes", "Diabetes"))
diabetes = data.frame(diabetes)
tree.diabetes = tree(diabetes1~.-patient_number-glucose, diabetes)
summary(tree.diabetes) # Misclassification error rate = 10.26%
plot(tree.diabetes)
text(tree.diabetes,pretty=0)
tree.diabetes

set.seed(2)
train = sample(1:nrow(diabetes), nrow(diabetes)/2)
diabetes.test = diabetes[-train,]
diabetes1.test = diabetes1[-train]
tree.diabetes = tree(diabetes1~.-patient_number-glucose,diabetes,subset = train)
tree.pred = predict(tree.diabetes, diabetes.test, type = "class")
table(tree.pred,diabetes1.test)
(8+138)/195 # 74.87% Accuracy (Decreased 17.95% in accuracy without Glucose)

# Cross Validation w/o Glucose
set.seed(3)
cv.diabetes = cv.tree(tree.diabetes, FUN = prune.misclass)
names(cv.diabetes)
cv.diabetes
par(mfrow = c(1,2))
plot(cv.diabetes$size,cv.diabetes$dev, xlab = "Number of Nodes", ylab = "Deviation", main = "Nodes vs Deviation", type="b")
plot(cv.diabetes$k,cv.diabetes$dev, xlab = "K-Folds", ylab = "Deviation", main = "K-Folds vs Deviation", type="b")
prune.diabetes6 = prune.misclass(tree.diabetes, best = 6) #Chose 6 even though 1 had smaller deviation since 1 there cannot be a tree with only 1 node
plot(prune.diabetes6)
text(prune.diabetes6, pretty = 0)
tree.pred6 = predict(prune.diabetes6,diabetes.test,type="class")
table(tree.pred6,diabetes1.test)
(1+153)/195 # Increased to 78.97% Accuracy

prune.diabetes13 = prune.misclass(tree.diabetes, best = 13)
plot(prune.diabetes13)
text(prune.diabetes13, pretty = 0)
tree.pred13=predict(prune.diabetes13,diabetes.test,type="class")
table(tree.pred13,diabetes1.test) 
(7+138)/195 # Decreased to 74.36%


