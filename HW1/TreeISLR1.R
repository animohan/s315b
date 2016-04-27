library(tree)
library(ISLR)
attach(Carseats)

#High if sales>8, else no
High=ifelse(Sales<=8,"No", "Yes")

#new data frame:
Carseats=data.frame(Carseats, High)

#decision tree. High=response; All variables except Sales
#as predictor variables
tree.carseats=tree(High~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats


#seperating into test and training sets
set.seed(2)
train=sample(1:nrow(Carseats),200)
Carseats.test=Carseats[-train,]
High.test=High[-train]

tree.carseats=tree(High~.-Sales,Carseats, subset=train)
tree.pred=predict(tree.carseats, Carseats.test, type="class")
table(tree.pred,High.test)

#cost complexity pruning
set.seed(3)
cv.carseats=cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev, type="b")
prune.carseats=prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

#prediction using 9 node tree
tree.pred=predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

#looking at 15node tree.
prune.carseat=prune.misclass(tree.carseats, best=15)
plot(prune.carseat)
text(prune.carseat)
tree.pred=predict(prune.carseat, Carseats.test, type="class")
table(tree.pred, High.test)


#Regression trees:
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston, subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev, type='b')


prune.boston=prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

yhat=predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
