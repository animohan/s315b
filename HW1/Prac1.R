#Trees Practice
#HW1 requires using RPart; so load the RPart library
#Contains a dataset called stagec
#Stagec is a dataset of 146 patients with stage C prostate cancer

library(rpart)
names(stagec)
#Running the tree as part of rpart
fit=rpart(pgstat~age+eet+g2+grade+gleason+ploidy,stagec,control=rpart.control(cp=0.025))
plot(fit)
text(fit)

# In above example,results treat pgstat as numeric; Putting it as a factor
factor_pgstat=ifelse(stagec$pgstat==0,"No", "Yes")
fit=rpart(factor_pgstat~age+eet+g2+grade+gleason+ploidy,stagec,control=rpart(cp=0.025))

#Trying out various options with tree
plot(fit)
text(fit)

#Another way
progstat=factor(stagec$pgstat, levels=0:1, labels=c("No","Prog"))
cfit=rpart(progstat~age+eet+g2+grade+gleason+ploidy, data=stagec, method='class')
cfit
plot(cfit)
text(cfit)

print(cfit)
fitc2=prune(cfit, cp=0.03)
