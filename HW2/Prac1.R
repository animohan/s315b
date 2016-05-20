library(gbm)
inspam=read.csv("Spam_Data.txt")
spname<-c ("make", "address", "all", "3d", "our", "over", "remove",
          "internet","order", "mail", "receive", "will",
          "people", "report", "addresses","free", "business",
          "email", "you", "credit", "your", "font","000","money",
          "hp", "hpl", "george", "650", "lab", "labs",
          "telnet", "857", "data", "415", "85", "technology", "1999",
          "parts","pm", "direct", "cs", "meeting", "original", "project",
          "re","edu", "table", "conference", ";", "(", "[", "!", "$", "#",
          "CAPAVE", "CAPMAX", "CAPTOT","type")
colnames(inspam)=spname

set.seed(1)
x=inspam[sample(nrow(inspam)),]

set.seed(1)
gbm0=gbm(type~.,data = x,interaction.depth = 4, shrinkage =0.05, n.trees=2500, cv.folds=5, distribution="bernoulli", verbose=F)
gbm0.predict=predict(gbm0,x,type="response",n.trees = 300)
trainresp=rep(0,length(gbm0.predict))
trainresp[gbm0.predict>=0.5]=1
table[trainresp, x$type]

#Misclassification rate 66+96/2722+1716


wghts=rep(1,length(gbm0.predict))
wghts[x$type==0]=5;
gbm1=gbm(type~.,data = x,interaction.depth = 4, shrinkage =0.05,weights=wghts, n.trees=2500,cv.folds=5, distribution="bernoulli", verbose=F)
gbm1.predict=predict(gbm1,x,type="response",n.trees = 300)
trainresp1=rep(0,length(gbm1.predict))
trainresp1[gbm1.predict>=0.5]=1
table(trainresp1, x$type)


library(gbm)
inpcal=read.csv("California_Data.txt")
calname=c("hval","inc","hage","#rooms","#bed","pop","occu","lat","long")
colnames(inpcal)=calname
inpcal=inpcal[sample(nrow(inpcal)),]

set.seed(1)
gbmcal0=gbm(hval~.,data=inpcal, interaction.depth = 4, shrinkage = 0.05, n.trees=2500, cv.folds=5, distribution = "gaussian", verbose=F)


