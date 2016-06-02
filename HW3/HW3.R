library(nnet)

inspam=read.csv("Spam_Train.txt")
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
x=inspam
colnames(x)=spname
x$type=as.factor(inspam$type)
x[,1:57]=scale(x[,1:57], center=TRUE, scale=TRUE)
x=data.frame(x)
colnames(x)=spname


inspamtest=read.csv("Spam.Test.txt")
colnames(inspamtest)=spname
w=inspamtest
colnames(w)=spname
w$type=as.factor(as.factor(inspamtest$type))
w[,1:57]=scale(w[,1:57],center=TRUE, scale=TRUE)
w=data.frame(w)
colnames(w)=spname


w.type=w$type

#Find the best size
for(i in 1:10){  
  nn1=nnet(type~.,data=x, size=i, maxit=5000, decay=0.0, rang=0.5, trace=F)
  nn1.predict=predict(nn1, newdata = w[,1:57],type="class") 
  nn1.out=nn1.predict
  err=sum(nn1.out!=w.type)/(length(nn1.out))
  print(paste0(err))
}

#for the size, find the best decay rate
for(j in seq(0,1,0.1)){  
  nn1=nnet(type~.,data=x, size=9, maxit=5000, decay=j, rang=0.5, trace=F)
  nn1.predict=predict(nn1, newdata = w,type="class") 
  nn1.out=nn1.predict
  err=sum(nn1.out!=w.type)/(sum(nn1.out!=w.type)+sum(nn1.out==w.type))
  print(paste0(err))
}

nn1=nnet(type~.,data=x, size=9, maxit=10000, decay=0.3, rang=0.5, trace=F)
nn1.predict=predict(nn1, newdata = w[,1:57],type="raw")
nn1.out=rep(0,length(nn1.predict))
nn1.out[nn1.predict>0.6]=1
err=sum(nn1.out!=w.type)/(length(nn1.out))
print(paste0(err))
table(w.type,nn1.out)

min.err=0.1
min.size=NA
min.decay=NA
min.thresh=NA

#for(i in 1:10){
i=6
  for(j in seq(0,1,0.1)){
    for(k in seq(0,1,0.1)){  
      nn1=nnet(type~.,data=x, size=i, maxit=5000, decay=j, trace=F)
      nn1.predict=predict(nn1, newdata = w,type="raw") 
      nn1.out=rep(0,length(nn1.predict))
      nn1.out[nn1.predict>k]=1
      print(paste0("Decay: ",j, "Threshold: ",k))
      err=sum(nn1.out!=w.type)/(length(nn1.out))
      if(err<min.err){
        min.size=i
        min.decay=j
        min.thresh=k
        min.err=err
      }
      print(paste0(err))
    }
  }
#}

min.err
min.size
min.decay
min.thresh