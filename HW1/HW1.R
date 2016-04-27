library(rpart)
Income=read.csv("Income_Data.txt")
ModIncome=data.frame(Inc=Income$X9,sex=Income$X2,marital=Income$X1,age=Income$X5,edu=Income$X4,occ=Income$X5.1,dwelltime=Income$X5.2,dual=Income$X3,hh=Income$X3.1,hh18=Income$X0,house=Income$X1.1,hometype=Income$X1.2,Ethnic=Income$X7,lang=Income$NA.)

Inc=factor(ModIncome$Inc, levels=1:9, labels=c("<10K","10-15K","15-20K","20-25K","25-30K","30-40K","40-50K","50K-75K",">75K"))
sex=factor(ModIncome$sex, levels=1:2, labels=c("Male","Female"))
marital=factor(ModIncome$marital, levels=1:5,labels=c("Married","live-in","Divorced","Seperated","Single"))
age=factor(ModIncome$age,levels=1:7,labels=c("14-17","18-24","25-34","35-44","45-54","55-64","over 65"))
edu=factor(ModIncome$edu,levels=1:6,labels=c("less grade 8","grade 9-11","grad high","1-3 college","College grad","Grad"))
occ=factor(ModIncome$occ,levels=1:9,labels=c("Professional","Sales","laborer","Clerk","Home","Student","Military","Retired","Unemployed"))
dwelltime=factor(ModIncome$dwelltime,levels=1:5,labels=c("<1year","1-3 years","4-6 years","7-10 years",">10 years"))
dual=factor(ModIncome$dual, levels=1:3, labels=c("Not Married","Yes","No"))
hh=factor(ModIncome$hh, levels=1:9, labels=c("1","2","3","4","5","6","7","8",">9"))
hh18=factor(ModIncome$hh18, levels=1:9, labels=c("1","2","3","4","5","6","7","8",">9"))
house=factor(ModIncome$house, levels=1:3, labels=c("Own","Rent","Live with family"))
hometype=factor(ModIncome$house,levels=1:5, labels =c("House","Condo","Apa","Mobile","Other"))
ethnic=factor(ModIncome$Ethnic, levels=1:8, labels=c("American Ind","Asian","Black","East indian","Hispanic","Pacific Island","White","Other"))
lang=factor(ModIncome$lang,levels=1:3, labels=c("English","Spanish","Other"))

FinalInc=data.frame(Inc=Inc,sex=sex,marital=marital,age=age,edu=edu,occ=occ, dwelltime=dwelltime, dual=dual, hh=hh, hh18=hh18,house=house, hometype=hometype,ethnic=ethnic, lang=lang)

incfit=rpart(Inc~.-Inc,FinalInc)
plot(incfit)
text(incfit)
summary(incfit)

mydata=data.frame(Inc=" ",sex="Male",marital="Married",age="35-44",edu="Grad",occ="Professional",dwelltime="1-3 years",dual="No",hh="3",hh18="3",house="Own",hometype="House",ethnic="Asian",lang="Other")
tree.pred=predict(incfit,mydata, type="class")
tree.pred

# Root had 8992 elements
  # Prediction for Root=<10K
  # Expected Loss is 0.8059386 implying that 8992*(0.8059386)=7247 have annual income
  # greater than >10K hence that is the error rate when root is classified <10K
  # Class counts are 1745(<10K); 775(10-15K); 667(15-20K), 812(20-25K),722(25-30K),1110(30-40K), 969(40-50K), 1308(50-75K), 883(>75K)
  # Since highest probability/count is class <10K=1745 entries that is chosen as representative
  # value at the root node.
  # Left node has 1843 observations and right node has 7149 observations.
  # best primary split was occupation and age is fairly close
  # 136 entries have occupation missing=> 8992-136=8854 had occupation entries
  # Age is chosen as surrogate as the split with age is similar to  8854*0.859=7605 Entries
  # Age split as LRRRRRRR implying 14-17 as left class and all others as right


