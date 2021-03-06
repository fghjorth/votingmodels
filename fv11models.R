#mac
#FV11.a<-read.csv(file="/Users/frederikhjorth/Dropbox/Data/ElectionStudy-2011/ElectionStudy-2011_F1.csv",sep=",")
#pc
FV11.a<-read.csv(file="C:/Users/fh/Dropbox/Data/ElectionStudy-2011/ElectionStudy-2011_F1.csv",sep=",")


#get vars
FV11<-data.frame(elec=rep("FV11",nrow(FV11.a)))

FV11$female<-ifelse(FV11.a$v5==5,1,0)
FV11$age<-2011-FV11.a$v6
FV11$region<-as.factor(as.character(FV11.a$region))

FV11$edu<-"Anden/Vil ikke svare"
FV11$edu[FV11.a$v328==4]<-"Grundskole"
FV11$edu[FV11.a$v327 %in% 5:6]<-"Gymnasie"
FV11$edu[FV11.a$v329 %in% 5:6]<-"EUD"
FV11$edu[FV11.a$v329==7]<-"KVU"
FV11$edu[FV11.a$v329==8]<-"MVU"
FV11$edu[FV11.a$v329==10]<-"LVU"
FV11$edu<-as.factor(FV11$edu)

FV11$hinc<-NA
FV11$hinc[FV11.a$v314==1]<-"<100k"  
FV11$hinc[FV11.a$v314 %in% 2:3]<-"100k-199k" 
FV11$hinc[FV11.a$v314 %in% 4:5]<-"200k-299k" 
FV11$hinc[FV11.a$v314 %in% 6:7]<-"300k-399k" 
FV11$hinc[FV11.a$v314 %in% 8:9]<-"400k-499k" 
FV11$hinc[FV11.a$v314==10]<-"500k-599k" 
FV11$hinc[FV11.a$v314==11]<-"600k-699k" 
FV11$hinc[FV11.a$v314==12]<-"700k-799k" 
FV11$hinc[FV11.a$v314==13]<-"800k-899k" 
FV11$hinc[FV11.a$v314==14]<-"900k-999k" 
FV11$hinc[FV11.a$v314 %in% 15:20]<-">1M" 
FV11$hinc[FV11.a$v314 %in% 88:99]<-"Vil ikke svare"
FV11$hinc<-as.factor(FV11$hinc)

FV11$leftvot.p<-ifelse(FV11.a$v67 %in% c(1,2,4,6,9),1,0)
FV11$leftvot.c<-ifelse(FV11.a$v54 %in% c(1,2,4,6,9),1,0)

FV11$redvot.p<-ifelse(FV11.a$v67 %in% c(1,4,9),1,0)
FV11$redvot.c<-ifelse(FV11.a$v54 %in% c(1,4,9),1,0)

FV11$safeblue<-ifelse(FV11$leftvot.p==0 & FV11$leftvot.c==0,1,0)
FV11$safered<-ifelse(FV11$leftvot.p==1 & FV11$leftvot.c==1,1,0)
FV11$swingvoter<-(FV11$leftvot.p - FV11$leftvot.c)^2

#decided after jan 1st?
FV11$latedecider<-ifelse(FV11.a$v55<4,1,0)

#decide late AND

rm(FV11.a)
str(FV11)

#run voter models
summary(mblue<-glm(safeblue~female+age+I(age^2)+edu+hinc,data=FV11,family="binomial"))
summary(mred<-glm(safered~female+age+I(age^2)+edu+hinc,data=FV11,family="binomial"))
summary(mswing<-glm(swingvoter~female+age+I(age^2)+edu+hinc,data=FV11,family="binomial"))
summary(mlatedec<-glm(latedecider~female+age+I(age^2)+edu+hinc,data=FV11,family="binomial"))


ageedupreds<-as.data.frame(expand.grid(age=18:80,female=0,edu=levels(factor(FV11$edu)),hinc="400k-499k"))

ageedupreds$swingpredict<-predict(mswing,newdata=ageedupreds,type="response")

ageedupreds$latedecidepredict<-predict(mlatedec,newdata=ageedupreds,type="response")


require(ggplot2)
ggplot(ageedupreds,aes(x=age,y=swingpredict,group=edu,color=edu)) +
  geom_line() +
  theme_minimal()

ggplot(ageedupreds,aes(x=age,y=latedecidepredict,group=edu,color=edu)) +
  geom_line() +
  theme_minimal()

#read in agenda data
setwd("~/GitHub/votingmodels") 

poldata1<-read.csv("poldata.csv")
poldata2<-read.csv("poldata2.csv",sep=";")[,c(1:6,8:19)]

names(poldata1)
names(poldata2)<-names(poldata1)

poldata<-rbind(poldata1,poldata2)

#recode variables to fit
poldata$female<-ifelse(poldata$gender=="Kvinde",1,0)
names(poldata)[14]<-"edu"
names(poldata)[16]<-"hinc"

levels(FV11$edu)
levels(poldata$edu)
levels(poldata$edu)<-c("Anden/Vil ikke svare","EUD","Grundskole","Gymnasie","KVU","LVU","MVU","Anden/Vil ikke svare","KVU","LVU","MVU")

levels(poldata$hinc)<-c(levels(FV11$hinc)[2],levels(FV11$hinc)[3:11],levels(FV11$hinc)[1],levels(FV11$hinc)[12],levels(FV11$hinc)[12])

predictions<-data.frame(RESPID=poldata$RESPID,redprediction=predict(mred,newdata=poldata,type="response"),blueprediction=predict(mblue,newdata=poldata,type="response"),swingprediction=predict(mswing,newdata=poldata,type="response"),latedecprediction=predict(mlatedec,newdata=poldata,type="response"))

ggplot(predictions,aes(x=swingprediction)) +
  geom_histogram()

ggplot(predictions,aes(x=latedecprediction)) +
  geom_histogram()

write.csv(predictions,file="predictions.csv")
