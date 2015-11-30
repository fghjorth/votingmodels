#load original data sets
require(foreign)
setwd("C:/Users/fh/Dropbox/Data") #pc
#setwd("/Users/frederikhjorth/Dropbox/Data") #mac
FT94.a<-read.csv(file="ElectionStudy-1994/ElectionStudy-1994_F1.csv",sep=",")
FT01.a<-read.csv(file="ElectionStudy-2001/ElectionStudy-2001_F1.csv",sep=",")
FT11.a<-read.csv(file="ElectionStudy-2011/ElectionStudy-2011_F1.csv",sep=",")

require(dplyr)
require(magrittr)

FT94<-FT94.a %>%
  transmute(lrscale=ifelse(v330<11,(v330-1)/9,NA),
            edu=ifelse(v457<5,(v457-1)/3,NA),
            hinc=ifelse(v474<9,((v474-1)/7),NA),
            emp.student=ifelse(v470==10,1,0),
            emp.pension=ifelse(v470==11,1,0),
            leftvot.p=ifelse(v114 %in% c(1,2,4,6,13),1,0),
            female=ifelse(v440==2,1,0),
            age=as.numeric(1994-v401-1900),
            eupos=ifelse(v318<6,(v318-5)*-.25,NA),
            elecyr=1994)

FT01<-FT01.a %>%
  transmute(lrscale=ifelse(FT01.a$v187<11,(FT01.a$v187)/10,NA),
            envir=ifelse(FT01.a$v124<6,(FT01.a$v124-1)/4,NA),
            edu=ifelse(FT01.a$v283<5,(FT01.a$v283-1)/3,NA),
            hinc=ifelse(FT01.a$v274<17,((FT01.a$v274-1)/16),NA),
            emp.student=ifelse(FT01.a$v288==9,1,0),
            emp.pension=ifelse(FT01.a$v288==10,1,0),
            postalnum=as.character(FT01.a$v8),
            leftvot.p=ifelse(FT01.a$v32 %in% c(1,2,5,10),1,0),
            female=ifelse(FT01.a$v270==2,1,0),
            age=as.numeric(2002-FT01.a$v271-1900),
            eupos=ifelse(v240<6,(v240-1)/4,NA),
            elecyr=2001)

FT11<-FT11.a %>%
  transmute(lrscale=ifelse(v227<11,(v227)/10,NA),
            edu=ifelse(v327<6,(v327-1)/4,.5),
            hinc=ifelse(v313<16,((v313-1)/14),NA),
            emp.student=ifelse(v333==5,1,0),
            emp.pension=ifelse(v333 %in% 8:9,1,0),
            leftvot.p=ifelse(v67 %in% c(1,2,4,6,9),1,0),
            female=ifelse(v5==5,1,0),
            age=as.numeric(2011-v6),
            eupos=ifelse(v207<6,(v207-5)*-.25,NA),
            elecyr=2011)

aggd<-bind_rows(select(FT94,eupos,age,female,edu,hinc,lrscale,elecyr),
                select(FT01,eupos,age,female,edu,hinc,lrscale,elecyr),
                select(FT11,eupos,age,female,edu,hinc,lrscale,elecyr))

rm(FT94.a,FT01.a,FT11.a)

aggd$elecyrfac<-factor(aggd$elecyr)

m94<-lm(eupos~age+I(age^2)+female+edu+hinc,data=FT94)
m94b<-lm(eupos~age+I(age^2)+female+edu+hinc+lrscale+I(lrscale^2),data=FT94)
m01<-lm(eupos~age+I(age^2)+female+edu+hinc,data=FT01)
m01b<-lm(eupos~age+I(age^2)+female+edu+hinc+lrscale++I(lrscale^2),data=FT01)
m11<-lm(eupos~age+I(age^2)+female+edu+hinc,data=FT11)
m11b<-lm(eupos~age+I(age^2)+female+edu+hinc+lrscale++I(lrscale^2),data=FT11)
# 
require(stargazer)
# 
stargazer(m94,m01,m11,style="apsr",type="text",column.labels = c("1994","2001","2011"))
stargazer(m94b,m01b,m11b,style="apsr",type="text",column.labels = c("1994","2001","2011"))

setwd("~/GitHub/votingmodels")
stargazer(m94,m01,m11,style="apsr",type="text",column.labels = c("1994","2001","2011"),covariate.labels = c("Alder","Alder sq.","Kvinde","Uddannelse","Husstandsindkomst","Konstant"),dep.var.labels = "Støtte til EU") %>%
  writeLines(.,con="r2models.txt")

require(lme4)

aem<-lm(eupos~edu*elecyrfac,data=aggd)
alm<-lm(eupos~lrscale*elecyrfac,data=aggd)
aelm<-lm(eupos~female+age+edu*elecyrfac+lrscale*elecyrfac+I(lrscale^2)*elecyrfac,data=aggd)
summary(aem)
summary(alm)
summary(aelm)

edupreds<-as.data.frame(expand.grid(female=1,age=47,edu=seq(from=0,to=1,by=.05),elecyrfac=levels(aggd$elecyrfac),lrscale=.5))
edupreds$fit<-predict(aelm,newdata=edupreds,se.fit=T)$fit
edupreds$se<-predict(aelm,newdata=edupreds,se.fit=T)$se.fit
edupreds$var<-"Uddannelse"

lrpreds<-as.data.frame(expand.grid(female=1,age=47,lrscale=seq(from=0,to=1,by=.05),elecyrfac=levels(aggd$elecyrfac),edu=.5))
lrpreds$fit<-predict(aelm,newdata=lrpreds,se.fit=T)$fit
lrpreds$se<-predict(aelm,newdata=lrpreds,se.fit=T)$se.fit
lrpreds$var<-"V/H-placering"

preds<-bind_rows(edupreds,lrpreds) %>%
  mutate(ivar=ifelse(var=="Uddannelse",edu,lrscale))


require(ggplot2)

ggplot(preds,aes(x=ivar,y=fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-1.96*se,ymax=fit+1.96*se),alpha=.2) +
  geom_ribbon(aes(ymin=fit-1.65*se,ymax=fit+1.65*se),alpha=.2) +
  theme_bw() +
  facet_grid(var~elecyrfac,scales="free_y") +
  labs(x="Forklarende variabel",y="Støtte til EU") +
  scale_x_continuous(breaks=0:1,labels=c("Min","Max"))

ggsave(file="eugrid.png",width=7,height=5)
