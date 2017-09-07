pdat<-read.csv("New_Pharm_Data_Headers(Clean).csv",na.strings=c(98,99))
pdat$PurchaseOTC[pdat$P1==1]<-"Yes"
pdat$PurchaseOTC[pdat$P1==2]<-"No"
pdat$PurchaseOTC[pdat$P1==99]<-NA

pdat$OTCFreq[pdat$OTC_Often..Refuse.==1]<-NA
pdat$OTCFreq[pdat$OTC_Often..Don.t.Know.==1]<-"DNR"
pdat$OTCFreq[pdat$OTC_Often.Never.==1]<-"Never"
pdat$OTCFreq[pdat$OTC_Often..couple.year.==1]<-"Couple/yr"
pdat$OTCFreq[pdat$OTC_Often..1.month.==1]<-"Once/Month"
pdat$OTCFreq[pdat$OTC_Often..1.wk.==1]<-"Once/Wk"
pdat$OTCFreq[pdat$OTC_Often..Daily.==1]<-"Daily"

pdat$RAge<-NULL
pdat$RAge[pdat$Age.resp.<=5]<-"0-5"
pdat$RAge[pdat$Age.resp.<=11&pdat$Age.resp.>=6]<-"6-11"
pdat$RAge[pdat$Age.resp.<=17&pdat$Age.resp.>=12]<-"12-17"
pdat$RAge[pdat$Age.resp.<=24&pdat$Age.resp.>=18]<-"18-24"
pdat$RAge[pdat$Age.resp.<=34&pdat$Age.resp.>=25]<-"25-34"
pdat$RAge[pdat$Age.resp.<=44&pdat$Age.resp.>=35]<-"35-44"
pdat$RAge[pdat$Age.resp.<=54&pdat$Age.resp.>=45]<-"45-54"
pdat$RAge[pdat$Age.resp.<=64&pdat$Age.resp.>=55]<-"55-64"
pdat$RAge[pdat$Age.resp.<=74&pdat$Age.resp.>=65]<-"65-74"
pdat$RAge[pdat$Age.resp.<=84&pdat$Age.resp.>=75]<-"75-84"
pdat$RAge[pdat$Age.resp.<=94&pdat$Age.resp.>=85]<-"85-94"
pdat$RAge[pdat$Age.resp.>=95]<-"95+"

pdat$Income[pdat$Income_100k==1]<-">100k"
pdat$Income[pdat$Income_75k==1&pdat$Income_100k==2]<-"75-99"
pdat$Income[pdat$Income_50k==1&pdat$Income_75k==2]<-"50-74"
pdat$Income[pdat$Income_25k==1&pdat$Income_50k==2]<-"25-49"
pdat$Income[pdat$Income_25k==2]<-"<25k"


pdat$RxFreq[pdat$Rx_Often.Refuse.==1]<-NA
pdat$RxFreq[pdat$Rx_Often.Don.t.know.==1]<-"DNR"
pdat$RxFreq[pdat$Rx_Often.Never.==1]<-"Never"
pdat$RxFreq[pdat$Rx_Often.couple.year.==1]<-"Couple/yr"
pdat$RxFreq[pdat$Rx_Often.1.month.==1]<-"Once/Month"
pdat$RxFreq[pdat$Rx_Often.1.wk.==1]<-"Once/Wk"
pdat$RxFreq[pdat$Rx_Often..Daily.==1]<-"Daily"

pdat$Gender<-factor(pdat$Gender,levels=c(1,2,99),labels=c("Male","Female",NA))
pdat$Race<-factor(pdat$Race,levels=c(1,2,3,4,5),labels=c("White","Black","American Indian","Asian","Other"))
pdat$Dev.Environ<-factor(pdat$Dev.Environ,levels=c(1,2,3),labels=c("Rural","Suburban","Urban"))
pdat$Education<-factor(pdat$Education,levels=c(1,2,3,4,5,6),labels=c("<High School","High School Grad","Some College","Associates","Bachelor","Post Grad"))
pdat$Politic<-factor(pdat$Politic,levels=c(1,2,3,4,5,6),labels=c("Republican","Democrat","Independent","Progressive","Not Affiliated","Other"))
pdat$Insurance<-factor(pdat$Insurance,levels=c(1,2,3,4,5),labels=c("None","Medicare","Medicaid","Private through employer","Private thru Self"))

pdat$NumOTC<-NULL
pdat$NumOTC[pdat$Num.OTC.Past.12==0]<-"0"
pdat$NumOTC[pdat$Num.OTC.Past.12<=3&pdat$Num.OTC.Past.12>=1]<-"1-3"
pdat$NumOTC[pdat$Num.OTC.Past.12<=6&pdat$Num.OTC.Past.12>=4]<-"4-6"
pdat$NumOTC[pdat$Num.OTC.Past.12<=10&pdat$Num.OTC.Past.12>=7]<-"7-10"
pdat$NumOTC[pdat$Num.OTC.Past.12<98&pdat$Num.OTC.Past.12>10]<-">10"
pdat$NumOTC[pdat$OTC_Between..1.10.==1]<-"1-3"
pdat$NumOTC[pdat$OTC_Between..1.10.==2]<-"4-6"
pdat$NumOTC[pdat$OTC_Between..1.10.==3]<-"7-10"
pdat$NumOTC[pdat$OTC_Between..1.10.==4]<-">10"

pdat$NumRx<-NULL
pdat$NumRx[pdat$Num.Rx.past.12==0]<-"0"
pdat$NumRx[pdat$Num.Rx.past.12<=3&pdat$Num.Rx.past.12>=1]<-"1-3"
pdat$NumRx[pdat$Num.Rx.past.12<=6&pdat$Num.Rx.past.12>=4]<-"4-6"
pdat$NumRx[pdat$Num.Rx.past.12<=10&pdat$Num.Rx.past.12>=7]<-"7-10"
pdat$NumRx[pdat$Num.Rx.past.12<=98&pdat$Num.Rx.past.12>10]<-">10"
pdat$NumRx[pdat$Rx_Between.1.10.==1]<-"1-3"
pdat$NumRx[pdat$Rx_Between.1.10.==2]<-"4-6"
pdat$NumRx[pdat$Rx_Between.1.10.==3]<-"7-10"
pdat$NumRx[pdat$Rx_Between.1.10.==4]<-">10"

pdat$NumAn<-NULL
pdat$NumAn[pdat$Num_Med.Animal.<=3&pdat$Num_Med.Animal.>=1]<-"1-3"
pdat$NumAn[pdat$Num_Med.Animal.<=6&pdat$Num_Med.Animal.>=4]<-"4-6"
pdat$NumAn[pdat$Num_Med.Animal.<=10&pdat$Num_Med.Animal.>=7]<-"7-10"
pdat$NumAn[pdat$Num_Med.Animal.<=98&pdat$Num_Med.Animal.>10]<-">10"
pdat$NumAn[pdat$Med_An_Between..1.10.==1]<-"1-3"
                                     

library(dplyr)
demo<-select(pdat,id,Gender,RAge,Race,Income,Dev.Environ,Education,Politic,Insurance)
Pharm<-select(pdat,id,PurchaseOTC,Obtained.Rx.,OTCFreq,RxFreq,NumOTC,NumRx,Animal,OTC_Animal,RX_Animal,NumAn,Leftover_Meds,Dispose_Flush,Dispose_Trash,Dispose_Takeback,Dispose_Friend.Fam,Takeback_NDTBD,Takeback_Police,Takeback_Pharm,Info_How_Dispose,DTBP,Heard_Med_Waterway)
test<-merge(Pharm,demo)
head(test)
#y<-NULL
#p<-NULL
#name<-NULL
#name2<-NULL
#min<-NULL
#chi<-NULL
#head(test[23:30])
#for (i in 2:22){
 #   x<-chisq.test(test[,i],test$Insurance)
  #  y[i]<-x$statistic
   # p[i]<-x$p.value
    #min[i]<-min(x$observed)
    #name[i]<-colnames(test[i])}
   
#chiGender<-data.frame(name,y,p,min)  
#chiRAge<-data.frame(name,y,p,min)
#chiRace<-data.frame(name,y,p,min)
#chiIncome<-data.frame(name,y,p,min)
#chiEnviron<-data.frame(name,y,p,min)
#chiEdu<-data.frame(name,y,p,min)
#chiPoli<-data.frame(name,y,p,min)
#chiIns<-data.frame(name,y,p,min)


chisq.test(table(test$PurchaseOTC,test$Gender)[1:2,1:2]) # 0.291
fisher.test(table(test$PurchaseOTC,test$RAge)[1:2,3:10],workspace=2e+08) #0.13
chisq.test(table(test$PurchaseOTC,test$Insurance)) #0.11
chisq.test(table(test$PurchaseOTC,test$Income)) #0.70
chisq.test(table(test$PurchaseOTC,test$Dev.Environ)) #0.39
chisq.test(table(test$PurchaseOTC,test$Education)) #0.09
chisq.test(table(test$Obtained.Rx.,test$Gender)[1:2,1:2])#0.0021
chisq.test(table(test$Obtained.Rx.,test$RAge)[1:2,3:10])#0.0028
chisq.test(table(test$Obtained.Rx.,test$Insurance))#0.08
chisq.test(table(test$Obtained.Rx.,test$Income))#0.45
chisq.test(table(test$Obtained.Rx.,test$Dev.Environ))#0.047
chisq.test(table(test$Obtained.Rx.,test$Education)) #0.08

Var1<-c(rep("Purchase OTC",6),rep("Purchase Rx",6),rep("OTC Daily",3),rep("Rx Daily",3),rep("Rx Animal",2),rep("Leftover Meds",3),rep("Dipose Info",5),rep("Drug Takeback",4),rep("Med Waterway",6))
Var2<-c("Gender","Resp Age","Insurance","Income","Dev Environ","Education","Gender","Resp Age","Insurance","Income","Dev Environ","Education","Gender","Resp Age","Insurance","Gender","Resp Age","Insurance","Income","Dev Environ","Gender","Resp Age","Insurance","Education","Resp Age","Gender","Politics","Income","Education","Resp Age","Income","Dev Environ","Education","Resp Age","Income","Dev Environ","Gender","Politics")
p_value<-c(0.291,0.13,0.11,0.70,0.39,0.09,0.0021,0.0028,0.08,0.45,0.047,0.08,0.355,0.000001,0.0000058,0.78,0.00000000041,0.00020,0.51,0.92,0.24,0.52,0.23,0.35,0.014,0.20,0.26,0.67,0.22,0.00051,0.13,0.25,0.00028,0.0027,0.08,0.37,0.36,0.68)
chis<-cbind(Var1,Var2,p_value)
chis_1<-chis[order(p_value),]

test$FreqOTC<-ifelse(test$OTCFreq=="Daily","Daily","Less than Daily")
chisq.test(table(test$FreqOTC,test$Gender)[1:2,1:1:2]) #0.355
chisq.test(table(test$FreqOTC,test$RAge)[1:2,3:10]) #0.000001
chisq.test(table(test$FreqOTC,test$Insurance))#0.0000058

test$FreqRx<-ifelse(test$RxFreq=="Daily","Daily","Less than Daily")
chisq.test(table(test$FreqRx,test$Gender)[1:2,1:2])#0.78
chisq.test(table(test$FreqRx,test$RAge)[1:2,3:10])#0.00000000041
chisq.test(table(test$FreqRx,test$Insurance)) #0.00020

chisq.test(table(test$RX_Animal,test$Income))#0.51
chisq.test(table(test$RX_Animal,test$Dev.Environ))#0.92

chisq.test(table(test$Leftover_Meds,test$Gender)[1:2,1:2])#0.24
chisq.test(table(test$Leftover_Meds,test$RAge)[1:2,3:10])#0.52
chisq.test(table(test$Leftover_Meds,test$Insurance))#0.23

chisq.test(table(test$Info_How_Dispose,test$Education))#0.35
chisq.test(table(test$Info_How_Dispose,test$RAge)[1:2,3:10])#0.014
chisq.test(table(test$Info_How_Dispose,test$Gender)[1:2,1:2])#0.20
chisq.test(table(test$Info_How_Dispose,test$Politic))#0.26
chisq.test(table(test$Info_How_Dispose,test$Income))#0.67

chisq.test(table(test$DTBP,test$Education))#0.22
chisq.test(table(test$DTBP,test$RAge)[1:2,3:10])#0.00051
chisq.test(table(test$DTBP,test$Income))#0.13
chisq.test(table(test$DTBP,test$Dev.Environ))#0.25

chisq.test(table(test$Heard_Med_Waterway,test$Education))#0.00028
chisq.test(table(test$Heard_Med_Waterway,test$RAge)[1:2,3:10])#0.0027
chisq.test(table(test$Heard_Med_Waterway,test$Income))#0.08
chisq.test(table(test$Heard_Med_Waterway,test$Dev.Environ))#0.37
chisq.test(table(test$Heard_Med_Waterway,test$Gender)[1:2,1:2])#0.36
chisq.test(table(test$Heard_Med_Waterway,test$Politic))#0.68

chisq.test(table(test$Dispose_Takeback,test$Education))#0.13
chisq.test(table(test$Dispose_Takeback,test$RAge)[1:2,3:10])#0.44

fisher.test(table(test$Dispose_Trash,test$Education))#0.14
#fisher.test(table(test$Dispose_Trash,test$RAge),workspace=2e+08)#0.36


chisq.test(table(test$Heard_Med_Waterway,test$Dispose_Flush))#0.00016
table(test$Heard_Med_Waterway,test$Dispose_Flush)
chisq.test(table(test$Heard_Med_Waterway,test$Dispose_Trash)) #0.91
chisq.test(table(test$Heard_Med_Waterway,test$Dispose_Takeback))#0.005


table(pdat$MD_inform_Disposal)
table(pdat$Pharm_Inform_Disposal)
table(pdat$Vet_Info_Disposal)

test$InformMd<-ifelse(pdat$MD_inform_Disposal==4,"No","Yes")
test$InformPharm<-ifelse(pdat$Pharm_Inform_Disposal==4,"No","Yes")
test$InformVet<-ifelse(pdat$Vet_Info_Disposal==4,"No","Yes")
chisq.test(table(test$Heard_Med_Waterway,test$InformMd))
chisq.test(table(test$Heard_Med_Waterway,test$InformPharm))
chisq.test(table(test$Heard_Med_Waterway,test$InformVet))

table(test$InformMd,test$InformPharm)
test$Inform[test$InformMd=="Yes"&test$InformPharm=="Yes"]<-"YY"
test$Inform[test$InformMd=="Yes"&test$InformPharm=="No"]<-"YN"
test$Inform[test$InformMd=="No"&test$InformPharm=="Yes"]<-"YN"
test$Inform[test$InformMd=="No"&test$InformPharm=="No"]<-"NN"
test$Inform_<-ifelse(test$Inform=="NN"&test$InformVet=="No","No","Yes")
table(test$Inform_)
chisq.test(table(test$Heard_Med_Waterway,test$Inform_))
