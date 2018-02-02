### Loading Packages
```{r setup, include=TRUE,message=FALSE}

library (tidyr)
library (readr)                                              
library (dplyr)
library (ggplot2)
library(reshape2)
```

## Load the data in RStudio
```{r, include=TRUE}

data_original <- read_csv("Capstone_CSV.csv",col_names = TRUE)
data_original<-data.frame(data_original)
colnames(data_original)[31]<-paste("Result")
data_original$WEBSITE_ID <- seq.int(nrow(data_original))
head(data_original)
data_original_Feat<-select(data_original_Feat,1:30)
ADRBBF<-data.frame(ADRBBF)
head(data_original_Feat)
colnames(data_original_Feat)

levels_original=unique(do.call(c,data_original_Feat))
out_data_original_Feat <- sapply(levels,function(x)rowSums(data_original_Feat==x))
head(out_data_original_Feat)
colnames(out_data_original_Feat) <- c("Phishing","Legitimate","Suspicious")
out_data_original_Feat<-data.frame(out_data_original_Feat)
out_data_original_Feat$WEBSITE_ID <- seq.int(nrow(out_data_original_Feat))

data_original_new<-merge(data_original,out_data_original_Feat,by="WEBSITE_ID")
head(data_original_new)

data_original_Summ<-data_original_new%>%select(Phishing,Suspicious,Legitimate,Result,WEBSITE_ID)%>%mutate(Phishing_norm=Phishing/30,Suspicious_norm=Suspicious/30,Legitimate_norm=Legitimate/30)
data_original_gather<-data_original_Summ%>%gather("Type","value",6:8)
head(data_original_Summ)
head(data_original_gather)
ggplot(data_original_gather,aes(x=WEBSITE_ID,y=value,col=factor(Result)))+geom_jitter()+facet_grid(.~Type)

data_original_gather%>%ggplot(aes(x=WEBSITE_ID,y=value))+geom_area(aes(color=Type,fill=Type),position = 'stack')+facet_grid(.~Result)


```
str(data_original)
names(data_original)
class(data_original)

table(data_original$`Result  { -1,1 }`)/length(data_original$`Result  { -1,1 }`)



ADRBBF<- select(data_original,1:12)
ADRBBF_ID<- select(data_original,1:12,Result,WEBSITE_ID)

ADRBBF<-data.frame(ADRBBF)
colnames(ADRBBF)
glimpse(ADRBBF)
head(ADRBBF)

levels_1=unique(do.call(c,ADRBBF))
out_ADRBBF <- sapply(levels,function(x)rowSums(ADRBBF==x))
head(out_ADRBBF)

colnames(out_ADRBBF) <- c("Phishing","Legitimate","Suspicious")
out_ADRBBF<-data.frame(out_ADRBBF)
out_ADRBBF$WEBSITE_ID <- seq.int(nrow(out_ADRBBF))


ADRBBF_new<-merge(ADRBBF_ID,out_ADRBBF,by="WEBSITE_ID")
head(ADRBBF_new)

ADRBBF_Summ<-ADRBBF_new%>%select(Phishing,Suspicious,Legitimate,Result,WEBSITE_ID)%>%mutate(Phishing_norm=Phishing/12,Suspicious_norm=Suspicious/12,Legitimate_norm=Legitimate/12)
ADRBBF_gather<-ADRBBF_Summ%>%gather("Type","value",6:8)
head(ADRBBF_Summ)
head(ADRBBF_gather)
ggplot(ADRBBF_gather,aes(x=WEBSITE_ID,y=value,col=factor(Result)))+geom_jitter()+facet_grid(.~Type)

ADRBBF_gather%>%ggplot(aes(x=WEBSITE_ID,y=value))+geom_area(aes(color=Type,fill=Type),position = 'stack')+facet_grid(.~Result)

head(ADRBBF_ID)
colnames(ADRBBF_ID)
ADRBBF_ID_GA1<-ADRBBF_ID%>%gather("Features","value",1:6)
ADRBBF_ID_GA2<-ADRBBF_ID%>%gather("Features","value",7:12)

ggplot(ADRBBF_ID_GA1,aes(x=Features,fill=factor(value)))+geom_bar(position = "dodge")+facet_grid(.~Result)+ggtitle("Features Distribution")
ggplot(ADRBBF_ID_GA2,aes(x=Features,fill=factor(value)))+geom_bar(position = "dodge")+facet_grid(.~Result)+ggtitle("Features Distribution")




ABBF_ID<- select(data_original,13:18,Result,WEBSITE_ID)
ABBF<- select(data_original,13:18)

ABBF<-data.frame(ABBF)
colnames(ABBF)
glimpse(ABBF)
head(ABBF)

levels_2=unique(do.call(c,ABBF))
out_ABBF <- sapply(levels,function(x)rowSums(ABBF==x))
head(out_ABBF)

colnames(out_ABBF) <- c("Phishing","Legitimate","Suspicious")
out_ABBF<-data.frame(out_ABBF)
out_ABBF$WEBSITE_ID <- seq.int(nrow(out_ABBF))

ABBF_new<-merge(ABBF_ID,out_ABBF,by="WEBSITE_ID")
head(ABBF_new)

ABBF_Summ<-ABBF_new%>%select(Phishing,Suspicious,Legitimate,Result,WEBSITE_ID)%>%mutate(Phishing_norm=Phishing/6,Suspicious_norm=Suspicious/6,Legitimate_norm=Legitimate/6)
ABBF_gather<-ABBF_Summ%>%gather("Type","value",6:8)
head(ABBF_Summ)
head(ABBF_gather)
ggplot(ABBF_gather,aes(x=WEBSITE_ID,y=value,col=factor(Result)))+geom_jitter()+facet_grid(.~Type)

ABBF_gather%>%ggplot(aes(x=WEBSITE_ID,y=value))+geom_area(aes(color=Type,fill=Type),position = 'stack')+facet_grid(.~Result)

head(ABBF_ID)
ABBF_ID_GA<-ABBF_ID%>%gather("Feature","value",1:6)
ggplot(ABBF_ID_GA,aes(x=Feature,fill=factor(value)))+geom_bar(position = "dodge")+facet_grid(.~Result)+ggtitle("Abnormal Based Features Distribution")



HJBF<- select(data_original,19:23)
HJBF_ID<- select(data_original,19:23,Result,WEBSITE_ID)

HJBF<-data.frame(HJBF)
colnames(HJBF)<-paste(c("Redirect","on_mouseover","RightClick","popUpWidnow","Iframe"))
colnames(HJBF_ID)<-paste(c("Redirect","on_mouseover","RightClick","popUpWidnow","Iframe","Result","WEBSITE_ID"))
glimpse(HJBF)
head(HJBF)

levels_3=unique(do.call(c,HJBF))
out_HJBF <- sapply(levels_3,function(x)rowSums(HJBF==x))
head(out_HJBF)

colnames(out_HJBF) <- c("Phishing","Legitimate")
out_HJBF<-data.frame(out_HJBF)
out_HJBF$WEBSITE_ID <- seq.int(nrow(out_HJBF))

HJBF_new<-merge(HJBF_ID,out_HJBF,by="WEBSITE_ID")
head(HJBF_new)

HJBF_Summ<-HJBF_new%>%select(Phishing,Legitimate,Result,WEBSITE_ID)%>%mutate(Phishing_norm=Phishing/5,Legitimate_norm=Legitimate/5)
HJBF_gather<-HJBF_Summ%>%gather("Type","value",5:6)
head(HJBF_Summ)
head(HJBF_gather)
ggplot(HJBF_gather,aes(x=WEBSITE_ID,y=value,col=factor(Result)))+geom_jitter()+facet_grid(.~Type)

HJBF_gather%>%ggplot(aes(x=WEBSITE_ID,y=value))+geom_area(aes(color=Type,fill=Type),position = 'stack')+facet_grid(.~Result)

head(HJBF_ID)
HJBF_ID_GA<-HJBF_ID%>%gather("Features","value",1:5)

ggplot(HJBF_ID_GA,aes(x=Features,fill=factor(value)))+geom_bar(position = "dodge")+facet_grid(.~Result)+ggtitle("HTML & JavaScript Based Features Distribution")




DBF<- select(data_original,24:30)
DBF_ID<- select(data_original,24:31,WEBSITE_ID)
DBF<-data.frame(DBF)
colnames(DBF)
glimpse(DBF)

levels=unique(do.call(c,DBF))
out_DBF <- sapply(levels,function(x)rowSums(DBF==x))
head(out_DBF)
colnames(out_DBF) <- c("Phishing","Legitimate","Suspicious")
out_DBF<-data.frame(out_DBF)
out$WEBSITE_ID <- seq.int(nrow(out))


DBF_new<-merge(DBF_ID,out_DBF,by="WEBSITE_ID")
head(DBF_new)

DBF_Summ<-DBF_new%>%select(Phishing,Suspicious,Legitimate,Result,WEBSITE_ID)%>%mutate(Phishing_norm=Phishing/8,Suspicious_norm=Suspicious/8,Legitimate_norm=Legitimate/8)
DBF_gather<-DBF_Summ%>%gather("Type","value",6:8)
head(DBF_Summ)
ggplot(DBF_gather,aes(x=WEBSITE_ID,y=value,col=factor(Result)))+geom_jitter()+facet_grid(.~Type)
#ggplot(DBF_Summ,aes(x=WEBSITE_ID,y=Legitimate_norm,col=factor(Result)))+geom_jitter()
#ggplot(DBF_Summ,aes(x=WEBSITE_ID,y=Suspicious_norm,col=factor(Result)))+geom_jitter()


#DBF_gather%>%filter(Result==-1)%>%ggplot(aes(x=WEBSITE_ID,y=value))+geom_area(aes(color=Type,fill=Type),position = 'stack')

#DBF_gather%>%filter(Result==1)%>%ggplot(aes(x=WEBSITE_ID,y=value))+geom_area(aes(color=Type,fill=Type),position = 'stack')


DBF_gather%>%ggplot(aes(x=WEBSITE_ID,y=value))+geom_area(aes(color=Type,fill=Type),position = 'stack')+facet_grid(.~Result)

head(DBF_ID)
DBF_ID_GA<-DBF_ID%>%gather("Features","value",1:7)
ggplot(DBF_ID_GA,aes(x=Features,fill=factor(value)))+geom_bar(position = "dodge")+facet_grid(.~Result)+ggtitle("Domain Based Features Distribution")



DBF_long<-melt(DBF,id.var="Result")
head(DBF_long)


