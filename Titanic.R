library (tidyr)
library (readr)                                              
library (dplyr)
library(ggplot2)

rm(titanic_original )

titanic_original <- read_csv("titanic_original.csv",col_names = TRUE)
glimpse(titanic_original)

titanic_original%>%select(embarked)
titanic_original%>%select(embarked)%>%filter(is.na(embarked))

titanic_original$embarked<-  replace_na(titanic_original$embarked,list(embarked="S"))
tbl_df(titanic_original)

titanic_original%>%filter(!is.na(age))%>%summarise(mean=mean(age),max=max(age),min=min(age),median=median(age))
titanic_original%>%mutate(age=round(age,0))%>%group_by(age)%>%summarise(count_age=n())%>%arrange(desc(count_age))
titanic_original%>%mutate(age=round(age,0))%>%group_by(age)%>%summarise(count_age=n())%>%arrange(count_age)

titanic_original%>%filter(!is.na(age))%>%mutate(age=round(age,0))%>%group_by(age)%>%summarise(count_age=n())%>%arrange(desc(count_age))%>%with(plot(age,count_age,main = "AGE  DISTRIBUTION  "))

titanic_original%>%filter(!is.na(age))%>%group_by(sex)%>%summarise(mean=mean(age),max=max(age),min=min(age),median=median(age))
titanic_original%>%filter(!is.na(age))%>%mutate(age=round(age,0))%>%group_by(age,sex)%>%summarise(count_age=n())%>%arrange(desc(count_age))%>%with(plot(age,count_age,col=c("red","blue"),main = "AGE  DISTRIBUTION  "))


ggplot( titanic_original,aes(x = age,y=age, col = sex)) +
  geom_point(size=3,alpha=0.5)


titanic_original%>%filter(!is.na(age))%>%mutate(age=round(age,0))%>%group_by(age,sex,survived)%>%summarise(count_age=n())%>%arrange(desc(count_age))%>%ggplot( aes(x = age,y=count_age, col = sex)) +
  geom_point()+ facet_grid(. ~ survived)+geom_smooth()



titanic_original%>%filter(!is.na(age),sex=='male')%>%mutate(age=round(age,0))%>%group_by(age,sex,survived)%>%summarise(count_age=n())%>%arrange(desc(count_age))%>%ggplot( aes(x = age,y=count_age,col=factor(survived)) +
  geom_point()+ facet_grid(. ~ survived)+geom_smooth()
titanic_original%>%filter(!is.na(age),sex=='female')%>%mutate(age=round(age,0))%>%group_by(age,sex,survived)%>%summarise(count_age=n())%>%arrange(desc(count_age))%>%ggplot( aes(x = age,y=count_age,col=factor(survived))) +
  geom_point()+ facet_grid(. ~ survived)+geom_smooth()


legend(x=64,y=30,legend=c("FEMALE","MALE"),fill=c("red","blue"))
Male_mean_survied<-titanic_original%>%filter(!is.na(age),sex=="male",survived==1)%>%summarise(mean=mean(age))
Male_mean_not_survied<-titanic_original%>%filter(!is.na(age),sex=="male",survived==0)%>%summarise(mean=mean(age))
Female_mean_survied<-titanic_original%>%filter(!is.na(age),sex=="female",survived==1)%>%summarise(mean=mean(age))
Female_mean_not_survied<-titanic_original%>%filter(!is.na(age),sex=="female",survived==0)%>%summarise(mean=mean(age))
Male_mean_survied
Male_mean_not_survied
Female_mean_survied
Female_mean_not_survied

titanic_original<-titanic_original%>%mutate(age=ifelse(is.na(age) & sex=="male" , 30.58523, ifelse(is.na(age) & sex=="female" , 28.68707,age)))


titanic_original<-titanic_original%>%mutate(boat=ifelse(is.na(boat)  ,"None",boat))
titanic_original%>%group_by(boat)%>%summarise(count_boat=n())%>%arrange(desc(count_boat))

titanic_original<-titanic_original%>%mutate(has_cabin_number=ifelse(is.na(cabin)  ,0,1))
titanic_original%>%select(cabin,has_cabin_number,survived)




titanic_original%>%filter(! is.na(survived))%>%group_by(survived,has_cabin_number)%>%summarise(count_survied=n())
