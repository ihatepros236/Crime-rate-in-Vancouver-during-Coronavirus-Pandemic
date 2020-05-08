library(tidyverse)
library(lubridate)

setwd("C:/Users/Muhammad/Projects/Coronavirus/Coronavirus")
crimedata<-read_csv("crimedata_csv_all_years.csv")
pop_data<-read_csv("vancouver_pop_2003to2020.csv")
names(crimedata)[2]<-"year"
crimedata<-full_join(crimedata,pop_data)
unique(crimedata$TYPE)
mycrime<-filter(crimedata, year>=2017)

#creating total incidents and crime rate:

pop_data1<-filter(pop_data, year>=2017)
mycrime<- mycrime %>%
  mutate(date = make_date(year, MONTH, DAY))
mycrime<-mutate(mycrime, week=week(date))

mycrime1<-group_by(mycrime,year,week,TYPE) %>%
  summarise(total_incidents=n())
mycrime1<-full_join(mycrime1,pop_data1)

mycrime1<-mutate(mycrime1, crimeRate=(total_incidents/population)*100)
ggplot(mycrime1,aes(x=week,y=crimeRate,color=factor(year)))+
  geom_smooth(se=FALSE)+
  ggtitle("Weekly Variation of crime rate across years")


ggplot(mycrime1)+
  geom_line(aes(x=week,y=crimeRate,color=factor(year),group=year))+
  ggtitle("Weekly Variation of crime rate across years")
  
ggplot(mycrime1)+
  geom_smooth(aes(x=date,y=crimeRate,color=factor(year)))+
  ggtitle("Weekly Variation of crime rate across years")


mycrime2<-filter(mycrime, year<2020)%>%
  group_by(week,TYPE) %>%
  summarise(total_threeYr_avg=n()/3)
crime2020<-filter(mycrime1,year>=2020)
mycrime2<-full_join(crime2020,mycrime2)
mycrime2<-pivot_longer(mycrime2,cols=starts_with("total"),names_to ="incident_time",  values_to = "incidents")

ggplot(mycrime2)+
  geom_smooth(aes(x=week,y=incidents, color=incident_time),se=FALSE)




