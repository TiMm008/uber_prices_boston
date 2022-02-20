uber_weather<-MergeFile
uber_weather<-uber_weather%>%filter(distance>0.1&price<100)

#-------------------------------------------------------------------------------
#comparison between "date" and "price"
#Frequency of the date
uber_weather<-uber_weather%>%mutate(n_date=substr(n_pick_up_time,6,10))

fre_date<-ggplot(uber_weather[!is.na(uber_weather$n_date),],aes(x=n_date))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Frequency by Date", x="Date", y="Frequency")
fre_date

#-------------------------------------------------------------------------------
#comparison between "clouds" and "humidity"
#correlation
ct_cl_h<-cor.test(uber_weather$clouds, uber_weather$humidity)
ct_cl_h

#-------------------------------------------------------------------------------
#comparison between "clouds" and "pressure"
ct_cl_pr<-cor.test(uber_weather$clouds, uber_weather$pressure)
ct_cl_pr

#-------------------------------------------------------------------------------
#comparison between "humidity" and "pressure"
ct_h_pr<-cor.test(uber_weather$pressure, uber_weather$humidity)
ct_h_pr

#-------------------------------------------------------------------------------
#comparison between "wind" and "pressure"
ct_w_pr<-cor.test(uber_weather$pressure, uber_weather$wind)
ct_w_pr

#-------------------------------------------------------------------------------
#comparison between "wind" and "humidity"
ct_w_h<-cor.test(uber_weather$humidity, uber_weather$wind)
ct_w_h

#-------------------------------------------------------------------------------
#comparison between "wind" and "clouds"
ct_w_cl<-cor.test(uber_weather$clouds, uber_weather$wind)
ct_w_cl

#---------------------------------


