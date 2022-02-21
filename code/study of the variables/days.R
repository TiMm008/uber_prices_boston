#comparison between "price" and "days"
#transfer the days into the numbers
unique(uber_weather$n_days)
v_days<-c("Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday")

#Friday, Saturday and Sunday are 1 the rest of days are 0
for(i in 1:7){
  uber_weather$n_weekend[uber_weather$n_days == v_days[6]|uber_weather$n_days == v_days[7]]<-1}
uber_weather$n_weekend[is.na(uber_weather$n_weekend)] <- 0

uber_weather$n_days<-factor(uber_weather$n_days, levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"))

#correlation(holiday)
ct_day<-cor.test(uber_weather$price, uber_weather$n_weekend)
ct_day

#frequency of each days (colored by car type)
fre_days<-ggplot(uber_weather[!is.na(uber_weather$n_days),],aes(x=n_days))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Frequency in Each Day of Week",x="",y="Frequency")
fre_days

#frequency of each days for each source
s_fre_days<-ggplot(uber_weather[!is.na(uber_weather$n_days),],aes(x=n_days, y=source))+
  geom_bin2d()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_fill_gradient(low = "#ccffcc", high = "#003300")+
  labs(title="Frequency in Each Day of Week and Each Source", x="", y="", fill="Frequency")
s_fre_days


#average price in each day
avg_p_each_day<-as.data.frame(aggregate(uber_weather$price,by=list(uber_weather$n_days),FUN=mean))%>%
  mutate(Condition=ifelse(x>avg_p, "greater than average price", "less than average price"))
avg_p_each_day

bar_avg_p_each_day<-ggplot(avg_p_each_day, aes(x=Group.1, y=x, fill=Condition))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Average Price in Each Day of Week", x="", y="Price")
bar_avg_p_each_day
