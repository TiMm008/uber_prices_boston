#summary traffic
summary(uber_weather$traffic)

#correlation
ct_traffic<-cor.test(uber_weather$price, uber_weather$traffic)
ct_traffic

#linear regression 
lm_traffic<-ggplot(uber_weather[!is.na(uber_weather$price),], aes(x=traffic, y=price))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line())+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Linear Regression of Traffic Condition and Price", x="Traffic Condition", y="Price")

lm_traffic

#price, hour and traffic condition
traffic_hour<-ggplot(uber_weather, aes(x=n_hour, y=price, fill=traffic))+
  geom_tile()+
  labs(title="Title", x="Hour", y="Price", fill="Traffic")+
  scale_y_continuous(labels=scales::dollar_format())+
  scale_fill_viridis(discrete=FALSE) 
  
traffic_hour

fre_hour<-ggplot(uber_weather, aes(x=n_hour, fill=as.character(traffic)))+
  geom_bar()+
  labs(title="Title", x="Hour", y="Frequency")

fre_hour
