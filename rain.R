#comparison between "price" and "rain"
#correlation
ct_r<-cor.test(uber_weather$price, uber_weather$rain)
ct_r

#relation between price and rain 
lm_p_r<-ggplot(uber_weather[!is.na(uber_weather$rain)&!is.na(uber_weather$price),], aes(x=rain, y=price))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line())+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Linear Regression of Pirce and Rain", x="Rain", y="Price")
lm_p_r

#-------------------------------------------------------------------------------
ct_rain_or_not<-cor.test(uber_weather$rain, uber_weather$humidity)
ct_rain_or_not

h_r<-ggplot(uber_weather[!is.na(uber_weather$humidity),], aes(x=humidity, y=rain))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line())+
  labs(title="Linear Regression of Humidity and Rain", x="Humidity", y="Rain")
h_r
