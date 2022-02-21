#comparison between "price" and "distance"
#classify distance
uber_weather<-uber_weather%>%
  mutate(n_distance_range=case_when(distance<=1~"<=1",
                                    distance>1&distance<=2~"1-2",
                                    distance>2&distance<=3~"2-3",
                                    distance>3&distance<=4~"3-4",
                                    distance>4&distance<=5~"4-5",
                                    distance>5&distance<=6~"5-6",
                                    distance>6~">6"))

uber_weather<- within(uber_weather,
                      n_distance_range <- factor(
                        n_distance_range,levels=c("<=1","1-2","2-3","3-4","4-5","5-6",">6")))

#summary distance
summary(uber_weather$distance)
boxplot(cab_rides$distance, col = c("yellow"), main = paste("Boxplot for Distance"), ylab = "Quantiles")


#average distance
avg_d<-mean(uber_weather$distance, na.rm = T)
avg_d

#relation between distance and price
box_d<-ggplot(uber_weather[!is.na(uber_weather$n_distance_range)|!is.na(uber_weather$price),], aes(x=n_distance_range, y=price))+
  geom_boxplot()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Boxplot of Distance According to Price", x="Distance(Mile)", y="")
box_d

#correlation
ct_distance<-cor.test(uber_weather$price, uber_weather$distance)
ct_distance

#linear regression
lm_p_d<-lm(distance~price, uber_weather)
lm_p_d
summary(lm_p_d)

line_lm_p_d<-ggplot(uber_weather[!is.na(uber_weather$distance)|!is.na(uber_weather$price),], aes(x=distance, y=price))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line())+
  labs(title="Linear Regression of Price and Distance", x="Distance(Mile)", y="Price")+
  scale_y_continuous(labels=scales::dollar_format())

line_lm_p_d

#frequency of range of distance(colored by car type)
fre_range_d<-ggplot(uber_weather[!is.na(uber_weather$n_distance_range),], aes(x=n_distance_range, fill=name))+
  geom_bar()+
  labs(title="Frequency of Distance By Car Type", x="Distance(Mile)", y="Frequency")
fre_range_d


#average price in each range of distance
avg_p_each_d<-as.data.frame(aggregate(uber_weather$price,by=list(uber_weather$n_distance_range),FUN=mean))%>%
  mutate(Condition=if_else(x>avg_p, "greater than average price", "less than average price"))
avg_p_each_d

col_avg_p_each_d<-ggplot(avg_p_each_d, aes(x=Group.1, y=x, fill=Condition))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Average Price in Each Distance Range", x="Distance(Mile)", y="Price")
col_avg_p_each_d

#the average price per mile in each distance
uber_weather<-uber_weather%>%mutate(per_mile=uber_weather$price/uber_weather$distance)

summary(uber_weather$per_mile)

per_mile<-as.data.frame(aggregate(uber_weather$per_mile,by=list(uber_weather$n_distance_range),FUN=mean))
col_per_mile<-ggplot(per_mile, aes(x=Group.1, y=x,))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Average Price Per Mile in Each Distance Range", x="Distance(Mile)", y="Avg(Price/Distance)")
col_per_mile

#frequency in each distance and each price
p_d<-ggplot(uber_weather[!is.na(uber_weather$distance)&!is.na(uber_weather$price),], aes(x=distance,y=price))+
  geom_bin2d()+
  scale_fill_gradient(low = "#353436", high = "#f6f805")+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Frequency of Distance and Price", x="Distance(Mile)", y="Price", fill="Frequency")
p_d

