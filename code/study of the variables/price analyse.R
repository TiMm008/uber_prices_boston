#summary price
summary(uber_weather$price)

boxplot(cab_rides$price, col = c("yellow"), main = paste("Boxplot for Price"), ylab = "Quantiles")

#average price
avg_p<-mean(uber_weather$price, na.rm = T)
avg_p


#density of price
den_p<-ggplot(uber_weather[!is.na(uber_weather$price),],aes(x=price))+
  geom_density(fill="grey")+
  geom_vline(aes(xintercept=avg_p), color="black", linetype="dashed", size=1)+
  scale_x_continuous(labels=scales::dollar_format())+
  labs(title="Density of Price", x="Price", y="Density")
den_p