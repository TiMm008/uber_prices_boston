#comparison between "price" and "source"
#transfer the car types into the numbers
unique(uber_weather$source)
v_source<-c("South Station",
            "Haymarket Square",
            "North End",
            "Boston University", 
            "Northeastern University", 
            "Fenway",
            "West End",
            "Theatre District",
            "Back Bay",
            "Financial District",
            "Beacon Hill",
            "North Station")
for(i in 1:12){
  uber_weather$num_source[uber_weather$source == v_source[i]]<- i}

#correlation
ct_source<-cor.test(uber_weather$price, uber_weather$num_source)
ct_source


#frequency of source in (colored by car type)
table_source <- table(cab_rides$source)
print(table_source)
prop.table(table_source)

fre_s_type<-ggplot(uber_weather[!is.na(uber_weather$source),], aes(x=source, fill=name))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Frequency of Source by Car Type", x="Source", y="Frequency", fill="Car Type")
fre_s_type

#relation between source distance and price
by(cab_rides[,c(1, 5)], cab_rides[,"source"], FUN = summary)

boxplot(uber_weather$distance ~ uber_weather$source, col = "yellow", main = paste("Boxplot for Distance according to the source"), cex.axis= 0.6, xlab = "Source", ylab = "Distance")

#relation between car type, source and the accumulation of price
s_type<-ggplot(uber_weather[!is.na(uber_weather$source),], aes(x=source, y=price))+
  geom_col()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Total Price by Source", x="Source", y="Price",fill="Car Type")
s_type

#price in each source
s_each_p<-ggplot(uber_weather[!is.na(uber_weather$source),],aes(x=source, y=price))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Boxplot of Source Arroding to Price", x="Price", y="Source")
s_each_p

#average price in each source
avg_p_each_s<-as.data.frame(aggregate(uber_weather$price,by=list(uber_weather$source),FUN=mean))%>%
  mutate(Condition=ifelse(x>avg_p, "greater than average price", "less than average price"))
avg_p_each_s

bar_avg_p_each_s<-ggplot(avg_p_each_s, aes(x=Group.1, y=x,fill=Condition))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title="Average Price by Source", x="", y="")
bar_avg_p_each_s
