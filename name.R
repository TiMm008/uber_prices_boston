
install.packages("gridExtra")
library(gridExtra)
install.packages("hrbrthemes")
library(hrbrthemes)

#comparison between "price" / "distance" and "name(car type)"
#transfer the car types into the numbers : 1 is most cheapest ; 6 is most expensive
unique(uber_weather$name)
v_name<-c("UberPool",
          "UberX",
          "WAV",
          "UberXL",
          "Black",
          "Black SUV")
for(i in 1:6 ){
  uber_weather$n_num_name[uber_weather$name == v_name[i]]<- i}

uber_weather$name<-factor(uber_weather$name, levels = c("Black SUV", "Black", "UberXL", "WAV", "UberPool"))

#correlation
ct_type<-cor.test(uber_weather$price, uber_weather$n_num_name)
ct_type

#boxplot of car type and price
box_type<-ggplot(uber_weather, aes(x=name, y=price))+
  geom_boxplot()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Boxplot of Car Type According to Price", x="Car Type", y="Price")
  
box_type

#boxplot of car type and per mile
box_pm_type<-ggplot(uber_weather, aes(x=name, y=per_mile))+
  geom_boxplot()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Boxplot of Car Type According to Price Per Mile", x="Car Type", y="Price")

box_pm_type

#liner regression of each car type (price)
lm_type<-function(i){
  uber_weather%>%
    filter(name==i)%>%
    ggplot(aes(x=distance, y=price))+
    geom_point()+
    scale_y_continuous(labels=scales::dollar_format())+
    labs(title=i, x="Distance(Mile)", y="Price")}

lm_black_suv<-lm_type("Black SUV")
lm_black_suv

lm_black<-lm_type("Black")
lm_black

lm_uberxl<-lm_type("UberXL")
lm_uberxl

lm_wav<-lm_type("WAV")
lm_wav

lm_uberx<-lm_type("UberX")
lm_uberx

lm_uberpool<-lm_type("UberPool")
lm_uberpool

grid.arrange(lm_black_suv, lm_black, lm_uberxl, lm_wav, lm_uberx, lm_uberpool, ncol=3, nrow=2)

#liner regression of each car type (price per mile)
lm_pm_type<-function(i){
  uber_weather%>%
    filter(name==i)%>%
    ggplot(aes(x=distance, y=per_mile))+
    geom_point()+
    scale_y_continuous(labels=scales::dollar_format())+
    labs(title=i, x="Distance(Mile)", y="Avg(Price/Distance)")}

lm_pm_black_suv<-lm_pm_type("Black SUV")
lm_pm_black_suv

lm_pm_black<-lm_pm_type("Black")
lm_pm_black

lm_pm_uberxl<-lm_pm_type("UberXL")
lm_pm_uberxl

lm_pm_wav<-lm_pm_type("WAV")
lm_pm_wav

lm_pm_uberx<-lm_pm_type("UberX")
lm_pm_uberx

lm_pm_uberpool<-lm_pm_type("UberPool")
lm_pm_uberpool

grid.arrange(lm_pm_black_suv, lm_pm_black, lm_pm_uberxl, lm_pm_wav, lm_pm_uberx, lm_pm_uberpool, ncol=3, nrow=2)

#frequency of each car types
fre_type<-ggplot(uber_weather[!is.na(uber_weather$name),], aes(x=name))+
  geom_bar()+
  labs(title="Frequency of Each Car Type", x="Car Type", y="Frequency")
fre_type

#average price in each car type
avg_p_each_n<-as.data.frame(aggregate(uber_weather$price,by=list(uber_weather$name),FUN=mean))%>%
  mutate(Condition=ifelse(x>avg_p, "greater than average price", "less than average price"))
avg_p_each_n

bar_avg_p_each_n<-ggplot(avg_p_each_n, aes(x=reorder(Group.1,-x), y=x, fill=Condition))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Average Price in Car Type", x="Car Type", y="Price")
bar_avg_p_each_n

#relation between distance, price and car type
p_d_type<-ggplot(uber_weather[!is.na(uber_weather$distance)&!is.na(uber_weather$price),], aes(x=distance,y=price))+
  geom_point(aes(color=name))+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Distance According to Price in Each Car Type", x="Distance(Mile)", y="Price", color="Car Type")
p_d_type


