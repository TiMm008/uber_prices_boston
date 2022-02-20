f_analyse<- function(MergeFile){
  cab_rides        <- MergeFile
  price            <- cab_rides$price
  distance         <- cab_rides$distance
  surge_multiplier <- cab_rides$surge_multiplier
  destination      <- cab_rides$destination
  
  
  linreg <- lm(price ~ distance+surge_multiplier+factor(destination))
  
  summary(linreg)
  
  cor.test(price, distance)
  
  #Visualisation
  plot(surge_multiplier, price)
  boxplot(price ~ distance)
  
  # 
  # cor(cab_rides)
  
  return (cor(cab_rides))
}