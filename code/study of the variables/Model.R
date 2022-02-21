#Linear regresion
linreg <- lm(MergeFile$price ~ MergeFile$distance+factor(MergeFile$name))
linreg
summary(linreg)


# Residuals
residuals <- linreg$residuals
residuals

# Verify residuals normality 
qqnorm(residuals)
qqline(residuals)

# Verify non corelation
plot(residuals)

# Verify the power prediction of our model
cor(linreg$fitted.values, MergeFile$price)
plot(MergeFile$price, linreg$fitted.values, xlab = "Price")
