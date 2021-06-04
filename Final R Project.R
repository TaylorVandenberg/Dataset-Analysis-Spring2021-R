library(ISLR)
imports.85 <- read.csv("~/Desktop/imports-85.csv")
plot(imports.85[,16:25])#price and engine size are the go to

#some possible predictors for price
#r^2 .7888
m1.engn <- lm(price ~ enginesize, data=imports.85)
summary(m1.engn)

#r^2 .5147
m1.hghway <- lm(price ~ highwaympg, data=imports.85)
summary(m1.hghway)

#r^2 .6583
m1.hrspwr <- lm(price ~ horsepower, data=imports.85)
summary(m1.hrspwr)

#r^2 .2948
m1.bore <- lm(price ~ bore, data=imports.85)
summary(m1.bore)

#plotting the data
plot(imports.85$enginesize, imports.85$price)
abline(m1.engn, col='purple', lwd=2)

AIC(m1.engn, m1.hghway, m1.hrspwr, m1.bore)
#engine size has the highest r^2 as well as the lowest AIC

#new superior model, also creates xmesh and yhat for the more accurate line
m1.engn.sq <- lm(price ~ enginesize + I(enginesize^2), data=imports.85)
summary(m1.engn.sq)
AIC(m1.engn, m1.hghway, m1.hrspwr, m1.bore, m1.engn.sq) #proves new model is improved
plot(m1.engn.sq)
x <- imports.85$enginesize
xmesh <- seq(0.5*min(x), 2*max(x), by=0.1) # getting lots of x values to draw the line through
yhat <- predict(m1.engn.sq, 
                newdata=data.frame(enginesize=xmesh))

#plot the data with both lines
plot(imports.85$enginesize, imports.85$price, xlab="Engine Size", ylab="Price", main="Price vs Engine Size")
abline(m1.engn, col="red", lwd=2)
lines(xmesh, yhat, col="blue", lwd=2)
legend("topleft",  #legend location
       c("Linear", "Quadratic"), # line names
       lty=c(1,1),
       lwd=c(2,2), #line size
       col=c("red", "blue"))

#overfitting
overfit <- lm(price ~ ., data=imports.85) #r^2 .9536
summary(overfit)

#remove irrelevant data
overfit2 <- lm(price ~ . - highwaympg - citympg - horsepower - stroke - fuelsystemspfi
               - fuelsystemspdi - fuelsystemmpfi - numcylinderstwelve
               - numcylindersthree - numcylinderssix - numcylindersfour 
               - enginetypeohcf - enginetypeohc - enginetypel - drivewheelsfwd 
               - drivewheelsrwd - numdoorstwo - makevolvo - makevolkswagen 
               - makesubaru - makesaab - makeporsche - makemercury - makemercedes-benz 
               - makemazda - makejaguar - makehonda - makeaudi - symboling,
               data=imports.85)
summary(overfit2)

#multiple linear crew
m2.engn.peakrpm <- lm(price ~ enginesize+I(enginesize^2) + peakrpm+I(peakrpm^2) , data=imports.85)
summary(m2.engn.peakrpm) #r^2 .8037
plot(m2.engn.peakrpm)

#engine size and peakrpm squad
m2.interaction <- (price ~ enginesize*peakrpm)
summary(m2.engn.peakrpm) #r^2 .8037

#rss
yhat_m2 <- predict(m2.engn.peakrpm)
rss_m2.engn.peakrpm <- sum((imports.85$price - yhat_m2)^2)
rss_m2.engn.peakrpm #GIGA CHAD