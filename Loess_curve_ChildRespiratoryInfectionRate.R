#Loess 
load(url("http://www.users.muohio.edu/hughesmr/sta333/respiratory.RData"))
attach(respiratory)

head(respiratory)

#plot(x = respiratory$age,y = respiratory$rate)

plot(respiratory$rate~respiratory$age,data=respiratory)
fit <- lm(respiratory$rate~respiratory$age,data=respiratory)
#summary(fit)

predict(fit, newdata=data.frame(age=18), int="conf")
abline(fit)
#Par(mfrow=c(1,2))
plot(respiratory$age,residuals(fit))
qqnorm(residuals(fit))

plot(rate~age, data=respiratory, main="Respiratory Infection Rate vs Age")
out = list()
i=1
for(s in c(0.10,0.25,0.5,0.75)){
  loes <- loess(rate~age, data=respiratory,span=s)
  print(loes)
  out[[i]] <- predict(loes)
  i=i+1
}
#print(out)
library(ggplot2)
#curve(predict(out, newdata=data.frame(age = x)), add=TRUE)

plot(rate, x=age, type="p", main="Loess Smoothing and Prediction", 
     xlab="Child Age", ylab="Respiratory Infection Rate")
print(length(out))

color = c("red","blue","green","black")
for(k in 1:length(out)){
  lines(out[[k]], x=respiratory$age, col=color[k]) 
  
}

