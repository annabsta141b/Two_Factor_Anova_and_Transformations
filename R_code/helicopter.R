
Helicopter = read.csv("~/LocalDocuments/UCDavis/Year2/STA 106/datasets/Helicopter.csv")
Helicopter$Shift = as.factor(Helicopter$Shift)
h.model = lm(formula = Count ~ Shift, data = Helicopter)
##################### Boxplot & histogram #####################
boxplot(Count ~ Shift, data = Helicopter, main = "Helicopter Count by Shift", horizontal = TRUE)

library(ggplot2)
ggplot(Helicopter, aes(x = Count)) + geom_histogram(binwidth = 0.5) + facet_grid(Shift ~.) +ggtitle("Helicopter Count by Shift")

##################### test normality ##################### 

## qqplot 
h.model = lm(Count ~ Shift, data = Helicopter)
qqnorm(h.model$residuals)
qqline(h.model$residuals)

## Shapiro-Wilkes Test
h.ei = h.model$residuals
the.SWtest = shapiro.test(h.ei)
the.SWtest


##################### variance ##################### 

###Errors Vs. Groups
qplot(Shift, h.ei, data = Helicopter) +  ggtitle("Errors vs. Groups") + xlab("Groups") + ylab("Errors") + geom_hline(yintercept = 0,col = "purple")
###Brown-Forsythe Test
library(car)
the.BFtest = leveneTest(h.ei~ Shift, data=Helicopter, center=median)
p.val = the.BFtest[[3]][1]
p.val
### Model Fit
aggregate(Count ~ Shift, data = Helicopter, mean)
sd(Helicopter$Count)
anova.table = anova(h.model)

SSE= anova.table[2,2]


##################### box cox ##################### 

library(EnvStats)
h.model = lm(Count ~ Shift, data = Helicopter)
boxcox(h.model ,objective.name = "PPCC")

boxcox(h.model ,objective.name = "Shapiro-Wilk")
boxcox(Helicopter$Count,objective.name = "Log-Likelihood")
L2 = boxcox(h.model ,objective.name = "Shapiro-Wilk",optimize = TRUE)$lambda
L3 = boxcox(Helicopter$Count,objective.name = "Log-Likelihood",optimize = TRUE)$lambda
L2
L3

##################### transform ##################### 
## shapiro W
par(mfrow = c(1,2))
YTS = (Helicopter$Count^(L2)-1)/L2
tS.data = data.frame(Count = YTS, Shift = Helicopter$Shift)
tS.model = lm(Count~Shift,data = tS.data)
plot(tS.data$Shift, tS.data$Count)
qqnorm(tS.model$residuals)
qqline(tS.model$residuals)
qplot(Shift, tS.model$residuals, data = tS.data) +  
  ggtitle("Errors vs. Groups") + xlab("Groups") + 
  ylab("Errors") + geom_hline(yintercept = 0,col = "purple")
the.SWtest.tS = shapiro.test(tS.model$residuals)
the.SWtest.tS
the.BFtest.tS = leveneTest(tS.model$residuals~ Shift, data=tS.data, center=median)
p.val.tS = the.BFtest.tS[[3]][1]
p.val.tS


## Transformed data (LL)
par(mfrow = c(1,2))
YTS = (Helicopter$Count^(L3)-1)/L3
tLL.data = data.frame(Count = YTS, Shift = Helicopter$Shift)
tLL.model = lm(Count~Shift,data = t.data)
plot(tLL.data$Shift, tLL.data$Count)
qqnorm(tLL.model$residuals)
qqline(tLL.model$residuals)

the.SWtest.tLL = shapiro.test(tLL.model$residuals)
the.SWtest.tLL
the.BFtest.tLL = leveneTest(tLL.model$residuals~ Shift, data=tLL.data, center=median)
p.val.tLL = the.BFtest.tLL[[3]][1]
p.val.tLL
##################### outliers ############################

tS.data$ei = tS.model$residuals
nt = nrow(tS.data) #Calculates the total sample size
a = length(unique(tS.data$Shift)) #Calculates the value of a
SSE.tS = sum(tS.data$ei^2) #Sums and squares the errors (finds SSE)
MSE.tS = SSE.tS/(nt-a) #Finds MSE
eij.star = tS.model$residuals/sqrt(MSE.tS)
alpha = 0.05
t.cutoff= qt(1-alpha/(2*nt), nt-a)
CO.eij = which(abs(eij.star) > t.cutoff)
CO.eij





