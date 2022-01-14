
Salary=read.csv("~/LocalDocuments/UCDavis/Year2/STA 106/datasets/Salary.csv")


########################### summary of data ###########################
 boxplot(Annual ~ Prof + Region, data = Salary, main = "Salary by Prof & Region",
                 horizontal = TRUE)

names(Salary) = c("Y", "A", "B")
aggregate(Y ~ A + B, data = Salary, sd)
aggregate(Y ~ A + B, data = Salary, mean)
sd(Salary$Y)
aggregate(Y ~ A, data = Salary, sd)
aggregate(Y ~ B, data = Salary, sd)
find.means = function(the.data,fun.name = mean){
  a = length(unique(the.data[,2]))
  b = length(unique(the.data[,3]))
  means.A = by(the.data[,1], the.data[,2], fun.name)
  means.B = by(the.data[,1],the.data[,3],fun.name)
  means.AB = by(the.data[,1],list(the.data[,2],the.data[,3]),fun.name)
  MAB = matrix(means.AB,nrow = b, ncol = a, byrow = TRUE)
  colnames(MAB) = names(means.A)
  rownames(MAB) = names(means.B)
  MA = as.numeric(means.A)
  names(MA) = names(means.A)
  MB = as.numeric(means.B)
  names(MB) = names(means.B)
  MAB = t(MAB)
  results = list(A = MA, B = MB, AB = MAB)
  return(results)
}
the.means = find.means(Salary)
the.model = lm(Y ~ A*B, data = Salary)
SSE = sum(the.model$residuals^2)

mean(Salary$Y)
the.data = Salary
nt = nrow(the.data)

a = length(unique(the.data[,2]))
b = length(unique(the.data[,3]))
MSE = SSE/(nt-a*b)
names(the.data) = c("Y","A","B")



############################################# interaction plot ################3
interaction.plot(Salary$A, Salary$B, Salary$Y)

# test interaction effect 
AB = lm(Y ~ A*B,Salary)
A.B = lm(Y ~ A + B,Salary)
A = lm(Y ~ A,Salary)
B = lm(Y ~ B,Salary)
N = lm(Y ~ 1,Salary)

all.models = list(AB,A.B,A,B,N)
SSE = t(as.matrix(sapply(all.models,function(M) sum(M$residuals^2))))
colnames(SSE) = c("AB","(A+B)","A","B","Empty/Null")
rownames(SSE) = "SSE"
SSE
anova(A.B,AB)

############################################# factor effects##########################

# factor A
anova(B,A.B)
Partial.R2 = function(small.model,big.model){
  SSE1 = sum(small.model$residuals^2)
  SSE2 = sum(big.model$residuals^2)
  PR2 = (SSE1 - SSE2)/SSE1
  return(PR2)
}
Partial.R2(B,A.B)

# factor B effects
anova(A,AB)
Partial.R2(A,A.B)

############################## final model ########################
A.B = lm(Y ~ A+B,Salary)
get.gamma.delta = function(the.model,the.data){
  nt = nrow(the.data)
  a = length(unique(the.data[,2]))
  b = length(unique(the.data[,3]))
  the.data$hat = the.model$fitted.values
  the.ns = find.means(the.data,length)
  a.vals = sort(unique(the.data[,2]))
  b.vals= sort(unique(the.data[,3]))
  muij = matrix(nrow = a, ncol = b)
  rownames(muij) = a.vals
  colnames(muij) = b.vals
  for(i in 1:a){
    for(j in 1:b){
      muij[i,j] = the.data$hat[which(the.data[,2] == a.vals[i] & the.data[,3] == b.vals[j])[1]]
    }
  }
  mi. = rowMeans(muij)  
  m.j = colMeans(muij)
  mu.. = sum(muij)/(a*b)
  gammai = mi. - mu..
  deltaj = m.j - mu..
  gmat = matrix(rep(gammai,b),nrow = a, ncol = b, byrow= FALSE)
  dmat = matrix(rep(deltaj,a),nrow = a, ncol = b,byrow=TRUE)
  gamma.deltaij =round(muij -(mu.. + gmat + dmat),8)
  results = list(Gam = gammai, Del = deltaj, GamDel = gamma.deltaij)
  return(results)
}
Wow = get.gamma.delta(A.B, Salary)
Wow


#######################Diagnostics #############

theS.model = lm(Annual ~ Prof+Region, data = Salary)
Salary$ei = the.model$residuals
nt = nrow(Salary) #Calculates the total sample size
a = length(unique(Salary$Prof)) 
SSE = sum(Salary$ei^2) #Sums and squares the errors (finds SSE)
MSE = SSE/(nt-a) #Finds MSE
eij.star = the.model$residuals/sqrt(MSE)
alpha = 0.05
t.cutoff=qt(1-0.01,nt -a)
t.cutoff= qt(1-alpha/(2*nt), nt-a)
CO.eij = which(abs(eij.star) > t.cutoff)
CO.eij
rij = rstandard(the.model)
CO.rij = which(abs(rij) > t.cutoff)
CO.rij

## normality 
qqnorm(theS.model$residuals)
qqline(theS.model$residuals)

ei = theS.model$residuals
the.SWtest = shapiro.test(ei)
the.SWtest
### constant variance
plot(theS.model$fitted.values, theS.model$residuals, main = "Errors vs. Group Means",xlab = "Group Means",ylab = "Errors",pch = 19)
abline(h = 0,col = "purple")

the.BFtest = leveneTest(ei~ Prof*Region, data=Salary, center=median)
p.val = the.BFtest[[3]][1]
p.val

##################### CIS ########################
#equal weights
sum(the.data$A=="BE"& the.data$B=="SF")
sum(the.data$A=="SE"& the.data$B=="SF")
sum(the.data$A=="DS"& the.data$B=="SF")
sum(the.data$A=="BE"& the.data$B=="S")
sum(the.data$A=="DS"& the.data$B=="S")
sum(the.data$A=="SE"& the.data$B=="S")

all.mult = find.mult(alpha  = 0.05, a = 3, b = 2, dfSSE = nt-a-b+1, g = 3, group = "A")
All.mult


scary.CI = function(the.data,MSE,equal.weights = TRUE,multiplier,group,cs){
  if(sum(cs) != 0 & sum(cs !=0 ) != 1){
    return("Error - you did not input a valid contrast")
  }else{
    the.means = find.means(the.data)
    the.ns =find.means(the.data,length)
    nt = nrow(the.data)
    a = length(unique(the.data[,2]))
    b = length(unique(the.data[,3]))
    if(group =="A"){
      if(equal.weights == TRUE){
        a.means = rowMeans(the.means$AB)
        est = sum(a.means*cs)
        mul = rowSums(1/the.ns$AB)
        SE = sqrt(MSE/b^2 * (sum(cs^2*mul)))
        N = names(a.means)[cs!=0]
        CS = paste("(",cs[cs!=0],")",sep = "")
        fancy = paste(paste(CS,N,sep =""),collapse = "+")
        names(est) = fancy
      } else{
        a.means = the.means$A
        est = sum(a.means*cs)
        SE = sqrt(MSE*sum(cs^2*(1/the.ns$A)))
        N = names(a.means)[cs!=0]
        CS = paste("(",cs[cs!=0],")",sep = "")
        fancy = paste(paste(CS,N,sep =""),collapse = "+")
        names(est) = fancy
      }
    }else if(group == "B"){
      if(equal.weights == TRUE){
        b.means = colMeans(the.means$AB)
        est = sum(b.means*cs)
        mul = colSums(1/the.ns$AB)
        SE = sqrt(MSE/a^2 * (sum(cs^2*mul)))
        N = names(b.means)[cs!=0]
        CS = paste("(",cs[cs!=0],")",sep = "")
        fancy = paste(paste(CS,N,sep =""),collapse = "+")
        names(est) = fancy
      } else{
        b.means = the.means$B
        est = sum(b.means*cs)
        SE = sqrt(MSE*sum(cs^2*(1/the.ns$B)))
        N = names(b.means)[cs!=0]
        CS = paste("(",cs[cs!=0],")",sep = "")
        fancy = paste(paste(CS,N,sep =""),collapse = "+")
        names(est) = fancy
      }
    } else if(group == "AB"){
      est = sum(cs*the.means$AB)
      SE = sqrt(MSE*sum(cs^2/the.ns$AB))
      names(est) = "someAB"
    }
    the.CI = est + c(-1,1)*multiplier*SE
    results = c(est,the.CI)
    names(results) = c(names(est),"lower bound","upper bound")
    return(results)
  }
}
Tuk = find.mult(alpha  = 0.05, a = 3, b = 2, df{SSE} = nt - a*b, g = 3, group = "A")[1]
A.cs.1 = c(1,0,-1)
A.cs.2 = c(1,-1,0)
A.cs.3=c(0,1,-1)
scary.CI(the.data,MSE,equal.weights = TRUE,Tuk,"A",A.cs.1)
scary.CI(the.data,MSE,equal.weights = TRUE,Tuk,"A",A.cs.2)
scary.CI(the.data,MSE,equal.weights = TRUE,Tuk,"A",A.cs.3)
all.mult = find.mult(alpha  = 0.05, a = 3, b = 2, dfSSE = nt-a-b+1, g = 1, group = "B")
all.mult
Bon = find.mult(alpha  = 0.05, a = 3, b = 2, dfSSE = nt - a-b+1, g = 1, group = "B")[1]
A.cs.1 = c(1,-1)
scary.CI(the.data,MSE,equal.weights = TRUE,Bon,"B",A.cs.1)

all.mult = find.mult(alpha  = 0.05, a = 3, b = 2, dfSSE =nt-a-b+1, g = 2, group = "AB")
All.mult
AB.cs = matrix(0,nrow = a, ncol = b)
AB.cs
the.means$AB
AB.cs[3,1] = 1
AB.cs[1,1] = -1/2
AB.cs[2,1]=-1/2
scary.CI(Salary,MSE,equal.weights = TRUE,Bon,"AB",AB.cs)
AB.cs = matrix(0,nrow = a, ncol = b)
AB.cs
the.means$AB
AB.cs[2,2] = 1
AB.cs[1,1] = -1/2
AB.cs[3,1]=-1/2
scary.CI(Salary,MSE,equal.weights = TRUE,Bon,"AB",AB.cs)


names(Salary) = c("Y","A","B")
thebestS.model = lm(Y ~ A+B,data = Salary)
thebestS.model$coefficients

