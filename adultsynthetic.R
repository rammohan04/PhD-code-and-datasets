install.packages("RJDBC")
library(RJDBC)
library(diffpriv)
adult<-read.csv("adult.csv", header = TRUE)
adult_synthetic<-adult   

print(mean(adult$age))
print(sd(adult$age))
age_syn <- rnorm(32561,38.7,13.01)

print(mean(adult$fnlwgt))
print(sd(adult$fnlwgt))
fnlwgt_syn <- rnorm(32561,189778.9,10555.5)
fnlwgt_syn
print(mean(adult$capitalgain))
print(sd(adult$capitalgain))
capitalgain_syn <- rnorm(32561,1078,7385.8)


print(mean(adult$capital.loss))
print(sd(adult$capital.loss))
capitalloss_syn <- rnorm(32561,87.76,403)


adult_synthetic$age <- age_syn
adult_synthetic$fnlwgt <- fnlwgt_syn
adult_synthetic$capitalgain <- capitalgain_syn
adult_synthetic$capital.loss <- capitalloss_syn

  write.csv(adult_synthetic, "outputdataset.csv", row.names=FALSE)








summary(adult$age,adult$fnlwgt,adult$capitalgain,adult$capital.loss)
summary(adult_synthetic$age,adult_synthetic$fnlwgt,adult_synthetic$capitalgain,adult_synthetic$capital.loss)


f<-function(adult_synthetic) mean(adult_synthetic)
n <- 32561
mechanism <- DPMechLaplace(target = f, sensitivity = 1/n, dims = 1)
D <- runif(n, min = 0, max = 1)
pparams <- DPParamsEps(epsilon = 1)
r <- releaseResponse(mechanism, privacyParams = pparams, X = D)
r


