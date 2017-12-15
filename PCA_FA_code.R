#read the data
x <- read.table("bostonh.dat")

#transform the boston data as following
xt = x
xt[,1]  = log(boston[,1])
xt[,2] = boston[,2]/10
xt[,3]  = log(boston[,3])
xt[,5]  = log(boston[,5])
xt[,6]  = log(boston[,6])
xt[,7] = boston[,7]^2.7/10000
xt[,8]  = log(boston[,8])
xt[,9]  = log(boston[,9])
xt[,10]  = log(boston[,10])
xt[,11] = exp(0.4*boston[,11])/1000
xt[,12] = boston[,12]/100
xt[,13] = sqrt(boston[,13])
xt[,14] = log(boston[,14])

#remove the 4th column
xt <- xt[,-c(4)]
colnames(x) = c("X1", "X2", "X3", "X5", "X6", "X7", "X8", "X9", 
                   "X10", "X11", "X12", "X13", "X14")

#standardize the data
scaled_data <- scale(xt)

#covariance matrix of standardized data
cov <- cov(scaled_data)

#
i = 1:8
for(a in i){
  print(factanal(scaled_data, factors = a, rotation = "none")$PVAL)
}

#factor analysis of 3 factors
load_mat <- factanal(scaled_data, factors = 3, rotation = "none")

#loadings
load_mat$loadings

#uniqueness
load_mat$uniquenesses

#plot
#Factor1 & Factor2
par(mfcol = c(2,2))
plot(load_mat$loadings[,1], load_mat$loadings[,2], type='n', xlab = "Factor1", ylab = "Factor2")
text(load_mat$loadings[,1], load_mat$loadings[,2], colnames(x))
abline(h=0,v=0)
#Factor1 can be categorized as a "quality of life factor" - positively correlated with X1,X3,X5,X10,
#and negatively correlated with X8.

#Factor1 & Factor3
plot(load_mat$loadings[,1], load_mat$loadings[,3], type="n", xlab = "F1", ylab = "F3")
text(load_mat$loadings[,1], load_mat$loadings[,3], colnames(x))
abline(h=0,v=0)
#Factor2 can be interpreted as a "residential factor" - highly correlated with X6, and X13.

#Factor2 & Factor3
plot(load_mat$loadings[,2], load_mat$loadings[,3], type="n", xlab = "Factor2", ylab = "Factor3")
text(load_mat$loadings[,2], load_mat$loadings[,3], colnames(x))
abline(h=0,v=0)
#Factor3 can be interpreted as "employment factor" - highly correlated with X8, X9 and X5.

#-----Varimax rotation------
load_mat_vm = factanal(scaled_data, factors = 3)
#mlv1 = factanal(covmat = covdata, factors = 3, n.obs = 505)
load_mat_vm$loadings
#specific variances
mlv$uniquenesses

#calculation of communality
communalv <- diag(load_mat_vm$loadings%*%t(load_mat_vm$loadings))
communalv

#note the difference with 
varimax(load_mat_vm$loadings)




