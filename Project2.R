setwd("/users/numerp/documents/PGP-BABI/Module 3 Advanced Statistics/Project 2 (AS)")
getwd()
library(dplyr)
library(ggplot2)
library(readr)
library(psych)
library(lattice)
library(mice)
library(car)
library(knitr)
library(visreg)
library(scatterplot3d)
library(corrplot)
library(DataExplorer)
library(nFactors)
project2=read.csv("Factor-Hair-Revised.csv",header = T)
head(project2,10)
tail(project2,5)
names(project2)
str(project2)
summary(project2)
dim(project2)
attach(project2)
boxplot(project2)
any(is.na(project2))
project2a=project2[,c(-1)]
class(project2a)
dim(project2a)
str(project2a)
summary(project2a)
names(project2a)
boxplot(project2a[,1:11])
scatter.hist(project2a)
scatterplot3d(project2a)
cor(project2a[,1:11])
project2a_corr=cor(project2a[,1:11])
project2a_corr
corrplot(project2a_corr,method = "number",type = "upper")
corrplot(project2a_corr,method = "circle",type = "upper")
reg1=lm(Satisfaction~ProdQual)
summary(reg1)
reg2=lm(Satisfaction~Ecom)
summary(reg2)
reg3=lm(Satisfaction~TechSup)
summary(reg3)
reg4=lm(Satisfaction~CompRes)
summary(reg4)
reg5=lm(Satisfaction~Advertising)
summary(reg5)
reg6=lm(Satisfaction~ProdLine)
summary(reg6)
reg7=lm(Satisfaction~SalesFImage)
summary(reg7)
reg8=lm(Satisfaction~ComPricing)
summary(reg8)
reg9=lm(Satisfaction~WartyClaim)
summary(reg9)
reg10=lm(Satisfaction~OrdBilling)
summary(reg10)
reg11=lm(Satisfaction~DelSpeed)
summary(reg11)
reg12=lm(Satisfaction~ProdQual+Ecom+TechSup+CompRes+Advertising+ProdLine+SalesFImage+ComPricing+WartyClaim+OrdBilling+DelSpeed)
summary(reg12)
plot(reg12,col = "blue")
prediction=predict(reg1)
plot(prediction,col = "blue")
lines(prediction,col = "black")
actual=Satisfaction
backtrack=data.frame(actual,prediction)
backtrack
plot(actual,col="orange")
lines(actual,col="black")
pro2=eigen(project2a_corr)
pro2
eigenvalues=pro2$values
eigenvalues
eigenvectors=pro2$vectors
eigenvectors
prop.var=eigenvalues/sum(eigenvalues)*100
prop.var
cumvar=cumsum(prop.var)
cumvar
Factor=c(1:11)
scree=data.frame(Factor,eigenvalues)
plot(scree,main="Scree Plot",col="Green",ylim=c(0,4))
lines(scree,col="green")
plot(eigenvalues,type = "line",xlab = "Principal Components",ylab = "Eigen Values")
unrotate=principal(project2a,nfactors = 4,rotate = "none")
print(unrotate,digits = 3)
unrotatedprofile=plot(unrotate,row.names(unrotate$loadings))
rotate=principal(project2a,nfactors = 4,rotate = "varimax")
print(rotate,digits = 3)
rotatedprofile=plot(rotate,row.names(rotate$loadings),cex=1.0)
rotate$r.scores
rotate$scores
model13=lm(Satisfaction~ProdQual+Ecom+TechSup+CompRes)
summary(model13)
KMO(project2a_corr)
pro2a=eigen(project2a_corr)
pro2a
eigenvalues=pro2a$values
eigenvalues
eigenvectors=pro2a$vectors
eigenvectors
factor.scores(project2a_corr,f = rotate$loadings,method = "Harman")
parallel=fa.parallel(project2a_corr,fm="miners",fa="fa")
solution1=fa(r=project2a_corr,nfactors = 4,rotate = "none",fm = "pa")
solution1
fa.diagram(solution1,simple = F)
solution2=fa(r=project2a_corr,nfactors = 3,rotate = "none",fm = "pa")
solution2
solution1$communality
solution2$communality
fa.diagram(solution2,simple = F)
solution3=fa(r=project2a_corr,nfactors = 3,rotate = "varimax",fm="pa")
solution3
summary(project2a)