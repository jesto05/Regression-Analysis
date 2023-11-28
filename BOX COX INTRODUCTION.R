#Box Cox Transformation

rand=rexp(100,2)
rand

hist(rand)

#we require mass package to run box cox transformation
install.packages("MASS")
library("MASS")

md=lm(rand~1) #there is no explanatory varibale only the intercept

#checking for normality assumptions
plot(md,which=2)
#fromt he graph we see that errors does not follow normal
#distribution since not coincing perfectlywith the regression line

#shapiro test for numerical test of normality
#N0:Errors follow normal distribution
shapiro.test(md$residuals)
#we reject null hypothesis at 5% level of significance since p<0.05

#boxcox
bx=boxcox(md,lambda = seq(-2,2))

#finding the lambda value
#for the maximum y value i.e log likehood value, we find the corresponding x
#value which is the value for lambda
lmda=bx$x[which.max(bx$y)]
lmda  #0.34
#Here lambda is not zero so we use the transformation y^(lmda)= (y^lmda) -1/lmda,
#here y=rand
#If lambda=0 we have the formula y^(lmda)= log y

#Tranformation
ylmd=((rand^lmda)-1)/lmda
new_md=lm(ylmd~1)

#Again checking normality for new model
plot(new_md,which = 2)
#we see the observations coincide with the regresion line

#also checking using shapiro test
#N0:Errors follow normal distribution

shapiro.test(new_md$residuals)
#we see p value>0.05 , hence we accept the null hypothesis

#########################################################
#Checking using other methods

#for boxcox using Car package
install.packages("car")
library("car")

powerTransform(md,family="bcPower")
#here bcPower is Box Cox Power Transformation
#estimatedtranformation parameter is 0.28639

#for boxcox using AID package
install.packages("AID")
library("AID")

#boxcoxnc mean box cox tranformation for normality of a variable

#we give input as rand
#sw is shapiro wilk method
boxcoxnc(rand,method = "sw",lambda = seq(-3,3,0.01),lambda2=NULL,plot=FALSE,alpha =0.05)
#we get lambda= 0.31
#i.e if we take lambda=0.31 we will get the data as normal i.e p>0.05

#different method "ad"
boxcoxnc(rand,method = "ad",lambda = seq(-3,3,0.01),lambda2=NULL,plot=TRUE,alpha =0.05)
#lambda=0.35