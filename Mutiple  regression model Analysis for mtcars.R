# Multiple  Regression 
#Basically Multiple Regression analysis is used when we want to predict the Dependent variable based on more than one predictors
# lets say If we want to see the sales value of a grocery by the two use of two preditors like advertisement on digital media and advertisement on print media
# here we will get the sales value  based on the effects of both types of advertisement combined
# whereas in Simple Linear regression we used to have only one predictor and its effect was shown in the dependent variable.
# in Multiple Variable we will have combined effects of 2 or more independent variables x1,x2...xk on the single Dependent variable y
# As the equation of simple regression was  yhat = b0 + b1 + E  
## here b0 is sample intercept & b1 is sample slope/ Regression Coefficient in context to the population
# the equation of Multiple Linear regression will be    yhat= b0 + b1x1 + b2x2+ b3x3 +.........+bkxk    here we call 
# b0 as Regression constant/y intercept when all the xi's are zero 
# b1,b2,b3,......bk  are known as partial value of Regression coeffiients of x1,x2,x3,....xk respectively
 # there are same  assumption held , With Multiple Analysis whih are as they were with the linear Regression model


# We need to have  ar package installed to use for values like Darson Watson
install.packages(cars)
library(cars)
#Now let us import the  mtcars.csv file into the R enviromnet and build a multiple regression models with 4 predictors
# the 4 preditors are disp,hp,wt,drat
# we can build the same model by two ways as shown below
# fit & ki models are same models defined by two different ways

fit<- lm(mpg ~ disp+hp+wt+drat , data=mtcars)
fit
summary(fit)

# or

ki<- lm(mtcars$mpg ~ mtcars$disp+mtcars$hp+mtcars$wt+mtcars$drat)
ki
summary(ki)

# the summary(fit) gives us the t value and Pr(>|t|) which test for the null hypothesis of the slope of the eah variable in front of them
# as we know if p value is > alpha the null hypothesis is accepted and if p value < alpha null hypothesis rejected
# Ho: is that there is no significant relationship or there is no linear relationship #
#Ha: there is significant relationship or there is linear relationship

# as we can see that p value for disp & drat > alpha hence for them H0 is aepted and they are not good predictors of mpg 
# where as wt & hp have p value < alpha , for them H0 is rejeted and ha accepted implying that there is significant relatioship b/w them and mpg
# it means hp & wt are good predictors of mpg in mtcars 

# Also multiple r square here is 0.8376 which implies that 83.76% of the variane is explained by these 4 preditors colletively
# Multiple R square is the most powerful term in this hole result which tells us relationship between the predictors & the  how the data is spread
# we know R^2 = SSR/SST which can be written as  1- SSE/SST ,
# as we have used preditors so we need to adjust the R square value and find it by using fifferent formula as below
# Adjusted R square = 1 - (( SSE/n-k-1)/(SSE/n-1) ) 
# hene there is smal hange in value of Rsquared and multiple R squared
# also  [Correlation()]^2  = R^2

# we an also take adjusted R squared value as the best value tat will explain about the model very well in exchange with Multiple R squared value

#higher the multiple R squared better the model

# as we understood disp and drat are not good predictors we need to drop them from the model and build new model with hp & wt

ti<-lm(mpg~hp+wt,data=mtcars)
ti
#gives us the equation  of the multiple regression model directly by giving the regression constant & regression coefficients 
summary(ti)
# for this model adjusted  R square value is .8148% whih means 81.48% variane in mpg is explained by these two variables only
#hence they both are good predictors of  mpg
# the multiple R square for both the models i.e. fit and ti are amost same

# which means that our final model should be based on these 2 variables hp & wt ~ 
# rather than 4 variables (disp drat hp & wt )colletively

# there is a thumb rule .. Rule of Parsimony 
# it states that number of predicctors as less as possible with maximum Adjusted r square value

# now let us build model with only hp & only wt as the predictors for mps in mtcars
#model with hp as only predictor
hi<-lm(mpg~hp,data=mtcars)
hi
summary(hi)

#model with wt as only predictor
fi<-lm(mpg~wt,data=mtcars)
fi
summary(fi)


# out of these four models the model 'hi' is the worst model as it has the least r^2 value 
# model 'ti' is the best model as it has the best r^2 value with least no. of predictors as the rule says we 
#should try to have minimum numbers of predictors without much of compromise which is true in this model when ompared with the fit model

# hence we can say that 'ti' is the best of the 4 model present

# now checking Multicollinearity
vif(fit)
vif(fit)>5
# VIFi =  1/(1-Ri^2)  is the formula for finding Vif for a particular model with 2 or more predictors


# we can calculate DWS which is durbin WaTSON STATISTIS  USING THE eXCEL FORMULA OF  
#dws=  SUMMATION(Ei - E(i-1))^2/ Summation Ei^2 
#based on the degree of freedom and no of predictors we will get Dl & Du from the KEN BLAcK TABLE of DWS AND 
## THROUGH that we can make our scale whih is from 0 to 4   mid line is 2 and we will then insert the values  dl & du in the sale
# after inserting values on both sides of two we will desribe the +ve or -ve auto ccorrelation are/No autoccorrelation area/Indicisive region in the scale
# based on he region of the scale we will deide whether to go for Regression model or not
# if the DWS value whih we find out in 95 line is in No autoorrelatin zone-> we an move further
# if in Indicisive zone we will leave it on researher as benefit of doubt goes wwith researher. We an acept in thisregion even
## but if it is in +ve or -ve autoorrelation zone we will not consider making a linear regression model infact we will shift to our Time analysis prediction Model

#DWS is caculated for the model based on the Response variable and the Preditors

# # DWS is basically used to hek whether the next values in that variable are related to the previous value of the same variable ex ei is obtained by e(i-1)
# if yes either +ve or -ve autocorrelation, we do it by hecking DWS value of that particular model then we have to leave regression model and move to Time series 
# r ommand is  dwt(model name)
 dwt(hi)

#this dws value will give us autocorrelation in the predictors or in errors to say that next o/p is related with the previous one 
 # hence to understand this is important
 # DWS value lying bw 1.5 & 2 is ver common
