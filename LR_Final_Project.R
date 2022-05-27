# load Library
library(readxl)
getwd()

# dataset reading

csv_data<-read.csv("insurance.csv")
head(csv_data)
nrow(csv_data)

#Security Description - Descriptive Statistics
install.packages("pastecs")
library(pastecs)
stat.desc(csv_data)
stat.desc(y)


# regression model
y<-csv_data$charges # set up dependent variable

# check the structure of the data
str(csv_data)

# change the categorical variable
csv_data$smoker=as.factor(csv_data$smoker)
csv_data$sex=as.factor(csv_data$sex)
csv_data$children=as.factor(csv_data$children)
csv_data$region=as.factor(csv_data$region)
str(csv_data)

# fit the model
m1<-lm(y~csv_data$age+csv_data$bmi+csv_data$smoker+csv_data$sex+csv_data$children+csv_data$region)

summary(m1)
# regression equation is (charges= -11927.17 + 257.19(age) + 336.91(bmi) + 23836(smoker-yes)+1635.78(children2))
coefficients(m1)

anova(m1)

                #Assumptions

par(mfrow=c(2,2))
plot(m1)
plot(m1,1) #linear
plot(m1,2) #normality
plot(m1,3) #homo
               #removing hetero
m2<-lm(log(y)~csv_data$age+csv_data$bmi+csv_data$smoker+csv_data$children)
summary(m2)

#new regression equation(log(charges)= 6.9692685+0.0348147*age+0.0104155*bmi+
#(1.5437176)smoker+children1*0.1423780+children2*0.2792741+children3*0.2494404+
#(children4*0.5197718)+children5*0.3979236)
par(mfrow=c(2,2))
plot(m2)
plot(m2,3)


                   #hypothesis Ftest
confint(m2)
length(y)
qt(.975,1333)
install.packages("car")
library("car")
linearHypothesis(m2,c("csv_data$age=0", "csv_data$bmi=0"))
#reject H0 null hypothesis because f-test value is more than critical value 

                   #multicolinearity

x<-cbind(csv_data$age,csv_data$bmi,csv_data$smoker,csv_data$children)
cor(x)
pairs(x)
vif(m2)
sqrt(vif(m2))>2
#vif value is near 1 and by seeing cor() we can say that there is no multilinearity


                    #infuential points

#leverages
m2<-lm(log(y)~age+bmi+smoker+children,data=csv_data)
standard_res <- rstandard(m2)
standard_res
lev<-hatvalues(m2)>3*mean(hatvalues(m2))
csv_data[lev, ]
length(csv_data[lev, ])

#outliers
     #studentized

out <- rstudent(m2) > 3
csv_data[out, ]

     
     # Calculating DFFITS:
df <- m2$df.residual
p <- length(m2$coefficients)
n <- nrow(m2$model)
dffits_crit = 2 * sqrt((p + 1) / (n - p - 1))
data_dffits <- dffits(m2)
csv_data[which(abs(data_dffits) > dffits_crit),]

    # Calculating Cook's distances.
cooks_crit = 0.5
data_cooks <- cooks.distance(m2)
View(data_cooks)
csv_data[which(abs(data_cooks) > cooks_crit),]



                     #prediction
fitted<-fitted.values(m2)
head(fitted)# for given data

#for  new data
m2<-lm(log(y)~age+bmi+smoker+children,data=csv_data)
newdata<-data.frame(age=27,bmi=35.06,smoker="yes",children="3")
predict(m2,newdata)
z<-data.frame(age=c(35,25,36,48),
              bmi=c(22.09,28.9,32.6,35),
              smoker=c("yes","no","yes","yes"),
              children=c("2","4","3","3"))
View(z)
predict(m2,z)

# 19female 27.900 0 yes southwest 16884.924
newdata1<-data.frame(age=19, bmi=27.900, smoker="yes",children="3")
predict(m2,newdata1)

