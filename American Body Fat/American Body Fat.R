Fat <- read.csv("/Users/zhuoyujiang/Downloads/BodyFat.csv") 
BodyFat<- read.csv("/Users/zhuoyujiang/Downloads/BodyFat.csv") 
library(car)
BODYFAT = Fat$BODYFAT #converts each predictor into format easier to create models from
DENSITY = Fat$DENSITY
WEIGHT = Fat$WEIGHT
HEIGHT = Fat$HEIGHT
ADIPOSITY = Fat$ADIPOSITY
AGE = Fat$AGE
NECK = Fat$NECK
CHEST = Fat$CHEST
ABDOMEN = Fat$ABDOMEN
HIP = Fat$HIP
THIGH = Fat$THIGH
KNEE = Fat$KNEE
ANKLE = Fat$ANKLE
BICEPS = Fat$BICEPS
FOREARM = Fat$FOREARM
WRIST = Fat$WRIST

#We saw that the max weight is extremely high, so we want to check the ranked weight
v = order (WEIGHT, decreasing = TRUE)
weight.order = WEIGHT[v]
weight.order
a = order (HEIGHT, decreasing = FALSE)
height.order = HEIGHT[a]
height.order
#The largest weight is an extreme data, so we delete that. The smallest height is an extreme data, so we delete that.
index.1 = which(WEIGHT> 300)   
index.1
index.2 = which(HEIGHT<50)
index.2
#We can see the two data are also extreme in some other attributes. So, we delete the two data.
BodyFat[c(index.1, index.2),]
BodyFat.1 = BodyFat[-c(index.1, index.2),]
BodyFat.1
BodyFat.1[c(index.1, index.2),]
#We deleted the extreme data right now.
#Based on background information, if we know density, it's easy to get the body fat rate. But, it is not easy to get the density. So, we should exclude density.
#We should choose some easily measured data, all of the circumferences should be excluded. Also, the ID number is useless.
#So, combine with the background, we keep age, weight, height and ADIPOSITY
#We finish the first step of analyzing data.

par(mfrow=c(2,3)) #Makes a two-by-two, i.e. (2,2), plotting window
par(mgp=c(1.8,.5,0), mar=c(3,3,1,1)) #"Beautifies" plots when creating multiple figures. Google this for more info.
hist(BodyFat.1$BODYFAT,breaks=30,cex.lab=1.5,cex.main=1.6,
     main="Histogram of Body Fat %",xlim = c(0,50), ylim = c(0,35),xlab="Body Fat %")
hist(BodyFat.1$AGE,breaks=30,cex.lab=1.5,cex.main=1.6,
     main="Histogram of Age",xlim = c(20,90), ylim = c(0,25),xlab="Age (yrs)")
hist(BodyFat.1$WEIGHT,breaks=30,cex.lab=1.5,cex.main=1.6,
     main="Histogram of Weight",xlim = c(100,300), ylim = c(0,40),xlab="Weight (lbs)")
hist(BodyFat.1$HEIGHT,breaks=30,cex.lab=1.5,cex.main=1.6,
     main="Histogram of height",xlim = c(60,80), ylim = c(0,40),xlab="Height (cms)")
hist(BodyFat.1$ADIPOSITY,breaks=30,cex.lab=1.5,cex.main=1.6,
     main="Histogram of adiposity",xlim = c(15,40), ylim = c(0,30),xlab="Adiposity (bmi)")
BodyFat.2 = BodyFat.1[,-c(1,3,8,9,10,11,12,13,14,15,16,17,18)]
FM_1 = lm(BODYFAT ~ AGE + ADIPOSITY, data = BodyFat.2)
summary(FM_1)


#Our MLR model
FM_1 = lm(BODYFAT ~ AGE + ADIPOSITY, data = BodyFat.2)
Rsq = summary(FM_1)$adj.r.squared
Coefficients=FM_1$coefficients
out.FM_1 = data.frame( Coefficients=Coefficients, AsjustedRsqure = Rsq)
out.FM_1
#Adjusted R^2 = 0.5930276> 0.5911449. It's better than the 4 predictor-model's Adjusted R^2.
#Our model should be BodyFat= 0.1215295*AGE + 1.6603456*ADIPOSITY - 28.6753165	
```

```{r}
# 95% confidence interval
predict(FM_1, newdata=data.frame(AGE=23, ADIPOSITY = 31.2),interval="confidence",alpha=0.05)
# 95% prediction interval
predict(FM_1, newdata=data.frame(AGE=23, ADIPOSITY = 31.2),interval="prediction",alpha=0.05)
#I used my age and BMI to test, the fit estimated body fat is 25.92, my true body fat is 26.8, which is within the confidence interval and prediction interval.
```

#Since two predictors are all important for the model, we choose to use age and adiposity to build the model.
#3D plot when we use age and adiposity as predictors.
library(scatterplot3d)
s3d = scatterplot3d(BodyFat.1$AGE,BodyFat.1$ADIPOSITY,BodyFat.1$BODYFAT, pch=19,main="3D Scatterplot of Age (X), Adiposity (Y), and Body fat (Z)", xlab="Age (yrs)",ylab ="Adiposity (bmi)",zlab ="Body Fat %")
dims <- par("usr")
x <- dims[1]+0.9*diff(dims[1:2])
y <- dims[3]+0.08*diff(dims[3:4])
fit <- lm(BODYFAT ~ AGE+ADIPOSITY, data = BodyFat.2)
s3d$plane3d(fit)
```

```{r}
#Our MLR model
FM_1 = lm(BODYFAT ~ AGE + ADIPOSITY, data = BodyFat.2)
Rsq = summary(FM_1)$adj.r.squared
Coefficients=FM_1$coefficients
out.FM_1 = data.frame( Coefficients=Coefficients, AsjustedRsqure = Rsq)
out.FM_1
#Adjusted R^2 = 0.5930276> 0.5911449. It's 

