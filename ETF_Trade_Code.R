################################
## Set the working directory

## In RStudio use conveniently the menu "Session->Set Working 
## Directory->To Source File Location" 
## In R use only "/" for separating in paths (i.e. no backslash)
setwd("C:/Users/August/Dropbox/2semester/MatematiskStatestik/finans_project")


################################
## Import the data

## Read the finans1_data.csv file containing the data
wr <- read.table("finans1_data.csv", header=TRUE, sep=",", as.is=TRUE)


#############################
## Overview of the data

## Dimension of HE (number of rows and columns)
dim(wr)
## Column names
names(wr)
## The first rows
head(wr)
## The last rows
tail(wr)
## Default summary
summary(wr)
## Another summary function also including the data type
str(wr)
## Can check the amount of cells that are empty or zero 
# by using below commands:
sum(wr == "")
sum(wr == "0")

############################
## Descriptive analysis of selected variables
## b)

### Kunne evt. lave en funktion
plotsETF = function(ETF) {
  par(mrow=(1,1))
  dat = wr$ETF
  hist(dat, prob=FALSE, col="red", nclass=30, main="Emperisk tæthedsplot for ")
  lines(density(dat), col="red", lwd=2)
}


### Udfylder tabellens værdier med nedenstående kommandoer
# For AGG
sum(!is.na(wr$AGG))
mean(wr$AGG)
sd(wr$AGG)
var(wr$AGG)
summary(wr$AGG)
par(mfrow=c(1,2))
# For VAW
sum(!is.na(wr$VAW))
mean(wr$VAW)
var(wr$VAW)
sd(wr$VAW)
summary(wr$VAW)
# For IWN
sum(!is.na(wr$IWN))
mean(wr$IWN)
var(wr$IWN)
sd(wr$IWN)
summary(wr$IWN)
# For SPY
sum(!is.na(wr$SPY))
mean(wr$SPY)
var(wr$SPY)
sd(wr$SPY)
summary(wr$SPY)


### Nu laver jeg Emperiske tæthedsplots for de fire ETF'er ved følgende kode
## AGG
par(mfrow=c(2,2))
hist(wr$AGG, nclass=50, prob=TRUE, xlim=c(-0.15,0.15), col="red", main="Emperisk tæthedsplot for AGG", xlab="Relative ugentlige afkast")
lines(density(wr$AGG), col="black", lwd=3)
abline(v=0, col="black", lwd=1)

## VAW
hist(wr$VAW, nclass=50, prob=TRUE, xlim=c(-0.15,0.15), col="blue", main="Emperisk tæthedsplot for VAW", xlab="Relative ugentlige afkast")
lines(density(wr$VAW), col="black", lwd=3)
abline(v=0, col="black", lwd=3)

## IWN
hist(wr$IWN, nclass=50, prob=TRUE, xlim=c(-0.15,0.15), col="green", main="Emperisk tæthedsplot for IWN", xlab="Relative ugentlige afkast")
lines(density(wr$IWN), col="black", lwd=3)
abline(v=0, col="black", lwd=3)

## SPY
hist(wr$SPY, nclass=50, prob=TRUE, xlim=c(-0.15,0.15), col="yellow", main="Emperisk tæthedsplot for SPY", xlab="Relative ugentlige afkast")
lines(density(wr$SPY), col="black", lwd=3)
abline(v=0, col="black", lwd=3)

### Samt boxplots, som bliver plottet samlet for at gøre sammenligning nemmere.
## AGG, VAW, IWN, SPY
par(mfrow=c(1,1))
boxplot(wr$SPY, wr$IWN, wr$VAW, wr$AGG,  names=c("SPY","IWN","VAW","AGG"), xlab="Relative ugentlige afkast", col=c("yellow","green","blue","red"), main="Boxplot for de fire ETF'er", horizontal=TRUE)

### Udover disse to plotter jeg også den akkumulerede tæthedsfunktion
plot(ecdf(wr$AGG))

text(1, quantile(wr$AGG),c("Minimum","Q1","Median","Q3","Maximum"),col="blue",horizontal=TRUE)



###########################
## d)
## Determination of the correlation between ETFs 
## and determination of portfolio
cov(wr[ ,c("AGG","VAW","IWN","SPY","EWG","EWW")])



##########################
## e)
## Lav portefølje bestående af to ETF'er med mindst varians

### Definerer de stokastiske variable
XEWG = wr$EWG
XEWW = wr$EWW
XAGG = wr$AGG
XSPY = wr$SPY
XVAW = wr$VAW
XIWN = wr$IWN

### Laver en liste med dem alle
c(XEWG, XEWW, XAGG, XSPY, XVAW, XIWN)


### For (EWG,EWW)
### En funktion der angiver variansen af P1 ved variablen alpha (skrevet som a)
variansP1 = function(a){
  return((a^2)*var(XEWG)+((1-a)^2)*var(XEWW)+((2*a*(1-a))*cov(XEWG,XEWW)))
}
### Ved funktionen optimize finder jeg værdien for a hvori variansen bliver mindst.
optimize(variansP1,c(0,1))
### Ved at indsætte den fundne minimumsværdi for alpha kan den reelle stokastiske
### variabel defineres of det forventede ugentlige afkast beregnes
P1 = 0.6445187*XEWG+(1-0.6445187)*XEWW
mean(P1)
sd(P1)
var(P1)

### For (AGG,SPY)
variansP2 = function(a){
  return((a^2)*var(XAGG)+((1-a)^2)*var(XSPY)+((2*a*(1-a))*cov(XAGG,XSPY)))
}
optimize(variansP2,c(0,1))
P2 = 0.9047261*XAGG+(1-0.9047261)*XSPY
mean(P2)
sd(P2)
var(P2)

### For (VAW,IWN)
variansP3 = function(a){
  return((a^2)*var(XVAW)+((1-a)^2)*var(XIWN)+((2*a*(1-a))*cov(XVAW,XIWN)))
}
optimize(variansP3,c(0,1))
P3 = 0.114569*XVAW+(1-0.114569)*XIWN
mean(P3)
sd(P3)
var(P3)

### For (VAW,EWG)
variansP4 = function(a){
  return((a^2)*var(XVAW)+((1-a)^2)*var(XEWG)+((2*a*(1-a))*cov(XVAW,XEWG)))
}
optimize(variansP4,c(0,1))
P4 = 0.6350648*XVAW+(1-0.6350648)*XEWG
mean(P4)
sd(P4)
var(P4)

### For (VAW,EWW)
variansP5 = function(a){
  return((a^2)*var(XVAW)+((1-a)^2)*var(XEWW)+((2*a*(1-a))*cov(XVAW,XEWW)))
}
optimize(variansP5,c(0,1))
P5 = 0.8019989*XVAW+(1-0.8019989)*XEWW
mean(P5)
sd(P5)
var(P5)

### For (IWN,EWG)
variansP6 = function(a){
  return((a^2)*var(XIWN)+((1-a)^2)*var(XEWG)+((2*a*(1-a))*cov(XIWN,XEWG)))
}
optimize(variansP6,c(0,1))
P6 = 0.868502*XIWN+(1-0.868502)*XEWG
mean(P6)
sd(P6)
var(P6)

hist(P1, prob=TRUE, nclass=30)

### Jeg plotter alpha-værdi/varians kurverne for samtlige par
par(mfrow=c(3,2))
curve(variansP1, xname = "alpha", from = 0, to = 1, col="red", lwd=3, main="(EWG, EWW)")
abline(v=0.6445187, col="black", lwd=3)
curve(variansP2, xname = "alpha", from = 0, to = 1, col="red", lwd=3, main="(AGG, SPY)")
abline(v=0.9047261, col="black", lwd=3)
curve(variansP3, xname = "alpha", from = 0, to = 1, col="red", lwd=3, main="(VAW, IWN)")
abline(v=0.114569, col="black", lwd=3)
curve(variansP4, xname = "alpha", from = 0, to = 1, col="red", lwd=3, main="(VAW, EWG)")
abline(v=0.6350648, col="black", lwd=3)
curve(variansP5, xname = "alpha", from = 0, to = 1, col="red", lwd=3, main="(VAW, EWW)")
abline(v=0.8019989, col="black", lwd=3)
curve(variansP6, xname = "alpha", from = 0, to = 1, col="red", lwd=3, main="(IWN, EWG)")
abline(v=0.868502, col="black", lwd=3)

### Jeg plotter de seks portefølgers relative afkast som densitetsfunktion
hist(P1, prob=TRUE, nclass=30,col="red", xlim=c(-0.1,0.1))
lines(density(P1), col="black", lwd=3)
hist(P2, prob=TRUE, nclass=30,col="red", xlim=c(-0.1,0.1))
lines(density(P2), col="black", lwd=3)
hist(P3, prob=TRUE, nclass=30,col="red", xlim=c(-0.1,0.1))
lines(density(P3), col="black", lwd=3)
hist(P4, prob=TRUE, nclass=30,col="red", xlim=c(-0.1,0.1))
lines(density(P4), col="black", lwd=3)
hist(P5, prob=TRUE, nclass=30,col="red", xlim=c(-0.1,0.1))
lines(density(P5), col="black", lwd=3)
hist(P6, prob=TRUE, nclass=30,col="red", xlim=c(-0.1,0.1))
lines(density(P6), col="black", lwd=3)

###########################
## Model validation
## f)
## Validation of a model for AGG
par(mfrow=c(2,2))
title("Validation of normal distribution assumption for ETF", line = -1, outer = TRUE)
qqnorm(wr$AGG, main='AGG',
       xlab='z-scores', ylab='Weekly returns', col="red")
qqline(wr$AGG)
### For VAW
qqnorm(wr$VAW, main='VAW',
       xlab='z-scores', ylab='Weekly returns', col="blue")
qqline(wr$VAW)
### For IWN
qqnorm(wr$IWN, main='IWN',
       xlab='z-scores', ylab='Weekly returns', col="green")
qqline(wr$IWN)
### For SPY
qqnorm(wr$SPY, main='SPY',
       xlab='z-scores', ylab='Weekly returns', col="yellow")
qqline(wr$SPY)


###########################
## Calculations of the 95% confidence intervals
## g)
## t-quantile for the confidence interval for the mean of AGG, 
## since the degrees of freedom for the mean of AGG are 453 
qt(0.975, 453)

## Determination of the confidence interval for the mean parameter in a
## normally distributed random sample

## The 95% confidence interval for AGG
t.test(wr$AGG, conf.level=0.95)$conf.int
### Manuel udregning giver samme resultat
mean(wr$AGG)+1.965215*sd(wr$AGG)/sqrt(length(wr$AGG))
mean(wr$AGG)-1.965215*sd(wr$AGG)/sqrt(length(wr$AGG))
### For VAW
t.test(wr$VAW, conf.level=0.95)$conf.int
mean(wr$VAW)+1.965215*sd(wr$VAW)/sqrt(length(wr$VAW))
mean(wr$VAW)-1.965215*sd(wr$VAW)/sqrt(length(wr$VAW))
### For IWN
t.test(wr$IWN, conf.level=0.95)$conf.int
mean(wr$IWN)+1.965215*sd(wr$IWN)/sqrt(length(wr$IWN))
mean(wr$IWN)-1.965215*sd(wr$IWN)/sqrt(length(wr$IWN))
### For SPY
t.test(wr$SPY, conf.level=0.95)$conf.int
mean(wr$SPY)+1.965215*sd(wr$SPY)/sqrt(length(wr$SPY))
mean(wr$SPY)-1.965215*sd(wr$SPY)/sqrt(length(wr$SPY))

### Nu finder jeg konfidens intervallet for variansen
qchisq(c(0.975,0.025),453)
### For AGG
(453*var(wr$AGG))/513.8655
(453*var(wr$AGG))/395.9219
### For VAW
(453*var(wr$VAW))/513.8655
(453*var(wr$VAW))/395.9219
### For IWN
(453*var(wr$IWN))/513.8655
(453*var(wr$IWN))/395.9219
### For SPY
(453*var(wr$SPY))/513.8655
(453*var(wr$SPY))/395.9219

################################
## h)
### Non-parametric bootstrapping
## Resample many time
k = 100000

simAGG = replicate(k, sample(wr$AGG, replace = TRUE))
## Take the mean for every resample
simMeansAGG = apply(simAGG, 2, mean)
## Take the two quantiles to get the confidence interval
quantile(simMeansAGG, c(0.025,0.975))
### Finder KI for variansen ved at ændre ovenstående
simVarAGG = apply(simAGG, 2, var)
quantile(simVarAGG, c(0.025,0.975))
### For VAW
simVAW = replicate(k, sample(wr$VAW, replace = TRUE))
simMeansVAW = apply(simVAW, 2, mean)
quantile(simMeansVAW, c(0.025,0.975))
simVarVAW = apply(simVAW, 2, var)
quantile(simVarVAW, c(0.025,0.975))
### For IWN
simIWN = replicate(k, sample(wr$IWN, replace = TRUE))
simMeansIWN = apply(simIWN, 2, mean)
quantile(simMeansIWN, c(0.025,0.975))
simVarIWN = apply(simIWN, 2, var)
quantile(simVarIWN, c(0.025,0.975))
### For SPY
simSPY = replicate(k, sample(wr$SPY, replace = TRUE))
simMeansSPY = apply(simSPY, 2, mean)
quantile(simMeansSPY, c(0.025,0.975))
simVarSPY = apply(simSPY, 2, var)
quantile(simVarSPY, c(0.025,0.975))


################################
## i)
### R-funktionen t.test benyttes til at finde p-værdier for ETF'erne
### hvormed nulhypoteset om ingen signifikant forskel på investering
### eller ej kan be- eller afkræftes.
### For AGG
t.test(wr$AGG)
### For VAW
t.test(wr$VAW)
### For IWN
t.test(wr$IWN)
### For SPY
t.test(wr$SPY)

################################
## j)
### ETF (af AGG, VAW, IWN og SPY) med lavest og højest mean findes
mean(wr$AGG)
mean(wr$VAW)
mean(wr$IWN)
mean(wr$SPY)

### Den laveste mean tilhører AGG og den højeste VAW
t.test(wr$VAW,wr$AGG)

################################
## 
## Import data finans2_data.csv
etfSum <- read.table("finans2_data.csv",header=TRUE, sep=",")
str(etfSum)

## k)
## Determine the empirical correlation for the selected variables and
## examine the dependencies
cor(etfSum[,2:7], use="everything", method="pearson")

## First trim the square around the plot. See more on ?par
### For volatilitet og CVaR
par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
par(mfrow=c(2,2))
plot(etfSum$Volatility, etfSum$CVaR, pch=16, cex=0.7, col="red",
     xlab="Volatility [Ugentlig Pct.]",
     ylab="Conditional Value at Risk [Ugentlig Pct.]",  cex.lab=0.8,
     main="Relation mellem Volatility og CVaR", cex.main=0.8)
cor(etfSum$Volatility, etfSum$CVaR)

### Nu laver jeg et tilsvarende plot for Geo.mean og maxTuW
par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
#par(mfrow=c(1,1))
plot(etfSum$Geo.mean, etfSum$maxTuW, pch=16, cex=0.7, col="red",
     xlab="Geometrisk gennemsnit [Ugentlig Pct.]",
     ylab="Maksimum Time under Water [Pct.]",  cex.lab=0.8,
     main="Relation mellem Geo.mean og maxTuW", cex.main=0.8)
cor(etfSum$Geo.mean, etfSum$maxTuW)

### Nu laver jeg et tilsvarende plot for Volatilitet og maxDD
par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
#(mfrow=c(1,1))
plot(etfSum$Volatility, etfSum$maxDD, pch=16, cex=0.7, col="red",
     xlab="Volatility [Ugentlig Pct.]",
     ylab="Maksimum Draw Down [Pct.]",  cex.lab=0.8,
     main="Relation mellem Volatilitet og maxDD", cex.main=0.8)
cor(etfSum$Volatility, etfSum$maxDD)

### Nu laver jeg et tilsvarende plot for maxTuW og Volatilitet
par(mar=c(3,3,2,1),mgp=c(2,0.7,0))
#par(mfrow=c(1,1))
plot(etfSum$maxTuW, etfSum$Volatility, pch=16, cex=0.7, col="red",
     xlab="Maksimum Time under Water [Pct.]",
     ylab="Volitality [Ugentlig Pct.]",  cex.lab=0.8,
     main="Relation mellem maxTuW og Volitality", cex.main=0.8)
cor(etfSum$maxTuW, etfSum$Volatility)


## For calculations of the correlation between Geo.mean and maxTuW
## k)
cov(etfSum$Geo.mean, etfSum$maxTuW)
var(etfSum$Geo.mean)
var(etfSum$maxTuW)
### Korrelationen kan finden ved R-funktion eller 'håndregning', hvilket giver det samme
(cov(etfSum$Geo.mean, etfSum$maxTuW))/(sd(etfSum$Geo.mean)*sd(etfSum$maxTuW))
cor(etfSum$Geo.mean, etfSum$maxTuW)


#########################
## l)
## A simple linear regression
lm1 <- lm(etfSum$Geo.mean~etfSum$VaR, etfSum)
summary(lm1)

#########################
## m)

### Jeg finder nu manuelt først beta1 og så beta0
sum((etfSum$Geo.mean-mean(etfSum$Geo.mean))*(etfSum$VaR-mean(etfSum$VaR)))/(sum((etfSum$VaR-mean(etfSum$VaR))^2))
### Dette giver beta1=0.02352877 hvilket er det samme som lm-funktionens svar
mean(etfSum$Geo.mean)-0.02352877*mean(etfSum$VaR)
### beta0 giver ogsp her det rigtige, 0.1864193

### Jeg finder modellens varians
RSS=sum((etfSum$Geo.mean-(0.1864193+0.02352877*etfSum$VaR))^2)
### RSS=0.510839
RSS/(length(etfSum$Geo.mean)-2)
### Modellens varians er 0.005492893
