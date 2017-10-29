## ---- totalSteps
#Reads the data
data = read.csv('activity.csv')
data$date = as.Date(data$date, "%Y-%m-%d")
#Splits the data per date
splitDay = split(data, data$date)
#Total number of steps/Median per day
totalSteps = sapply(splitDay, function(x) sum(x$steps))
totalSteps

## ---- totalStepsHist
#Reads the data
data = read.csv('activity.csv')
data$date = as.Date(data$date, "%Y-%m-%d")
#Splits the data per date
splitDay = split(data, data$date)
#Total number of steps/Median per day
totalSteps = sapply(splitDay, function(x) sum(x$steps))
hist(totalSteps,main="Histogram of Frequency of TotalSteps/Day")

## ---- meanSteps
#Reads the data
data = read.csv('activity.csv')
data$date = as.Date(data$date, "%Y-%m-%d")
#Splits the data per date
splitDay = split(data, data$date)
#Total number of steps/Median per day
meanSteps = sapply(splitDay, function(x) mean(x$steps))
meanSteps

## ---- medianSteps
#Reads the data
data = read.csv('activity.csv')
data$date = as.Date(data$date, "%Y-%m-%d")
#Splits the data per date
splitDay = split(data, data$date)
#Total number of steps/Median per day
medianSteps = sapply(splitDay, function(x) median(x$steps))
medianSteps

## ---- timeseriesplot
#Reads the data
data = read.csv('activity.csv')
interval = seq(0,2355,5)
steps = numeric()
for(i in 1:472) {
  steps[i] = 0
}
avgSteps = data.frame(interval, steps)
names(avgSteps) = c('interval','avgsteps')
for (i in 1:472){
  avgSteps$avgsteps[i] = mean(data$steps[data$interval == interval[i]], na.rm=TRUE)
}
plot(avgSteps, type='l', main='Avg. Steps over 5 minute intervals',xlab='Interval(minutes)',
     ylab='Average Steps')
avgStepsinterval =avgSteps$interval[which.max(avgSteps$avgsteps)]
avgStepsinterval

## ---- numberofmissingvalues
data = read.csv('activity.csv')
numNa = sum(is.na(data$steps))

## ---- strategy
data = read.csv('activity.csv')
noNa = data
noNa[is.na(noNa)]=0
splitNa = split(noNa, noNa$date)
totalNa = sapply(splitNa, function(x) sum(x$steps))
totalNa
hist(totalNa,main="Histogram of Frequency of TotalSteps/Day", xlab="Total number of Steps")

## ----- mean
avgNa = sapply(splitNa, function(x) mean(x$steps))
avgNa

## ---- median
medNa = sapply(splitNa, function(x) median(x$steps))
medNa

## ---- Activitypattern
#Shows data of weekday and weekend values
data = read.csv('activity.csv')
activityFilled = data
activityFilled[is.na(noNa)]=0
activityFilled$day=ifelse(as.POSIXlt(as.Date(activityFilled$date))$wday%%6==0,
                          "weekend","weekday")
# For Sunday and Saturday : weekend, Other days : weekday 
activityFilled$day=factor(activityFilled$day,levels=c("weekday","weekend"))
stepsInterval2=aggregate(steps~interval+day,activityFilled,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=stepsInterval2,aspect=1/2,type="l")

