rawActivity<-read.csv("activity.csv")
summary(rawActivity)
rawActivity$date <- as.Date(rawActivity$date,"%Y-%m-%d")

print(head(rawActivity))

cleanActivity <- rawActivity
c <- complete.cases(rawActivity)
cleanActivity <- rawActivity[c,]

plot(cleanActivity$interval,cleanActivity$steps)

cleanStepsPerDay<-rowsum(cleanActivity$steps,format(cleanActivity$date,"%Y-%m-%d"))
hist(cleanStepsPerDay,breaks=25)
print(nrow(cleanStepsPerDay))
print(mean(cleanStepsPerDay))
print(median(cleanStepsPerDay))
meanTotalStepsPerDay <- mean(cleanStepsPerDay)
medianTotalStepsPerDay <- median(cleanStepsPerDay)

print(head(cleanActivity))

# avgStepsPerInterval<-by(data = cleanActivity$steps,cleanActivity$interval,mean)
avgStepsPerInterval<-aggregate(steps ~ interval, data=cleanActivity, FUN=mean)

plot(avgStepsPerInterval)
maxSteps<-subset(avgStepsPerInterval,avgStepsPerInterval==max(avgStepsPerInterval))
print(maxSteps)

sum(is.na(rawActivity[,'steps']))

imputedActivity <- rawActivity

numFixed<-0
for(i in 1:nrow(rawActivity)) {
    if(is.na(rawActivity[i,'steps'])) {
        interval<-activity[i,'interval']
        imputedActivity[i,'steps'] <- avgStepsPerInterval[avgStepsPerInterval$interval==interval,'steps']
#        imputedActivity[i,'steps'] <- 9000
        numFixed<-numFixed+1
    }
}

print(numFixed)

avgStepsPerInterval<-aggregate(steps ~ interval, data=imputedActivity, FUN=mean)

print(head(imputedActivity))
imputedStepsPerDay<-rowsum(imputedActivity$steps,format(imputedActivity$date,"%Y-%m-%d"))

hist(imputedStepsPerDay,breaks=25)

print(nrow(imputedStepsPerDay))

print(mean(imputedStepsPerDay))
print(median(imputedStepsPerDay))
print(summary(imputedStepsPerDay))

weekend<-c("Saturday","Sunday")

isWeekend <- weekdays(imputedActivity$date) %in% weekend
imputedActivity$weekday<-factor(isWeekend,c(FALSE,TRUE),labels=c("weekday","weekend"))
library(lattice)
avgStepsPerInterval<-aggregate(steps ~ interval+weekday , data=imputedActivity, FUN=mean)
panel<-xyplot(steps ~ interval | weekday, data=avgStepsPerInterval, layout=c(1,2),type='l')
print(panel)


