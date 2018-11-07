#loading the data
library(ggplot2)
library(plyr)
library(lattice)

activity <- read.csv("activity.csv")

#Processing the Data
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime <- as.POSIXct(activity$date, format="%Y-%m-%d")
#taking away the NA values
clean <- activity[!is.na(activity$steps),]

#---------------------------------
#The mean total number of steps taken per day
table <- aggregate(activity$steps ~ activity$date, FUN=sum)
colnames(table) <- c("Date", "Steps")

#We create the histogram
hist(table$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day")

#calcuate the mean of steps
print(as.integer(mean(table$Steps)))

#calculate the median of steps
print(as.integer(median(table$Steps)))

#---------------------------------
#Average daily activity pattern
#we reuse the variable clean
table <- ddply(clean, .(interval), summarize, Avg=mean(steps))

#we create a line plot with the number of steps per interval
p <- ggplot(table, aes(x=interval, y=Avg), xlab="Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

#maximum steps by interval
max <- max(table$Avg)

#maximum average number of steps
print(table[table$Avg == max, 1])

#---------------------------------
#Imputing missing values
#number of NAs in the original data set
print(nrow(activity) - nrow(clean))

#Create the average number of steps per weekday and interval
table <- ddply(clean, .(interval, day), summarize, Avg=mean(steps))

#Dataset with all NAs
nadata <- activity[is.na(activity$steps),]

#merge na data with average weekday interval for substitution
newdata <- merge(nadata, table, by=c("interval", "day"))

#reorder the data in order to have the same format as the clean data
newdataTwo <- newdata[,c(6,4,1,2,5)]
colnames(newdataTwo) <- c("steps", "date", "interval", "day", "DateTime")

#merge the NA data (newdata) + non NA data (clean)
mergeVar <- rbind(clean, newdataTwo)

#Create sum of steps per date to compare with step 1
sumTable <- aggregate(mergeVar$steps ~ mergeVar$date, FUN=sum)
tableSum <- aggregate(activity$steps ~ activity$date, FUN=sum)
colnames(sumTable) <- c("Date", "Steps")

# mean
print(as.integer(mean(sumTable$Steps)))

#median
print(as.integer(median(sumTable$Steps)))

#Create the histogram displaying the total steps per day
hist(sumTable$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day with NAs Fixed", col = "Black")
hist(tableSum$Steps, breaks = 5, xlab = "Steps", main = "Total Steps per Day with NAs Fixed", col = "Grey", add = T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill = c("black", "grey"))

#---------------------------------
#Activity Patterns Between Weekdays and Weekends
mergeVar$DayCategory <- ifelse(mergeVar$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

#summarize data by interval and type o the day
interval <- ddply(mergeVar, .(interval, DayCategory), summarize, Avg=mean(steps))

#Plot
xyplot(Avg~interval|DayCategory, data=interval, type="l", layout = c(1,2),
       main="Average Steps per Interval Based on Type of Day",
       ylab="Average Number of Steps", xlab="Interval")