#Data read-in
library(lubridate)
elen <- read.csv("household_power_consumption.txt", sep=";", dec = ".", header = TRUE, stringsAsFactors = FALSE)
elen$Date <- dmy(elen$Date)
elen <- subset(elen, elen$Date >= "2007-02-01" & elen$Date <= "2007-02-02")
elen$Global_active_power <- as.numeric(as.character(elen$Global_active_power))
elen$DateTime <- with(elen, as.POSIXct(paste(elen$Date, elen$Time)))
#elens2$Global_active_power <- as.numeric(as.character(elens2$Global_active_power))

#Plot1
hist(elen$Global_active_power, col="red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.copy(png,'plot1.png', width = 480, height = 480, units = "px")
dev.off()

#Plot2
plot(elen$Global_active_power~elen$DateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.copy(png,'plot2.png', width = 480, height = 480, units = "px")
dev.off()

#Plot3
with(elen, {
  plot(Sub_metering_1~DateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="", cex=0.8)
  lines(Sub_metering_2~DateTime, col='Red')
  lines(Sub_metering_3~DateTime, col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=1, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex=0.8)

dev.copy(png,'plot3.png', width = 480, height = 480, units = "px")
dev.off()

#Plot4
par(mfrow=c(2,2))
plot(elen$Global_active_power~elen$DateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
plot(elen$DateTime, elen$Voltage ,type="l", ylab="Voltage", xlab = "datetime")
with(elen, {
  plot(Sub_metering_1~DateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="", cex=0.8)
  lines(Sub_metering_2~DateTime, col='Red')
  lines(Sub_metering_3~DateTime, col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=1, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex=0.8)
plot(elen$DateTime, elen$Global_reactive_power ,type="l", xlab = "datetime")
dev.copy(png,'plot4.png', width = 480, height = 480, units = "px")
dev.off()
