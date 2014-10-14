## Exploratory Data Analysis Oct 2014
## Assignment 1 Plot 3


drawPlot3 <- function() {
    ## ---------------------------------------------------------------
    ## Preparation steps
    
    fileName <- "household_power_consumption.txt"
    filePath <- paste("./", fileName, sep="")
    
    # Check if the required files exists
    if (!file.exists(filePath)) {
        stop("Check if the data is in your working directory.")
    }
    
    ## ---------------------------------------------------------------
    ## Loads and prepares electic power comsumption dataset
    
    PowerConsumption <- read.table(filePath,
                         header=TRUE, sep=";")
    
    # Converts Date column to date format
    
    PowerConsumption$DateTime <- strptime(paste(PowerConsumption$Date, 
                                                PowerConsumption$Time)
                                    , "%d/%m/%Y %H:%M:%S")
    
    # subsets data frame keeping only records in date interval
    library(plyr)
    PowerConsumption <- subset(PowerConsumption, as.Date(PowerConsumption$DateTime) >= as.Date("2007-02-01")
                               & as.Date(PowerConsumption$DateTime) <= as.Date("2007-02-02") )
    
    # Converts column to numeric
    PowerConsumption$Sub_metering_1 <- as.numeric(
        as.character(PowerConsumption$Sub_metering_1))
    PowerConsumption$Sub_metering_2 <- as.numeric(
        as.character(PowerConsumption$Sub_metering_2))
    PowerConsumption$Sub_metering_3 <- as.numeric(
        as.character(PowerConsumption$Sub_metering_3))

    ## Plots graph to a PNG file
    
    # sets file
    png(filename="plot3.png", height=480, width=480)
    
    # sets font size
    par(ps = 11, cex = 1, cex.main = 1)
    
    
    plot(PowerConsumption$DateTime, PowerConsumption$Sub_metering_1, 
         type="l", 
         ylab = "Energy sub metering", 
         xlab = "")
    lines(PowerConsumption$DateTime, PowerConsumption$Sub_metering_2, 
         col="red" )
    lines(PowerConsumption$DateTime, PowerConsumption$Sub_metering_3, 
          col="blue" )
    
    legend("topright", c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
           lty=c(1,1,1), # gives the legend appropriate symbols (lines)
           col=c("black","red","blue")) # gives the legend lines the correct color and width
    
    dev.off() 
    
}

# Draw the plot
drawPlot3()
