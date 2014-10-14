## Exploratory Data Analysis Oct 2014
## Assignment 1 Plot 2


drawPlot2 <- function() {
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
    PowerConsumption$Global_active_power <- as.numeric(
        as.character(PowerConsumption$Global_active_power))

    ## Draws ---------------------------------------------------------------
    # sets device to a png file
    png(filename="plot2.png", height=480, width=480)
    
    # plots graph. type l draws lines instead of points
    plot(PowerConsumption$DateTime, PowerConsumption$Global_active_power, 
         type="l", 
         ylab = "Global Active Power (kilowatts)", 
         xlab = "")

    dev.off() 
}

# Draw the plot
drawPlot2()
