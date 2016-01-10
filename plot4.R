plot4 <- function()
{
    print ('================================================================================')
    print ('PRODUCING PLOT4.PNG')
    print ('--------------------------------------------------------------------------------')
    # Set English time locale    
    lc_time <- Sys.getlocale("LC_TIME")   
    Sys.setlocale("LC_TIME", "English")
    
    # Get raw data
    raw_data <- get_data("household_power_consumption.txt", 
                         "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip")
    # Select data
    data <- select_data(raw_data, c('1/2/2007', '2/2/2007'))
    

    # Produce graphics
    print ("Producing plot4.png ...")
    png(file = "plot4.png")
    par(mfrow = c(2,2))
    # 1-st plot
    y_label <- "Global Active Power"
    with(data, plot(datetime, Global_active_power, 
                    xlab = "", ylab = y_label, type = "l"))
    # 2-nd plot
    with(data, plot(datetime, Voltage, type = "l"))
    # 3-rd plot
    y_label <- "Energy sub metering"
    columns <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    colors <- c("blue", "red", "blue")
    with(data, plot(datetime, data[[columns[1]]], 
                    xlab = "", ylab = y_label, type = "n"))
    for (i in 1:3)
        with(data, lines(datetime, data[[columns[i]]], col = colors[i]))
    legend("topright", lty=c(1,1), col = colors, legend = columns, bty = "n")
    # 4-th plot
    with(data, plot(datetime, Global_reactive_power, type = "l"))
    # off
    dev.off()
    
    
    # Clean-up
    Sys.setlocale("LC_TIME", lc_time)
    raw_data <- c();    data <- c();
    # Done!
    print("Done!")
    print ('++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++')
}

get_data <- function(filename, file_url)
{
    local_data <- "data.zip"
    if (!file.exists(local_data)){
        # Downloade data file
        print ("Downloading data file ...")
        download.file(file_url, local_data)
    }
    
    # Read data file
    print ("Reading data file (please, wait) ...")
    raw_data <- read.table(unz(local_data, filename), header = TRUE, 
                           sep = ";", dec = ".", na.strings = "?")
    print (paste("rows =", nrow(raw_data), "read."))
    
    # Return raw data
    raw_data
}

select_data <- function(raw_data, dates)
{
    print ("Selecting data ...")
    # Select data
    data <- subset(raw_data, match(Date, dates, nomatch = -1) > 0)
    print (paste("rows =", nrow(data), "selected."))
    
    # Procude datetime column
    data$dt <- paste(data$Date, data$Time)
    data$datetime <- strptime(data$dt, "%d/%m/%Y %H:%M:%S")
    data$dt <- NULL
    
    # Return data
    data
}
