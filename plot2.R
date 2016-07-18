#clearworkspace
rm(list = ls())
#Loading the data
#Load Libraries
library(dplyr)
library(lubridate)

# Store filename
file <- "household_power_consumption.txt"
#read data file into my_tbl
my_tbl <- read.table(file, header = TRUE, sep = ";", 
                     colClasses = c("character","character", "numeric" , "numeric" , "numeric" , "numeric" , "numeric" , "numeric"),
                     na.strings = "?", 
                     comment.char = "")
#filter based on dates needed and store as df
small_df <- filter(my_tbl, 
                   Date == "1/2/2007" |
                     Date == "2/2/2007") 
#create month and day columns containing month and day 
#from Date variable
small_df <- mutate(small_df,
                   month = month(dmy(small_df$Date),
                                 label = TRUE, abbr = TRUE),
                   day  =  wday(dmy(small_df$Date), 
                                label = TRUE, abbr = TRUE)
)
#clean up workspace remove unused data frame
rm(my_tbl)


#Line plot
#y var"Global Active Power (kilowatts)"
# x var " Time as dermined by day of the week
library(lubridate)

#create variable timestamp 
#combining Date and time and converting to date
#format
small_df2 <-mutate (small_df, 
                    timestamp = parse_date_time(paste(small_df$Date, small_df$Time),
                                orders = "%d%m%y HMS"))
png(filename = "plot2.png", 
    width = 480, 
    height = 480, 
    units = "px"
    )
plot(small_df2$timestamp, small_df2$Global_active_power, 
     xlab = "",
     ylab = "Global Active Power (kilowatts)", 
     type = "l")
dev.off()



