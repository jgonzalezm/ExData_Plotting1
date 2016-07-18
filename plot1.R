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

#Creating and saving plot 1
#Histogram - Global Active Power (kilowatts) 
#title Global Active Power

# create png "plot1.png
png(filename = "plot1.png", width = 480, 
    height = 480, 
    units = "px", bg = "white")

#plot histogram
hist(x = small_df$Global_active_power, # plot histogram of Global active Power frequencies
     freq = TRUE,# show frequency count not proportion
     xlab = "Global Active Power (kilowatts)", # x axis label
     col = "red", # color bars red
     main = "Global Active Power"  # graph title
     ) 
#close device (plot1.png)
dev.off()
