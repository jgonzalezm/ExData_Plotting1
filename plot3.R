#clearworkspace
rm(list = ls())
#Loading the data
#Load Libraries
library(dplyr)
library(lubridate)
library(tidyr)

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

#create variable timestamp 
#combining Date and time and converting to date
#format
small_df2 <- mutate(small_df, 
                    timestamp = parse_date_time(
                              paste(small_df$Date, small_df$Time),
                    orders = "%d%m%y HMS")
                    )
rm(small_df)
#choose only columns needed
df3 <- select(small_df2, Sub_metering_1:Sub_metering_3, timestamp)
rm(small_df2)
#tidy df3 by 
df3 <- gather(df3, 
              key = sub_metering, 
              value = measurement, 
              Sub_metering_1:Sub_metering_3,
              -timestamp)
df3 <- mutate(df3, 
              sub_metering = extract_numeric(sub_metering))
# create png "plot3.png
png(filename = "plot3.png", width = 480, 
    height = 480, 
    units = "px", bg = "white")

with(df3,
     plot(timestamp, measurement, #col = as.character(sub_metering),
          #xlab = "", ylab = "Global Active Power (kilowatts)", 
          type = "n" 
          )
    )
with(subset(df3, sub_metering == 1), 
     lines(timestamp, measurement, 
            type = "l",
            col = "black")
     )
with(subset(df3, sub_metering == 2), 
     lines(timestamp, measurement, 
           type = "l",
           col = "red")
)
with(subset(df3, sub_metering == 3), 
     lines(timestamp, measurement, 
           type = "l",
           col = "blue")
)
leg.txt <- c("Sub_metering_1",
             "Sub_metering_2",
             "Sub_metering_3")
leg.col <- c("black", "red", "blue")

legend("topright", legend = leg.txt,
       #text.width = strwidth("1,000,000"),
       lty = 1, xjust = 1, yjust = 1,
       inset = 0,
       col = leg.col)
dev.off()