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
                  my_tbl$Date == "1/2/2007" |
                  my_tbl$Date == "2/2/2007"
                   )
rm(my_tbl)
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
df3<- mutate(small_df, 
                    timestamp = parse_date_time
                    (paste(small_df$Date, small_df$Time),
                      orders = "%d%m%y HMS"
                    )
                  )
rm(small_df)
#choose only columns needed
df3 <- select(df3, Sub_metering_1:Sub_metering_3, 
              Global_active_power:Voltage, 
              timestamp 
              )
rm(small_df2)
# df3 <- gather(df3, 
#               key = sub_metering, 
#               value = measurement, 
#               Sub_metering_1:Sub_metering_3,
#               -timestamp)


#tidy df3 by 
df3 <- gather(df3, 
              key = sub_metering, 
              value = measurement, 
             -(Global_active_power:timestamp)
              )
# df3 <- gather(df3, 
#               key = globals, 
#               value = energy_val, 
#               -(timestamp:measurement)
#               )

df3 <- mutate(df3, 
              sub_metering_value = 
                extract_numeric(sub_metering)
              )
df3 <- arrange(df3, timestamp)
####################################################
library(graphics)
dev.off()
#  png(filename = "plot4.png", 
#      width = 480, 
#      height = 480, 
#      units = "px"
# )
par(mfrow = c(2,2))
######active power

with(df3,
     plot(timestamp, df3$Global_active_power,
          #col = as.character(sub_metering),
          xlab = "", ylab = "Global Active Power", 
          type = "n" 
     )
)
lines(df3$timestamp, df3$Global_active_power, 
           type = "l",
           col = "black")
######voltage

with(df3,
     plot(timestamp, df3$Voltage,
          #col = as.character(sub_metering),
          xlab = "", ylab = "Voltage", 
          type = "n" 
     )
)
lines(df3$timestamp, df3$Voltage, 
      type = "l",
 
       col = "black")
########### SUBMETERING
with(df3,
     plot(timestamp, measurement, #col = as.character(sub_metering),
          xlab = "", ylab = "measurement", 
          type = "n" 
     )
)
with(subset(df3, sub_metering_value == 1), 
     lines(timestamp, measurement, 
           type = "l",
           col = "black")
)
with(subset(df3, sub_metering_value == 2), 
     lines(timestamp, measurement, 
           type = "l",
           col = "red")
)
with(subset(df3, sub_metering_value == 3), 
     lines(timestamp, measurement, 
           type = "l",
           col = "blue")
)


###### reactive
with(df3,
     plot(timestamp, df3$Global_reactive_power,
          #col = as.character(sub_metering),
          xlab = "", ylab = "Global_Reactive_power", 
          type = "n" 
     )
)
lines(df3$timestamp, df3$Global_reactive_power, 
      type = "l",
      col = "black")

#dev.off()

