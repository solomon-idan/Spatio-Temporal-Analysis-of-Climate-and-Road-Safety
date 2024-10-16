# set working directorate
setwd("D:/document/mphil")

#import dataset
library(readxl)
kumasi_data_full<- read_excel("kumasi_data_full.xlsx")

#create year, month , non-fatal variable
kumasi_data_full2 <- kumasi_data_full %>%
  mutate(date = ymd(date), # convert date to a Date class
         year = format(date, "%Y"), # extract year as a character
         month = format(date, "%m"), # extract month as a character
         non_fatal_injuries = monthly_severe_injury + monthly_minor_injury)
#---------------------------------------------------------------#

#plot of climate variables

# Create the time series plot
ggplot(kumasi_data_full, aes(x = date)) +
  geom_bar(aes(y = avg_rainfall, fill = "Average Rainfall"),
           stat = "identity", alpha = 0.9)+
  geom_line(aes(y = avg_temp, color = "Average Temperature"), size = 0.9) +
  scale_color_manual(values = c("Average Temperature"= "")) +
  scale_fill_manual(values = c("Average Rainfall" = "#66c2a5")) +
  labs(title = "TREND OF CLIMATE VARIABLES FROM 2010-2021",
       x = "Date",
       y = "Temperature (Celsius) / Rainfall (mm)",
       color = " ",
       fill = " ") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold",
                                  margin = margin(b = 20)), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6),legend.position = "bottom")+ 
  scale_x_date(date_breaks = "8 month", date_labels = "%b\n%Y")

# graph road crashes and climate variables
ggplot(kumasi_data_full, aes(x = date))+
  geom_bar(aes(y = monthly_crashes, fill = "Number of Crashes"),
           stat = "identity", alpha = 0.9)+
  geom_line(aes(y = avg_temp, color = "Average Temperature"), size = 0.9) +
  geom_line(aes(y = avg_rainfall, color = "Average Rainfall"), size = 0.9) +
  scale_color_manual(values = c("Average Temperature" = "yellow",
                                "Average Rainfall" = "#2ca25f")) +
  scale_fill_manual(values = c("Number of Crashes" = "#FFE5B4")) +
  labs(title = "Trend of Road Traffic Crashes and Climate Variables",
       x = "Date",
       y = "Temperature (Celsius) / Rainfall (mm)",
       color = " ",
       fill = " ") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold",
                                  margin = margin(b = 20)), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6),legend.position = "bottom")+ 
  scale_x_date(date_breaks = "8 month", date_labels = "%b\n%Y") 


# graph fatal injuries and climate variables
ggplot(kumasi_data_full, aes(x = date))+
  geom_bar(aes(y = monthly_fata_injury , fill = "Number of Fatal Injuries"),
           stat = "identity", alpha = 0.9)+
  geom_line(aes(y = avg_temp, color = "Average Temperature"), size = 0.9) +
  geom_line(aes(y = avg_rainfall, color = "Average Rainfall"), size = 0.9) +
  scale_color_manual(values = c("Average Temperature" = "yellow",
                                "Average Rainfall" = "#2ca25f")) +
  scale_fill_manual(values = c("Number of Fatal Injuries" = "#b5b8ff")) +
  labs(title = "Trend of Road Traffic Fatalities and Climate Variables",
       x = "Date",
       y = "Temperature (Celsius) / Rainfall (mm)",
       color = " ",
       fill = " ") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold",
                                  margin = margin(b = 20)), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6),legend.position = "bottom")+ 
  scale_x_date(date_breaks = "8 month", date_labels = "%b\n%Y") 


# graph non-fatal injuries and climate variables

ggplot(kumasi_data_full2, aes(x = date))+
  geom_bar(aes(y = non_fatal_injuries , fill = "Number of Non-Fatal Injuries"),
           stat = "identity", alpha = 0.9)+
  geom_line(aes(y = avg_temp, color = "Average Temperature"), size = 0.9) +
  geom_line(aes(y = avg_rainfall, color = "Average Rainfall"), size = 0.9) +
  scale_color_manual(values = c("Average Temperature" = "yellow",
                                "Average Rainfall" = "#2ca25f")) +
  scale_fill_manual(values = c("Number of Non-Fatal Injuries" = "#fccde5")) +
  labs(title = "Trend of Road Traffic Non-Fatal Injuries and Climate Variables",
       x = "Date",
       y = "Temperature (Celsius) / Rainfall (mm)",
       color = " ",
       fill = " ") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold",
                                  margin = margin(b = 20)), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 6),legend.position = "bottom")+ 
  scale_x_date(date_breaks = "8 month", date_labels = "%b\n%Y") 


#plot yearly spatial trend of road crashes

ggplot(kumasi_data_full2) +
  aes(fill = monthly_crashes) +
  geom_sf(size = 1.2) +
  scale_fill_gradient(low ="white", high = "#FFE5B4") +
  theme_minimal() +
  facet_wrap(vars(year)) +
  labs(title = "Spatial Trend of Yearly Road Traffic Crashes in Kumasi Metropolitan District ",
       color = " ",
       fill = " ") + 
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5))

#plot yearly spatial trend of road fatalities

ggplot(kumasi_data_full2) +
  aes(fill = monthly_fata_injury ) +
  geom_sf(size = 1.2) +
  scale_fill_gradient(low ="white", high = "#b5b8ff") +
  theme_minimal() +
  facet_wrap(vars(year))+
  labs(title = "Spatial Trend of Yearly Road Traffic Fatalities in Kumasi Metropolitan District ",
       color = " ",
       fill = " ") + 
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5))


#plot yearly spatial trend of non-fatal road traffic injury
ggplot(kumasi_data_full2) +
  aes(fill =non_fatal_injuries) +
  geom_sf(size = 1.2) +
  scale_fill_gradient(low ="white", high = "#fccde5") +
  theme_minimal() +
  facet_wrap(vars(year))+
  labs(title = "Spatial Trend of Yearly Non- Fatal Road Traffic Injury in Kumasi Metropolitan District ",
       color = " ",
       fill = " ") + 
  theme(plot.title = element_text(size = 18L,
                                  face = "bold",
                                  hjust = 0.5)) 



