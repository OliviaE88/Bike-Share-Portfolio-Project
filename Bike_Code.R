# Install Packages.
install.packages("tidyverse")
library("tidyverse")
install.packages("janitor")
library("janitor")
install.packages("skimr")
library("skimr")
install.packages("lubridate")
library("lubridate")
install.packages("ggstatsplot")
install.packages(("naniar"))
library("naniar")
install.packages("waffle")
library("waffle")

# Import 12 months of data. 
# The size of the files exceeds the memory capacity of my R Studio membership.
# To avoid bias, take a random sample of each quarter and merge them. 
# Sample size of each data frame is one tenth of the original file. 


# Calculate sample size, create samples and remove large data set. 

# Divvy_Trips_2019_Q1 <- read.csv("Divvy_Trips_2019_Q1.csv")
# count(Divvy_Trips_2019_Q1)/10 # 36506
# set.seed(2022286)
# Cyclistic_Q1_2019<- Divvy_Trips_2019_Q1 %>%
# sample_n(36506)
# rm(Divvy_Trips_2019_Q1) 

# Divvy_Trips_2019_Q2 <- read.csv("Divvy_Trips_2019_Q2.csv")
# count(Divvy_Trips_2019_Q2)/10 # 110816
# set.seed(2022286)
# Cyclistic_Q2_2019 <- Divvy_Trips_2019_Q2 %>%
# sample_n(110816)
# rm(Divvy_Trips_2019_Q2)

# Divvy_Trips_2019_Q3 <- read.csv("Divvy_Trips_2019_Q3.csv")
# count(Divvy_Trips_2019_Q3)/10 # 164071
# set.seed(2022286)
# Cyclistic_Q3_2019 <- Divvy_Trips_2019_Q3 %>%
#  sample_n(164071)
# rm(Divvy_Trips_2019_Q3)

# Divvy_Trips_2019_Q4 <- read.csv("Divvy_Trips_2019_Q4.csv")
# count(Divvy_Trips_2019_Q4)/10 # 70405
# set.seed(2022286)
# Cyclistic_Q4_2019 <- Divvy_Trips_2019_Q4 %>%
#  sample_n(70405)
# rm(Divvy_Trips_2019_Q4)

# Check that all variables are in the right order and have the same format.

glimpse(Cyclistic_Q1_2019)
glimpse(Cyclistic_Q2_2019)
glimpse(Cyclistic_Q3_2019)
glimpse(Cyclistic_Q4_2019)



# The column titles in Q2 are inconsistent with the other files.
# Rename them in order to be able to merge the data frames. 

Cyclistic_Q2_2019 <- Cyclistic_Q2_2019 %>%
  clean_names()%>% 
  rename(trip_id = x01_rental_details_rental_id,
         start_time = x01_rental_details_local_start_time,
         end_time = x01_rental_details_local_end_time,
         bikeid = x01_rental_details_bike_id,
         tripduration = x01_rental_details_duration_in_seconds_uncapped,
         from_station_id = x03_rental_start_station_id,
         from_station_name = x03_rental_start_station_name,
         to_station_id = x02_rental_end_station_id,
         to_station_name = x02_rental_end_station_name,
         usertype = user_type,
         gender = member_gender,
         birthyear = x05_member_details_member_birthday_year)


# Create a single data frame using rbind().

Cyclistic_2019 <- rbind(Cyclistic_Q1_2019,
                        Cyclistic_Q2_2019,
                        Cyclistic_Q3_2019,
                        Cyclistic_Q4_2019)


# Rename the column names and make them consistent. 

Cyclistic_2019 <- Cyclistic_2019 %>%
  clean_names()%>%
  rename( bike_id = bikeid,
          trip_duration = tripduration,
          user_type = usertype,
          birth_year = birthyear)

# Check for duplicates in trip_id (should be unique values).
Cyclistic_2019 %>%
  distinct(trip_id) # There are no duplicates in trip_id

# Calculate trip duration to check the time unit used in the data set.
difftime(Cyclistic_2019$end_time, Cyclistic_2019$start_time, 
         units = "secs" )

# Trip duration is currently in seconds. We want to have it in minutes.
# Either divide the vector trip_duration by 60 or calculate it
# using the difftime function. 

Cyclistic_2019 <- Cyclistic_2019 %>%
  mutate(trip_duration_minutes= round(trip_duration/60 , 1))

#Identify NA Values. 
sapply(Cyclistic_2019, function(x) sum(is.na(x)))

#There are only NAs in Gender and birthyear. 


# Fill in missing data:
# We only know the gender for Subscribers.
# Therefore, replace NA with "Unknown" for Customers.

Cyclistic_2019 <- Cyclistic_2019 %>%
  mutate(gender= as.character(gender))%>%
  mutate(gender= if_else(user_type== "Customer", "Unknown", gender))

# Assign the right class to each variable.  

Cyclistic_2019 <- Cyclistic_2019 %>%
  mutate(trip_id = as.integer(trip_id), 
         bike_id = as.integer(bike_id),
         from_station_id = as.integer(from_station_id),
         to_station_id = as.integer(to_station_id),
         user_type = as.factor(user_type),
         gender = as.factor(gender),
         birth_year = as.integer(birth_year))

# Add variables weekday and month.

Cyclistic_2019 <- Cyclistic_2019 %>%
  mutate(weekday = wday(start_time, label = TRUE, abbr = FALSE),
         month = month(start_time, label = TRUE, abbr = FALSE)) 

# Create an age variable. 

Cyclistic_2019 <- Cyclistic_2019 %>%
  mutate(age = 2019- birth_year)

# Check trip duration. 
Cyclistic_2019 <- Cyclistic_2019 %>%
  arrange(trip_duration_minutes)

# The duration of the trip varies significantly, from 1 minute to three months. 
# I suspect that there are outliers that would influence the analysis. 
# Have a closer look at the distribution of the data.


options(scipen = 999) #turns off scientific mode

hist(Cyclistic_2019$trip_duration_minutes)
# The duration on the higher end seem to be due to outliers. 

Cyclistic_2019%>%
  ggplot(aes(month , trip_duration_minutes))+
  geom_boxplot(varwidth=T, fill="plum", outlier.color = "red")+
  ylim(0,300)

# Both the histogram and the boxplot confirm that there are several outliers 
# that do not represent the typical use of customers and members. 
# Use the IQR method to find and remove outliers. 

Q <-  quantile(Cyclistic_2019$trip_duration_minutes, 
               probs = c(0.25, 0.75), na.rm = FALSE)
IQR <- IQR(Cyclistic_2019$trip_duration_minutes)

upper_inner_fence <- (21.4 + 1.5 * 14.6) # Q2 + 1.5*IQR
lower_inner_fence <- (6.8 - 1.5 * 14.6) # Q2 + 1.5*IQR

Cyclistic_2019_Outliers_Excluded <- Cyclistic_2019 %>%
  filter(trip_duration_minutes > 0 & trip_duration_minutes < 43.3)

# Also, I am assuming that trips under 3 minutes, where start station equals 
# the end station are due to technical issues with the bike and should not 
# be included in the analysis.  

Cyclistic_2019_Outliers_Excluded <- 
  Cyclistic_2019_Outliers_Excluded %>%
  mutate(tech_issue = 
           if_else(trip_duration_minutes <3 & 
                     from_station_name== to_station_name, TRUE, FALSE))%>%
  filter(tech_issue != TRUE)

# Summarize data

Cyclistic_2019_Outliers_Excluded %>%
  select(user_type, gender, trip_duration_minutes, weekday, month, age) %>%
  summary()

# There is an issue with the age. Max age is 131, which is not possible. 

Cyclistic_2019_Outliers_Excluded <- 
  Cyclistic_2019_Outliers_Excluded %>% 
  arrange(desc(age))

Cyclistic_2019_Outliers_Excluded %>%
  ggplot(aes(user_type, age)) +
  geom_boxplot()

# Upon closer inspection there are several ages over 100, which is highly 
# unlikely for a bike rental. We assume that the age input is wrong.
# Replace all ages over 85 with NA. 

Cyclistic_2019_Outliers_Excluded <- 
  replace_with_na_at(data= Cyclistic_2019_Outliers_Excluded,
                     .vars = "age", 
                     condition = ~.x > 85)

Cyclistic_2019_Outliers_Excluded <- 
  replace_with_na_at(data= Cyclistic_2019_Outliers_Excluded,
                     .vars = "birth_year", 
                     condition = ~.x < 1934)

# Create age ranges.

Cyclistic_2019_Outliers_Excluded <- 
  Cyclistic_2019_Outliers_Excluded %>%
  mutate(age_range= case_when(
    age >= 16 & age < 20 ~ "between 16 and 19",
    age >= 20 & age < 30 ~ "between 20 and 29",
    age >= 30 & age < 40 ~ "between 30 and 39",
    age >= 40 & age < 50 ~ "between 40 and 49",
    age >= 50 ~ "50+"))

# Visualizations

# Prepare data for stacked area chart. 

Stacked_Area_Chart <- 
  Cyclistic_2019_Outliers_Excluded %>%
  select (start_time, trip_duration_minutes, user_type, month) %>%
  mutate(week = isoweek(start_time)) %>%
  group_by(user_type, week) %>%
  summarise(mean_duration = mean(trip_duration_minutes))%>%
  mutate(mean_duration= round(mean_duration, digits= 1))


# Create stacked area chart. 

Stacked_Area_Chart %>%
  drop_na(mean_duration)%>%
  ggplot(aes(x= week, y= mean_duration, fill= user_type )) +
  geom_area() +
  theme_bw()+
  labs(title = "Customer vs Subscriber Bike Usage over the Year",
       subtitle = "Usage as distance in minutes for the year 2019",
       caption = "source: Motivation International Inc", 
       x= "weeks", y= "bike usage", fill= "User Type")+
  scale_fill_manual(values = c("Customer" = "#006d77", "Subscriber"= "#83c5be"))

# Prepare data for Scatterplot.

Scatterplot <- Cyclistic_2019_Outliers_Excluded %>%
  select (trip_id, start_time, trip_duration_minutes, user_type, month) %>%
  group_by(month, user_type) %>%
  summarise(mean_duration = mean(trip_duration_minutes, na.rm= TRUE),
            total_rides = n())

# Crete Scatterplot.

Scatterplot %>%
  ggplot(aes(month, total_rides))+
  geom_point(aes(col= user_type, size= mean_duration))+
  theme_bw()+
  labs(title = "Customers vs Subscribers", 
       subtitle = "2019 Number of trips and duration",
       x="month", y= "Number of rides", 
       fill= "user type", size= "average duration (minutes)",
       caption = "source: Motivation International Inc")+
  scale_fill_manual(values = c("Customer" = "#006d77", "Subscriber"= "#83c5be"))

# Boxplot
Cyclistic_2019_Outliers_Excluded %>%
  filter(gender == "Male" | gender == "Female")%>%
  drop_na(gender, age)%>%
  ggplot(aes(gender, age))+
  geom_boxplot(varwidth = T, fill = "#006d77")+
  labs(title = "Gender and Age Disctribution", 
       subtitle = "Data only available for subscribers",
       size= "average duration (minutes)",
       caption = "source: Motivation International Inc")+
  theme_bw()



# Pie Chart
Cyclistic_2019_Outliers_Excluded %>%
  drop_na(gender)%>%
  ggplot(aes(x = "", fill = factor(gender))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="", 
       x=NULL, 
       y=NULL, 
       title="Gender of Subscribers", 
       caption="source: Motivation International Inc")+
  coord_polar(theta = "y", start=0)+
  theme_bw()+
  scale_fill_manual(values = c("Male" = "#006d77", 
                               "Female"= "#e29578", 
                               "Unknown"="#edf6f9"))

# Prepare Data for Waffle Chart.

Waffle <- Cyclistic_2019_Outliers_Excluded %>%
  drop_na(age_range) %>%
  group_by(age_range)%>%
  summarise(age_frequency= n())

# Create Vector

age<-c('between 16 and 19'= 2666, 
       'between 20 and 29'= 119735, 'between 30 and 39'= 111172,
       'between 40 and 49'= 41161, '50+'= 40380)

# Create Waffle Chart.
waffle(age/10000, rows=5, size=0.6, 
       colors=c("#006d77", "#83c5be", "#edf6f9", 
                "#ffddd2", "#e29578"), 
       title="Age Distribution of Subscribers", 
       xlab="1 square = 10000 persons")

# Prepare Data for Calendar Heatmap. 

Heatmap <- Cyclistic_2019_Outliers_Excluded %>%
  select(start_time, month, weekday)%>%
  mutate(date= as.Date(start_time)) %>%
  mutate(week = isoweek(start_time)) %>%
  group_by(date,weekday, week, month)%>%
  summarise(number_of_trips= n())

# Change levels for weekdays

Heatmap$weekday <- factor(Heatmap$weekday, levels = c("Monday", "Tuesday",
                                                      "Wednesday", "Thursday",
                                                      "Friday", "Saturday", 
                                                      "Sunday"))

# Create Vector for week of the month.
week_of_month <- (4 + day(Heatmap$date) + 
                    wday(floor_date(Heatmap$date, "month")))%/% 7
Heatmap$week_of_month <- week_of_month

# Create Vector for day.

day <-format(as.Date(Heatmap$date,format="%Y-%m-%d"), format = "%d")

# Create factor for week_of_month

Heatmap <- Heatmap %>% 
  mutate(week_of_month_factor = case_when(week_of_month==0 ~ "null",
                                          week_of_month==1 ~ "first", 
                                          week_of_month==2 ~ "second",
                                          week_of_month==3 ~ "third",
                                          week_of_month==4 ~ "fourth",
                                          week_of_month==5 ~ "fifth")) %>%
  mutate(weekday= substring(weekday, 1, 2),
         week_of_month_factor = as.factor(week_of_month_factor))%>%
  mutate(week_of_month_factor= recode(week_of_month_factor,"fifth"=1,
                                      "fourth"=2,"third"=3, "second"=4,
                                      "first"= 5, "null"=6))

# Create Calendar Heatmap




Heatmap %>%
  ggplot(aes(reorder(weekday, week_of_month_factor, decreasing=FALSE), 
             week_of_month_factor, fill = number_of_trips)) + 
  geom_tile(colour = "white") + 
  geom_text(aes(label= day))+
  facet_wrap(~month)+
  scale_fill_gradient(low="red", high="green")+
  labs(x="Day", 
       y="",
       fill= "Daily Trips",
       title = "Bike Rental Frequency per Day", 
       subtitle="Trips per Day in 2019",
       caption = "source: Motivation International Inc")+
  theme_minimal()+
  theme(strip.text.x = element_text(size=10, face="bold"),
        strip.background = element_rect(colour="black", fill="#CCCCFF"))+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0)) +
  coord_fixed(ratio=1)

# Prepare data for stacked bar chart

# create breaks
time_breaks <- hour(hm("00:00", "6:00", "9:00", "12:00", "3:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Early Morning", "Late Morning", 
            "Early Afternoon", "Late Afternoon", "Evening")

Cyclistic_2019_Outliers_Excluded$Time_of_day <- 
  cut(x=hour(Cyclistic_2019_Outliers_Excluded$start_time), 
      breaks = time_breaks, labels = labels, include.lowest=TRUE)


Stacked_Bar<- Cyclistic_2019_Outliers_Excluded %>%
  select(Time_of_day, user_type)%>%
  group_by(user_type, Time_of_day)%>%
  summarise(time_of_day_n =  n())%>%
  mutate(time_of_day_perc= round(case_when(user_type== "Customer" ~ time_of_day_n/ sum(time_of_day_n)*100,
                                           user_type== "Subscriber" ~ time_of_day_n/ sum(time_of_day_n)*100),1))

# Create stacked bar chart:
Stacked_Bar %>%  
  ggplot(aes(user_type, time_of_day_perc, fill= Time_of_day)) +
  geom_bar(position = "fill", stat = "identity")+
  theme_bw()+
  scale_fill_manual(values = c("Night" = "#006d77", 
                               "Early Morning"= "#83c5be", 
                               "Late Morning"="#edf6f9",
                               "Early Afternoon"= "#ffddd2" ,
                               "Late Afternoon"= "#e29578",
                               "Evening"= "#723d46")) +
  labs(x="User Type", 
       y="Frequency",
       fill= "Time of Day",
       title = "Bike Rental Frequency per Time of Day", 
       subtitle="2019 customer vs subscribers bike usage",
       caption = "source: Motivation International Inc")


# Create Bar Chart
Cyclistic_2019_Outliers_Excluded %>%
  ggplot(aes(weekday, fill= user_type)) +
  geom_bar(position= "dodge")+
  theme_bw()+
  scale_fill_manual(values = c("Customer" = "#006d77", 
                               "Subscriber"= "#83c5be")) +
  labs(x="", 
       y="Number of Trips",
       fill= "User Type",
       title = "Customer vs Subscriber Rentals", 
       subtitle="2019 Bike Rental Frequency per Weekday",
       caption = "source: Motivation International Inc")


