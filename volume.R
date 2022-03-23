library(rStrava)
library(dplyr)
library(ggplot2)
library(lubridate)

col_lightblue <-rgb(52/256, 176/256, 227/256, 1)

app_name <- '' # chosen by user
app_client_id  <- '' # an integer, assigned by Strava
app_secret <- '' # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))

myinfo <- get_athlete(stoken, id = '') # enter your athlete ID
head(myinfo)

# get all activities, to plot distance o time 
my_acts <- get_activity_list(stoken)

# convert to data frame
act_data <- compile_activities(my_acts)

# keep only running activities
act_data_runs <- act_data[act_data$type == "Run" | act_data$type == "VirtualRun", ]

# some cleaning and formatting, then keep only the last 3 years
act_data_runs_filtered <- act_data_runs %>% select(ends_with("heartrate"), ends_with("time"), ends_with("distance"), "start_date_local") %>%
  mutate(elapsed_time_h = elapsed_time / 60 / 60, moving_time_h = moving_time / 60 / 60, date = as.Date(start_date_local)) %>% 
  select(!c("start_date_local", "moving_time", "elapsed_time")) %>%
  mutate(year = format(date,"%Y")) %>%
  filter(elapsed_time_h < 12, year > 2019) %>%
  arrange(date)  %>%
  group_by(year) %>%
  mutate(cumulative_distance = cumsum(distance)) %>%
  ungroup()  %>%
  mutate(day = yday(date))

head(act_data_runs_filtered)

p1 <- ggplot(act_data_runs_filtered, 
             aes(day, cumulative_distance, fill = year)) +  #, group=type
  geom_area(position = 'dodge', alpha = 0.5) +
  scale_y_continuous("kilometers") + 
  scale_x_continuous("day of the year") + 
  scale_fill_manual(values = c("gray82", "gray54", col_lightblue)) +
  ggtitle("yearly running volume") 
p1

