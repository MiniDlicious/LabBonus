library(dplyr)
library(nycflights13)
library(ggplot2)
library(maps)

flights_data <- na.omit(nycflights13::flights)
airport_data <- nycflights13::airports


# Join data sets
flights_data <- dplyr::rename(flights_data, faa = dest)
joined_data <- left_join(flights_data, airport_data, by="faa")
#joined_data <- cbind(joined_data, mean_delay = joined_data[['dep_delay']] - joined_data[['arr_delay']])

plot_data <- data.frame(unique(joined_data[c('faa', 'name', 'lat', 'lon')]))
plot_data$mean_delay <- 

usa <- map_data("world") # we already did this, but we can do it again
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_point(data = plot_data, aes(x = lon, y = lat), color = "yellow", size = 5)
