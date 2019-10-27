#' Function for generating plot of mean arrival delays at different airports.
#'
#' \code{visualize_airport_delays} generates a map plot with color scale for mean arrival delay at different airports.Dataset used is the nycflights13.
#'  
#'
#' @return  \code{visualize_airport_delays} returns a plot.
#'
#' @examples
#' visualize_airport_delays()
#' 
#' @references \url{https://www.rdocumentation.org/packages/nycflights13/versions/1.0.1}
#'
#' @importFrom stats na.omit
#' @import maps
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'

visualize_airport_delays <- function(){
  flights_data <- na.omit(nycflights13::flights)
  airport_data <- na.omit(nycflights13::airports)
  
  
  # Join data sets
  flights_data <- dplyr::rename(flights_data, "faa" = "dest")
  joined_data <- inner_join(flights_data, airport_data, by="faa") # Discard data mismatches using inner join
  
  # Create dataframe for plotting (different structure)
  plot_data <- data.frame(unique(joined_data[c('faa', 'name', 'lat', 'lon')]))
  
  # Group by airports, then get mean for each category of airport over arrival delay
  calculated_mean <- joined_data %>% group_by(faa) %>% summarise(mean_delay = mean(arr_delay))
  
  # Join mean to plot data.
  plot_data <- left_join(plot_data, calculated_mean, by = "faa")
  
  # Create map and plot using mean value as color scale
  used_map <- map_data("usa")
  ggplot() + geom_polygon(data = used_map, aes_string(x="long", y = "lat", group = "group"), fill = "#636e72") + 
    coord_fixed(1.3) +
    geom_point(data = plot_data, aes(x = lon, y = lat, color = mean_delay), size = 5) +
    scale_colour_viridis_c()

}