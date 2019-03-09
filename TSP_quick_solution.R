install.packages("pacman")
pacman::p_load('tidyverse', 'data.table', 'magrittr', 'geosphere', 'tspmeta')


# get the data from https://github.com/toddwschneider/shiny-salesman/blob/master/data/usa_cities.rds
download.file("https://github.com/toddwschneider/shiny-salesman/blob/master/data/usa_cities.rds?raw=true", "usa_cities")
usa_cities <- read_rds("usa_cities")


# just to make it faster first
usa_cities_sample <- usa_cities %>% sample_frac(0.05)
matrix_distance <- cbind(usa_cities_sample$long, usa_cities_sample$lat) %>% distm()
tsp_instance <- tsp_instance(cbind(usa_cities_sample$long, usa_cities_sample$lat), matrix_distance)
tour <- run_solver(tsp_instance, method = "2-opt")
autoplot(tsp_instance, tour)

# now to the all cities

matrix_distance <- cbind(usa_cities$long, usa_cities$lat) %>% distm()
tsp_instance <- tsp_instance(cbind(usa_cities$long, usa_cities$lat), matrix_distance)
tour <- run_solver(tsp_instance, method = "2-opt")
autoplot(tsp_instance, tour)
