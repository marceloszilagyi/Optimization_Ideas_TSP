install.packages("pacman")
pacman::p_load('tidyverse', 'data.table', 'magrittr', 'geosphere', 'tspmeta')


# get the data from https://github.com/toddwschneider/shiny-salesman/blob/master/data/usa_cities.rds
download.file("https://github.com/toddwschneider/shiny-salesman/blob/master/data/usa_cities.rds?raw=true", "usa_cities")
usa_cities <- read_rds("usa_cities")


# solution for a small sample
usa_cities_sample <- usa_cities %>% sample_frac(0.10)
matrix_distance <- usa_cities_sample %>% dplyr::select(long, lat) %>% distm()
tsp_instance <- tsp_instance(usa_cities_sample %>% dplyr::select(long, lat) %>% as.matrix() , matrix_distance)
tour <- run_solver(tsp_instance, method = "2-opt", two_opt_repetitions = 30)
autoplot(tsp_instance, tour)

# now to the all cities; running 1000 times to try to get a better result 
matrix_distance <- usa_cities %>% dplyr::select(long, lat) %>% distm()
tsp_instance <- tsp_instance(usa_cities %>% dplyr::select(long, lat) %>% as.matrix(), matrix_distance)
tour <- run_solver(tsp_instance, method = "2-opt", two_opt_repetitions = 1000)
#tour returns the sequence of cities to visit
autoplot(tsp_instance, tour)
