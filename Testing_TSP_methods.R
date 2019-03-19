if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load('tidyverse', 'data.table', 'magrittr', 'geosphere', 'tspmeta', 'maps', 'gganimate', 'png', "transformr")


# get the data from https://github.com/toddwschneider/shiny-salesman/blob/master/data/usa_cities.rds
download.file("https://github.com/toddwschneider/shiny-salesman/blob/master/data/usa_cities.rds?raw=true", "usa_cities")
usa_cities <- read_rds("usa_cities")

# calculate distance matrix and assemble tsp_instance object for further processing
matrix_distance <- usa_cities %>% dplyr::select(long, lat) %>% distm()
tsp_instance <- tsp_instance(usa_cities %>% dplyr::select(long, lat) %>% as.matrix(), matrix_distance)

# list all methods
methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion", "arbitrary_insertion", "nn",
             "repetitive_nn", "2-opt")

# run all methods, use repetitions
compare_methods <- purrr::map(methods, function(x) {
  run_solver(tsp_instance, 
             method = x, 
             start= 1 ,
             control = list(rep = 50, two_opt_repetitions = 50)
             )})

names(compare_methods) <- methods

# functions to get attributes
tour_len_ext <- attr_getter("tour_length")
method_ext <- attr_getter("method")

# get summary of the tour size and plot it 
methods_compare_result <- purrr::map_df(compare_methods, function(x) 
{tibble(method = method_ext(x), length = tour_len_ext(x))})

methods_compare_result %>% arrange(-length) %>%
  ggplot(aes(fct_inorder(method), length/1000000)) +
  geom_point(stat = 'identity') +
  coord_flip() +
  ggtitle("Performance of methods in the TSP, for ~1000 US cities") +
  ylab("Length or the tour (in million meters?)") +
  xlab("Method")


# rearrange the tours to always start with the 1st city on the cities data frame

reset_index <- function(x) {
  
 reset_index <- x %>% enframe(name = "reference") %>%
    mutate(index = row_number()) %>% filter(reference == 1) %>% pull(index)
  list_size <- length(x)
 
   x %>% enframe(name = NULL) %>% 
    mutate(index = row_number()) %>%
    mutate(index_based_one = ifelse(index>=reset_index, 
                                    index-reset_index+1, 
                                    index+list_size-reset_index+1)) %>%
    mutate(index = index_based_one) %>%
    arrange(index) %>%
    dplyr::select(value, index) %>%
    mutate(method = method_ext(x))
}

tour_sequence <- purrr::map_df(compare_methods, function (x) {reset_index(x)}) %>% 
  right_join(usa_cities %>% mutate(value = row_number()), by = "value") %>%
  arrange(method, index, value)

# before plotting, prep the map
states <- map_data("state")

# need to add a last line to 'close the route'
closer <- tour_sequence %>% filter (index == 1) %>% mutate (index = 1002)
tour_sequence <- bind_rows(tour_sequence,closer) %>% arrange(method, index, value)

# methods are "2-opt", "nn"

tour_graph <- tour_sequence %>%
  filter(method %in% c("2-opt", "nn")) %>%
  mutate(ind_method = row_number()) %>%
  ggplot(aes(long,lat, color=method)) +geom_jitter(alpha = 0.20, color = "blue") + geom_path(alpha = 0.8) +
  geom_polygon(data = states, aes(long, lat, group=group), color = "grey", fill = NA) + coord_fixed(1.3) +
  theme_void() +
  ggtitle("          Routes of the most and least efficient TSP methods") +
  theme(legend.position = c(0.85, 0.25)) +
  scale_color_hue(labels = c("Most efficient: 2-opt", "Least efficient: nn"))

tour_graph

tour_animated <- tour_graph + transition_reveal(ind_method, keep_last = TRUE) +  ease_aes('cubic-in-out')
animate(tour_animated,nframes = 200, fps = 5, height = 510/2, width = 882/2) # need to play more with the right size


