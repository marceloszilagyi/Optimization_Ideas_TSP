install.packages("pacman")
pacman::p_load('tidyverse', 'data.table', 'magrittr', 'geosphere', 'tspmeta')
# sigmoid function as function of e and t
sigmoid_annealing = function(delta,temperature) {
  tibble(delta, temperature,
         prob = 1/(1+exp(-delta/temperature)))
}

expand.grid(-20:20,0:10) %>% as_tibble() %>%
  set_colnames(c("delta", "temperature")) %>%
  pmap_dfr(.f = sigmoid_annealing) %>% 
  mutate(temperature = as.factor(temperature)) %>%
  ggplot(aes(x = delta, y = prob, color = temperature)) + 
  geom_line() + 
  ggtitle("Example: Temperature impact on the probablity of selection")