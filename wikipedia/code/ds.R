library(tidyverse)
library(magrittr)
setwd("~/datascience/wikipedia/")



df <- read_csv('results/df.csv')

df %>% 
  ggplot(aes(length, watchers)) + 
  geom_point() + scale_x_log10() + scale_y_log10() 
  
