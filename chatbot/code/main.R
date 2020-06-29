setwd('~/datascience/chatbot/')

#ako funguje bot
#id je presne tolko isto
#co riadok to otazka? lebo su tam aj ano
#dva stlpce name
#R


library(tidyverse)
library(magrittr)

df <- read_csv('data/eurovea-export-vol2.csv')

df <- df %>% 
  mutate(date = as.Date(createdAt), month = format(createdAt,"%Y-%m") )




# EDA ---------------------------------------------------------------------


df %>% 
  arrange(session_conversation_id, createdAt) %>% 
  write_csv("results/df.csv")
  
df %>% count(intentId, answerId) %>% as.data.frame()

df %>% count(intentId, name) %>% as.data.frame()

df %>% 
  summarise(
    n_distinct(internal_user_id), 
    n_distinct(livezilla_userid), 
    n_distinct(session_conversation_id)
    )



# pocet otazok v case
df %>% 
  ggplot(aes(createdAt)) + geom_histogram() 

ggsave('results/pocet otazok v case.png', width=9)



# df %>% 
#   group_by(month) %>% summarise(sessions = n_distinct(session_conversation_id)) %>% 
#   ggplot(aes(month, sessions)) + geom_point() + geom_line(group = 1)
# 
# ggsave('results/pocet chatov v case.png')





# typy otazok v case
# df %>% 
#   drop_na(name) %>% 
#   group_by(month) %>% count(name) %>% 
#   ggplot(aes(month, n, group=name, color = name)) + geom_line()
df %>% 
  drop_na(name) %>% 
  group_by(month) %>% count(name) %>% 
  ggplot(aes(month, n, fill=name)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~name, ncol=4) + theme(legend.position = "none")

ggsave('results/typy otazok v case.png', width=11)




# podiel zodpovedanych otazok
df %>% 
  mutate(answered = !is.na(answerId)) %>% 
  filter(grepl("?", incomingmessage,  fixed = TRUE)) %>% 
  group_by(month) %>% summarise(answered = mean(answered)) %>% 
  ggplot(aes(month, answered)) + geom_bar(stat="identity")

ggsave('results/podiel zodpovedanych otazok v case.png')





# entity v texte (Golem, Korona)
#...

# dlzka session
df %>% 
  group_by(session_conversation_id) %>% 
  summarise(
    duration = 0.1 + difftime(max(createdAt), min(createdAt), units = "mins") %>% as.double(),
    month = min(month)
    ) %>% 
  ggplot(aes(duration)) + 
  geom_histogram() + 
  facet_wrap(~month, ncol=1) + 
  scale_x_log10()

ggsave('results/dlzka session v case.png')
