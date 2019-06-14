library(jsonlite)
library(tidyverse)
library(rvest)

#load eternal card set
eternal.cards <- fromJSON("./data/eternal-cards.json")

#scrape draft pack card names from eternalwarcry.com
#merge with full eternal card set to create column
#that marks eternal draft pack cards
eternal.cards <- lapply(1:4, function(x) {
  url <- sprintf("https://eternalwarcry.com/cards?Query=&DraftPack=yHgQekhUV5s&cardview=false&p=%s",x)  

  draft.pack <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="body-wrapper"]/div/div/div[2]/div[2]/div[3]/div/table') %>%
    html_table()
  
  colnames(draft.pack) <- draft.pack[1,] 
  
  draft.pack <- draft.pack %>%
    slice(2:n())
}) %>%
  bind_rows() %>%
  mutate(draft.pack = TRUE) %>%
  select(Name, draft.pack) %>%
  right_join(eternal.cards) %>%
  mutate(CardCode = paste0(SetNumber,"-",EternalID))

#replace NA in draft.pack column with 'no'
eternal.cards$draft.pack <- replace_na(eternal.cards$draft.pack, FALSE)

#create df with all draft cards (draft pack + dark frontier)
eternal.dark.frontier.draft <- eternal.cards %>%
  filter(SetNumber == 6 | draft.pack == TRUE)


#
#Random analysis stuff
#

#only single influence requirement
unique(eternal.dark.frontier.draft$Influence)

#average attack and health of single influence common and uncommon units
#by faction
eternal.dark.frontier.draft %>%
  filter(Rarity %in% c("Common","Uncommon") & Type == "Unit") %>%
  group_by(Influence) %>%
  summarise(n(),mean(Attack), mean(Health))
  
