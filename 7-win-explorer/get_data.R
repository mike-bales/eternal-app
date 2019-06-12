#get 7 win decks from google spreadsheet

library(googlesheets)
library(tidyverse)

#get sheet key
key <- extract_key_from_url('https://docs.google.com/spreadsheets/d/1dW4ALSyqTS-1DtBvhJ_ZVDtrt3DFtFxmg04A3B9YG98/edit#gid=144815197')

#get sheet
deck_sheet <- gs_key(key) %>%
  gs_read(ws = 'Draft Tracking') 

#loop through each row of the spreadsheet
#extract main cards
cards <- by(deck_sheet,1:nrow(deck_sheet), function(x){
  main_cards <- str_match_all(x$`EWC-P`,"((?:main=|\\G(?!^);)(\\d-\\d+:\\d+))")[[1]][,3]
  market_cards <- str_match_all(x$`EWC-P`,"((?:market=|\\G(?!^);)(\\d-\\d+:\\d+))")[[1]][,3]
  cards <- c(main_cards,market_cards)
  card_label <- c(rep('main',length(main_cards)),rep('market',length(market_cards)))
  df <- tibble(DeckID = x$TS,
                   cards = cards, 
                   MainMarket = card_label, 
                   DeckFaction = x$Factions,
                   DeckContributor = x$Contributor,
                   DeckWins = x$W,
                   DeckLoses = x$L) %>%
    mutate(SetNumber = str_sub(cards,1,1),
           EternalID = str_extract(cards,"(?<=-)\\d+"),
           Quantity = str_extract(cards, "(?<=:)\\d+")) %>%
    select(-cards)
  
  return(df)
})

all_decks <- bind_rows(cards)       
