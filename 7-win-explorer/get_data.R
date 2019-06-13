#get 7 win decks from google spreadsheet

library(googlesheets)
library(tidyverse)
library(readxl)

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
    mutate(CardCode = str_sub(cards,1,5),
           Quantity = str_extract(cards, "(?<=:)\\d+")) %>%
    select(-cards)
  
  return(df)
})

all_decks <- bind_rows(cards)

write_csv(all_decks, "./data/all_7_win_decks.csv")

#Read in Ben Grasher card list
bg_cards <- read_excel("./data/EWC Parsing Week 19.xlsx", sheet = "CardAttributes") %>%
  rename(CardCode = `Card Code`,
         DraftPack = `Set6 Curated`)

comb.cards <- eternal.cards %>%
  anti_join(bg_cards, by = "Name")
#eternal.cards variables
Name        
CardText
Cost
Rarity
Type
ImageUrl
UnitType
Influence

#bg_cards variables
Name 
Set
CardCode
DraftPack
Faction
Removal
Unit
Buff
Power
F
T
J
P
S
Str
Health
ProducesF
ProducesT
ProducesJ
ProducesP
ProducesS
