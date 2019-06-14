#get 7 win decks from google spreadsheet

library(googlesheets)
library(tidyverse)
library(readxl)

#get googlesheet key which is used to access the sheet
key <- extract_key_from_url('https://docs.google.com/spreadsheets/d/1dW4ALSyqTS-1DtBvhJ_ZVDtrt3DFtFxmg04A3B9YG98/edit#gid=144815197')

#download sheet and turn it into a dataframe
deck_sheet <- gs_key(key) %>%
  gs_read(ws = 'Draft Tracking') 

#loop through each row of the dataframe
#and get the main and market cards for each deck
#The output will be a list of dataframes, one for
#each deck that contains all the cards plus data 
#about the deck overall
cards <- by(deck_sheet,1:nrow(deck_sheet), function(x){
  
  #get the main deck and market cards by parsing the EWC URL using regular expressions
  #this will result in getting the card code and quantity string for each card (ex. 6-37:1)
  main_cards <- str_match_all(x$`EWC-P`,"((?:main=|\\G(?!^);)(\\d-\\d+:\\d+))")[[1]][,3]
  
  market_cards <- str_match_all(x$`EWC-P`,"((?:market=|\\G(?!^);)(\\d-\\d+:\\d+))")[[1]][,3]
  
  #combine the main and market cards into a single vector
  cards <- c(main_cards,market_cards)
  
  #create a variable that will identify whether a card is a main or market by repeating
  #"main" and "market" a number of times equal to the length of the main and market objects
  card_label <- c(rep('main',length(main_cards)),rep('market',length(market_cards)))
  
  #create the dataframe by combining cards, card_label, Faction, Contributor, W, L
  #The latter 4 will repeat for each row so they will have the same data on each row.
  #Also create the CardCode (set-card#) and Quantity variables. Quantity is the number of that card
  #in the deck. Finally, drop the cards variable.
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

#combine each individual dataframe into a single dataframe.
all_decks <- bind_rows(cards)

#Read in Ben Grasher card attributes list
bg_cards <- read_excel("./data/EWC Parsing Week 19.xlsx", sheet = "CardAttributes") %>%
  rename(CardCode = `CardCode`,
         DraftPack = `Set6 Curated`)

#merge the EWC JSON card list with Ben Grashers card list on the card name
#select which variables to keep and rename some of them
comb.cards <- eternal.cards  %>% 
  left_join(bg_cards, by = "Name") %>%
  select(Name, Set, CardCode = CardCode.x, 
         DraftPack = draft.pack, CardText, 
         Rarity = Rarity.x,Type = Type.x, 
         ImageUrl, UnitType, Influence, Cost, 
         Strength = Str, Health = Health.y, Faction,
         FE.Type = Type.y, FE.Subtype = Subtype,
         Removal, Unit, Buff, Power, F, T, J, P, S,
         starts_with("Produces"))

#Combine deck lists with combined card list
all_decks <- all_decks %>%
  left_join(comb.cards, by = "CardCode") 

#write the new deck list with card attributes to a csv
write_csv(all_decks, "./data/all_7_win_decks.csv")
