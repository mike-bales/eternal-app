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
    mutate(CardCode = str_extract(cards, "\\d-\\d+"),
           Quantity = as.numeric(str_extract(cards, "(?<=:)\\d+"))) %>%
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
#Finally, change NA to 0 for all Influence columns
comb.cards <- eternal.cards  %>% 
  left_join(bg_cards, by = "Name") %>%
  select(Name, Set, CardCode = CardCode.x, 
         DraftPack = draft.pack, CardText, 
         Rarity = Rarity.x,Type = Type.x, 
         ImageUrl, UnitType, Influence, Cost, 
         Strength = Str, Health = Health.y, Faction,
         FE.Type = Type.y, FE.Subtype = Subtype,
         Removal, Unit, Buff, Power, F, T, J, P, S,
         starts_with("Produces")) %>%
  mutate_at(c('F','T','J','P','S'), replace_na, replace = 0)

#Combine deck lists with combined card list
all_decks <- all_decks %>%
  left_join(comb.cards, by = "CardCode") 

write_csv(all_decks %>% 
            mutate(UnitType = paste(UnitType)), "./data/all_7_win_decks.csv")

write_csv(all_decks[1:20,]%>% 
            mutate(UnitType = paste(UnitType)), "./data/all_7_win_decks_small.csv")

##########
#Metrics
#########

#sum the number of each card across all decks
card.sum <- all_decks %>%
  group_by(Name) %>%
  summarise(total.cards = sum(Quantity), total.decks = n()) %>%
  arrange(desc(total.cards))

#####
#percentage of each decks color pips
#####

#We need to create a dataframe at the deck level so we group by DeckID
deck.factions <- all_decks %>%
  group_by(DeckID) %>%
  summarise(F = sum(F), #Then we sum up all the influence requirements of a decks cards by faction
            T = sum(T), 
            S = sum(S),
            P = sum(P),
            J = sum(J)) %>%
  mutate(total.pips = F + T + J + P + S) %>% #Next we sum all the influence pips across factions for each deck
  mutate_at(c('F','T','J','P','S'), ~round((. / total.pips)*100,2)) %>% #We use that to change our Faction pip 
                                                                        #number to a percentage
  select(-total.pips) %>% # drop the total.pips column since we don't need it anymore
  gather(key = "key", value = "value", -DeckID) %>% #change the data from wide to long
  arrange(DeckID, desc(value)) %>% #sort by Deck and then by the faction pip percentage so the Primary faction is first
  group_by(DeckID) %>% #group by DeckID again for next step
  mutate(OrderedDeckFaction = str_sub(paste(key, collapse = ""),1,2)) %>% #Create faction label for each deck with
                                                                          #primary faction letter first and secondary second
  ungroup() %>%
  mutate(GroupedDeckFaction = fct_collapse(factor(OrderedDeckFaction), #create the named faction label
                                           Feln = c("PS","SP"),
                                           Elysian = c("TP","PT"),
                                           Xenan = c("TS","ST"),
                                           Hooru = c("JP","PJ"),
                                           Combrei = c("JT","TJ"),
                                           Argenport = c("SJ","JS"),
                                           Rakano = c("JF","FJ"),
                                           Skycrag = c("FP","PF"),
                                           Stonescar = c("FS","SF"),
                                           Praxis = c("FT", "TF")))

#Create a column that labels the Faction rank for that deck. 
#Since we've ordered within each DeckID by pip percentage the first is
#always Primary, the second is secondary, and third is splash(tertiary).
#So our new column is just those three plus two NA looped down the whole dataset
deck.factions$FactionRank <- c("Primary","Secondary","Splash",NA,NA) 


deck.factions <- deck.factions %>%
  mutate(FactionRank = factor(FactionRank, levels = c("Splash","Secondary","Primary"))) %>%
  filter(value > 0) %>%
  arrange(value, DeckID)

library(ggplot2)
library(ggridges)

faction.colors <- c( P = "Blue",
                     S = "Purple",
                     T = "Yellow",
                     F = "Red",
                     J = "Green")

faction.plot.inputs <- list(Feln = c("S","P","J","T","F"),
                            Elysian = c("T","P","S","J","F"),
                            Xenan = c("T","S","J","F","P"),
                            Hooru = c("J","P","F","S","T"),
                            Combrei = c("J","T","P","F","S"),
                            Argenport = c("S","J","T","P","F"),
                            Rakano = c("J","F","T","P","S"),
                            Skycrag = c("F","P","T","S","J"),
                            Stonescar = c("F","S","P","T","J"),
                            Praxis = c("F","T","S","P","J"))

lapply(names(faction.plot.inputs), function(i){
  plot.data <- deck.factions %>%
    filter(GroupedDeckFaction == i) %>%
    group_by(DeckID) %>%
    mutate(not_keep = any(value[FactionRank == "Splash"] > 20)) %>%
    ungroup() %>%
    filter(not_keep == FALSE) %>%
    select(-not_keep)
  
  levs <- plot.data %>%
    filter(key == faction.plot.inputs[i][[1]][1])%>%
    arrange(value)
  
  plot.data$DeckID <- factor(plot.data$DeckID, levels = levs$DeckID)
  plot.data$key <- factor(plot.data$key, levels = faction.plot.inputs[i][[1]])
  
  plot <- ggplot(plot.data, aes(fill = key, x = DeckID, y = value)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values=faction.colors) +
          coord_flip() +
          ggtitle(sprintf("%s: Color Distribution For All 7 Win Decks",i)) +
          ylab("Percentage of Pips") 
  
  ggsave(sprintf("./output/%s_faction_pct_bar_plot.png",i),plot = plot)
}) 

ridge.plot <- ggplot(deck.factions, aes(y = FactionRank)) +
  geom_density_ridges(aes(x=value,height = ..density..), stat = "density", trim = TRUE, fill = "light blue", 
                      color = "white") +
  scale_x_continuous("Percent of Pips") +
  scale_y_discrete(name = NULL) +
  ggtitle("Distribution of Percentage of a Decks Pips That Are Primary, Secondary, and Splash Color") +
  theme_minimal()

ridge.plots.by.faction <- ridge.plot +
  facet_wrap(~ GroupedDeckFaction)
ridge.plots.by.faction
