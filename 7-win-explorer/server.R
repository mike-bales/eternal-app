#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

shinyServer(function(input, output) {
  # Get the data
  tbl_dat <- read_csv("./data/top_cards.csv")
  # change column types
  tbl_dat <- tbl_dat %>% 
    mutate(DeckFaction = as.factor(GroupedDeckFaction),
           Rarity = as.factor(Rarity),
           CardType = as.factor(Type),
           CardFaction = as.factor(Faction)) %>%
  select(DeckFaction, Name, Rarity, CardFaction, CuratedPack = DraftPack, CardType, 
         TotalCards, NormalizedTotalCards, NumberOfDecks, DecksWithCard, PctDecksWithCard, AvgCopiesPerDeck,
         -GroupedDeckFaction, -Type, -Faction)
  
  output$top_cards <- renderDT({
    datatable(tbl_dat, filter = "top")
  })
})
