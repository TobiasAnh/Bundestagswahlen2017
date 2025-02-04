library(tidyverse)
library(shiny)
library(DT)
library(plotly)
library(gridExtra)
library(sf)
library(rsconnect)
library(devtools)
theme_set(theme_classic())

#####  Cleaning overall file ##### 

file_local = "2017_german_election_overall.csv"
overall <- read_csv(file_local)

overall$area_names <- overall$area_names  %>%  str_replace_all(pattern = c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss"))
overall$state <- overall$state %>%  str_replace_all(pattern = c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss"))

identical(overall$X1,overall$area_id)
overall[,1] <- NULL 
overall[,c(1,4:9)] <- lapply(overall[,c(1,4:9)], as.integer)

glimpse(overall)

# estimating Wahlbeteiligung (absolute and relative) and number of valid second votes for GERMANY
Wahlbeteiligung_bund <- overall %>% 
  summarize(Wahlberechtigt = sum(registered.voters),
            Wahlbeteiligung = sum(total_votes), 
            Valide_Zweitstimmen = sum(valid_second_votes), # VALID SECOND VOTES !
            Wahlbeteiligung_proz = (sum(total_votes) / sum(registered.voters)))


# estimating Wahlbeteiligung (absolute and relative) and number of valid second votes for each GERMAN STATE
Wahlbeteiligung_laender <- overall %>% 
  group_by(state) %>% 
  summarize(Valide_Zweitstimmen = sum(valid_second_votes),
            Wahlbeteiligung = (sum(total_votes) / sum(registered.voters))) %>% 
  arrange(Wahlbeteiligung)

# Wahlbeteiligung for each German state
Wahlbeteiligung_laender %>% ggplot(aes(reorder(state, Wahlbeteiligung), Wahlbeteiligung)) + 
  geom_point() + 
  ylim(c(0.6,1)) + 
  labs(y="Wahlbeteiligung [%]", x = "Bundesland")+
  geom_text(aes(label = signif(Wahlbeteiligung, 3)),
            position = position_dodge(0.9),
            hjust = -1)+
  coord_flip()

#### Cleaning party file #### 
party <- read_csv("2017_german_election_party.csv")

party$area_name <- party$area_name %>%  str_replace_all(pattern = c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss"))
party$state <- party$state %>% str_replace_all(pattern = c("ä" = "ae", "ö" = "oe", "ü" = "ue", "ß" = "ss"))

identical(party$X1, party$area_id)
party$X1 != party$area_id
party[295:305,]
party$X1 <- NULL # X1 simple iterative row indices, area_id starts over for each new party
glimpse(party)
party[,c(1,5,6)] <- lapply(party[,c(1,5,6)], as.integer)
party$state <-  factor(party$state, ordered = TRUE)
final_parties$party <- factor(final_parties$party, levels = c("CDU/CSU", "SPD", "AfD", "FDP", "Linke", "B90/Gruenen", "Andere"), ordered = T)
glimpse(party)


#replacing long with short party names

names_long <- unique(party$party)
names_short <- c("CDU", "SPD", "Linke", "B90/Gruenen", "CSU", "FDP", "AfD")

for (x in 1:7) {
  party$party <- str_replace(party$party, names_long[x], names_short[x])
} 

#spread data set to combine SECOND votes of CDU and CSU #
party_zweit <- party %>% select(-votes_first_vote) %>% 
               spread(party, votes_second_vote) %>% 
               mutate(`CDU/CSU` = CDU+CSU) %>%
               select(-c(CDU,CSU))

# Add up all second votes of 'other' parties #
top_parties <- c("CDU/CSU", "SPD", "Linke", "B90/Gruenen", "FDP", "AfD")
andere_zweit <- select(party_zweit, -top_parties, -area_id, -area_name, -state)
party_zweit$Andere <- as.integer(rowSums(andere_zweit))

full_zweit <- party_zweit %>% 
              select(area_id, area_name, state, top_parties, Andere) %>% 
              gather(key = "party", value = "second_vote", `CDU/CSU`:`Andere`)

#spread data set to combine FIRST votes of CDU and CSU #

party_erst <- party %>% select(-votes_second_vote) %>% 
              spread(party, votes_first_vote) %>% 
              mutate(`CDU/CSU` = CDU+CSU) %>%
              select(-c(CDU,CSU))

andere_erst <- select(party_erst, -top_parties, -area_id, -area_name, -state)
party_erst$Andere <- as.integer(rowSums(andere_erst))

full_erst <- party_erst %>% 
             select(area_id, area_name, state, top_parties, Andere) %>% 
             gather(key = "party", value = "first_vote", `CDU/CSU`:Andere)


final_parties <- full_erst %>% mutate(second_vote = full_zweit$second_vote)

#### Resultate Bund (Zweitstimmen) ####

#party colors (order with respect to final result): CDU/CSU, SPD, ... 
final_parties$party <- factor(final_parties$party, levels = c("CDU/CSU", "SPD", 
                                                              "AfD", "FDP", "Linke", 
                                                              "B90/Gruenen", "Andere"), 
                              ordered = T)

party_colors <- c("CDU/CSU" = "#000000", "SPD" = "#EB001F",  
                  "AfD" = "#009EE0", "FDP"= "#FFFF00", 
                  "Linke" = "#BE3075","B90/Gruenen" = "#64A12D", 
                  "Andere" = "grey")


germany_results <- final_parties %>% 
                   group_by(party) %>% 
                   summarise(Zweitstimmen = sum(second_vote) / Wahlbeteiligung_bund$Valide_Zweitstimmen * 100) %>%
                   arrange(desc(Zweitstimmen))

plot_germany <- germany_results %>% ggplot(aes(party, Zweitstimmen, fill = party)) + 
                   geom_col(color = "black") +
                   geom_hline(yintercept = 5, color = "grey", alpha = 0.8, size = 1) + 
                   geom_text(aes(label = round(Zweitstimmen, 1)),
                             position = position_dodge(0.9),
                             vjust = -0.3) +
                   ylim(c(0,40)) +
                   scale_fill_manual(values = party_colors) +
                   labs(x = element_blank(), 
                        y = element_blank(),
                        title = "Bund") +
                   theme(legend.position = "none",
                         plot.title = element_text(hjust = 0.5)) 



#### Importing and preparing map data #### 

districts <- st_read("Geometrie_Wahlkreise_19DBT.shp") %>% select(WKR_NR, geometry)


#### Direktmandate #####
votes_of_winner <- final_parties %>% group_by(area_id) %>% summarize(winner_votes = max(first_vote))

parties_spread <- final_parties %>% select(-second_vote) %>% spread(party, first_vote)

district_winner <- inner_join(parties_spread, votes_of_winner, by = "area_id") %>% 
                   mutate(`CDU/CSU` = ifelse(`CDU/CSU` == winner_votes, 1, 0),
                           SPD = ifelse(SPD == winner_votes, 1, 0),
                           AfD = ifelse(AfD == winner_votes, 1, 0),
                           FDP = ifelse(FDP == winner_votes, 1, 0),
                           Linke = ifelse(Linke == winner_votes, 1, 0),
                           `B90/Gruenen` = ifelse(`B90/Gruenen` == winner_votes, 1, 0),
                           Andere = ifelse(Andere == winner_votes, 1, 0)) %>% 
                   select(-winner_votes) %>% 
                   gather(party, key, `CDU/CSU`:Andere, factor_key = TRUE) %>% filter(key == 1) %>% 
                   select(-key) %>%
                   mutate(party = factor(party, ordered = T))

mp_first_votes <- inner_join(districts, district_winner, by = c("WKR_NR" = "area_id"), )

map_first_votes <- ggplot(mp_first_votes) + 
                    geom_sf(aes(fill = party), alpha = 0.8, lwd = 0.1) + 
                    scale_fill_manual(values = party_colors)

map_first_votes









