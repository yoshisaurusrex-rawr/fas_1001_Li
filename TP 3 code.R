#importing packages to prepare for text analysis

library(fs)          
library(crayon)      
library(quanteda)    
library(tidyverse)   
library(clessnverse)
library(dplyr)
install.packages("textTinyR")
library(textTinyR)

#importation de données et dictionaire

debate_trans <- read.csv("/Users/yoshili/fas_1001_Li/lipad/2019/5/2019-5-3.csv") 

lexicoder_eng <- dictionary(file = "/Users/yoshili/fas_1001_Li/policy_agendas_english.lcd")

speechtext_list <- as.list(debate_trans$speechtext)
debate_dictionary <- dictionary(speechtext_list) #transform debate_trans into dictionary

#création d'un autre dictionnaire

first_nations_dictionary <- list(issues = c("indigenous child", "families", "separation", 
                                            "family reunification", "reform", "Jordan's principle", 
                                            "child and family services", "youth", "apprehended"),
                                 truth = c("truth and reconciliation commitee", "reconciliation", 
                                           "keeping families together", "cultural experiences", 
                                           "legislation", "bills", "laws", "injustice", 
                                           "historical injustice", "self-determination", "self-governance"),
                                 aboriginal = c("first nations", "Inuit", "Métis")) |>
  dictionary()

#fusion de lexicoder et nouveau dico

first_nations_dictionary_m <- first_nations_dictionary |> stack() #dictionary to databse

lexicoder_eng_m <- lexicoder_eng |> stack() #dictionary into database 

new_dico <- bind_rows(first_nations_dictionary_m, lexicoder_eng_m) |> #combine into one dictionary
  unstack(values~ind) |> #transformation into list
  dictionary() #then dictionary

#nettoyage de base et sélection des données

debate_trans_preclean <- debate_trans |> 
  select(speechtext, speakerparty) |> #garder seulement speechtext et speakerparty
  mutate(speechtext = tolower(speechtext)) |> #converting speechtext values into lowercase
  na.omit() #omitting NA values

#removing stopwords

english_stopwords <- stopwords(kind = "en")
debate_dictionary <- removeWords(debate_dictionary, english_stopwords)

#text analysis by speaker party

run_dictionary(data = debate_trans_preclean, 
               text = speechtext, 
               dictionary = lexicoder_eng_m) |> 
  bind_cols(debate_trans_preclean) |> 
  select(-c(doc_id,speechtext)) |> 
  pivot_longer(!speakerparty, names_to = "categorie", values_to = "n") |>
  #assigning speakerparty to a categorie, values with n
  #pivot_longer()shapes words from wide to long, basically shortening it to its root
  ungroup() |> 
  group_by(speakerparty, categorie) |> #grouping speakerparty and categorie 
  summarise(n=sum(n)) |> #sum of values in category 'n'
  mutate(prop = round(n/sum(n),3)*100, #creating variable prop (proportion) and calculating relative to total sum 'n', rounding to 3 decimal places, converting to percentage
         speakerparty = case_when(speakerparty == "Conservative"          ~ "Conservateur", #converting english term into french
                                  speakerparty == "Green Party"           ~ "Partie vert",
                                  speakerparty == "New Democratic Party"  ~ "NDP",
                                  speakerparty == "Liberal"               ~ "Libéral",
                                  T ~ as.character(speakerparty)), #converting speakerparty to character type
         categorie = case_when(categorie == "issues" ~ "Enjeux",
                               categorie == "truth" ~ "Vérité",
                               categorie == "aboriginal" ~ "Autochtone",
                               T ~ as.character(categorie))) |> #converting lexicoder categories into character type
  na.omit() |> #omitting NA values
  filter(categorie %in% c("Enjeux", "Vérité", "Autochtone"),
         !speakerparty == "Independent") |> 
  ggplot(aes(x = categorie, y = prop, fill = speakerparty)) + #creating point graph, 
  geom_bar(stat = "identity", position = "dodge") + #fitting regression lines
  scale_fill_manual(values = c("skyblue", "purple", "pink")) +
  coord_flip() + #flipping coordinates
  labs(x = "Enjeux discutés",
       y = "Proportion en %",
       title = "Importance des enjeux des Premières nations, Inuit et Métis") +
  theme_light() +
  theme(title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20, color = "black"))
