#global.R

#paketid
library(shiny)
library(dplyr)
library(DT)
library(shinyWidgets)
library(zoo)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)

#andmed
load(file = "../../Andmed/R_andmed/df_koik.RData")
load(file = "../../Andmed/R_andmed/df_meetmed.RData")

#plusmiinus sümbol
plusmiinus <- "\u00b1"
Encoding(plusmiinus) <- "UTF-8"

#tunnused
df_koik <- df_koik %>% 
  mutate(emtaktahttekst = droplevels(emtaktahttekst), #jätame alles ainult esinevad tasemed
         emtak2tekst = droplevels(emtak2tekst))
sektorid <- sort(unique(df_koik$emtaktahttekst))
emtak2 <- sort(unique(df_koik$emtak2tekst))
maakonnad <- sort(unique(df_koik$maakond))

max_kaive <- df_koik %>% filter(aasta == 2019, liider_kood != 1) %>% #meetme lehel ei kuvata grupeeritud ridu
  group_by(registrikood) %>% summarise(aasta_kaive = sum(kaive))%>%
  pull(aasta_kaive) %>% max()
max_tootajad <- df_koik %>% filter(aasta == 2019, liider_kood != 1) %>% #meetme lehel ei kuvata grupeeritud ridu
  group_by(registrikood) %>% summarise(aasta_tootajad = mean(tootajad))%>%
  pull(aasta_tootajad) %>% max()

ettevotte_nimekiri <- df_koik %>% 
  filter(liider_kood != 1) %>% #viskame grupeeritud read välja
  group_by(registrikood) %>% 
  summarise(nimi = last(nimi)) %>% #võtame igale registrikoodile viimase kvartali nime
  ungroup() %>% 
  rbind(df_koik %>% 
          filter(liider_kood == 1) %>% 
          group_by(registrikood) %>% 
          summarise(nimi = last(nimi))) %>% #lisame grupi valikud
  arrange(nimi) %>% 
  select(registrikood, nimi)

tunnusedjarjestamiseks <- c("rmaksud", "toomaksud", "kaive", "tootajad", 
                            "toomaksud_tootajakohta", "kaive_tootajakohta", 
                            "kaive_kasv4", "tootajate_kasv4", "toomaksud_kasv4", "toomaksud_tootajakohta_kasv4",
                            "dkaive", "dtootajad", "dtoomaksud", "dtoomaksud_tootajakohta", 
                            "kaive_olulisus_valdlinn", "tootajad_olulisus_valdlinn", "rmaksud_olulisus_valdlinn", "toomaksud_olulisus_valdlinn", 
                            "kaive_olulisus_maakond", "tootajad_olulisus_maakond", "rmaksud_olulisus_maakond", "toomaksud_olulisus_maakond", 
                            "kaive_olulisus_riik", "tootajad_olulisus_riik", "rmaksud_olulisus_riik", "toomaksud_olulisus_riik", 
                            "haru_myyjaid", "haru_ostjaid")