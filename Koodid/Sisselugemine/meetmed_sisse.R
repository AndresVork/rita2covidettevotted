#Meetme andmed 
#Annegrete Molloka
#16.11.2020

#paketid
library(tidyverse)
library(readxl)
#library(plyr) #kasutame seda paketti real 42, aga seda ei tasu terve paketina 
#sisse lugeda, sest tekib konflikt dplyr paketi funktsioonidega


#EAS
#eelmised andmed "Kriisitoetused NAVist 14.08.2020.xlsx"
andmed_eas <- read_excel("Andmed/Meetmed/EAS/Kriisitoetused NAVist 23.02.2021.xlsx", sheet = "Kohalik")
andmed_eas <- read_excel("Andmed/Meetmed/EAS/Kriisitoetused NAVist 23.02.2021.xlsx", sheet = "SF") %>% 
  rbind(andmed_eas) 
names(andmed_eas) <- c("sk_nr", "eas_skeem", "nimi", "registrikood", "eas_kp", "eas_summa")
andmed_eas <- andmed_eas %>% select(registrikood, eas_skeem, eas_summa, eas_kp)
save(andmed_eas, file="Andmed/R_andmed/andmed_eas.RData")

#HTM

#Kredex
andmed_kredex <- read.csv2("Andmed/Meetmed/Kredex/Kriisiabi_käenduslepingud_13082020.csv") %>% 
  select(registrikood = Toetuse.saaja.registrikood, kredex_meede = Meetme.nimetus, kredex_teenus = Teenuse.nimetus, 
         kredex_laenusumma = Laenusumma, kredex_kaendussumma = Käenduse.summa, kredex_kp = Lepingu.sõlmimise.kuupäev)
andmed_kredex2 <- read.csv2("Andmed/Meetmed/Kredex/Kriisiabi_laenulepingud_13082020.csv") %>% 
  select(registrikood = Toetuse.saaja.registrikood, kredex_meede = Meetme.nimetus, kredex_teenus = Teenuse.nimetus, 
         kredex_laenusumma = Laenusumma, kredex_kp = Lepingu.sõlmimise.kuupäev)
andmed_kredex <- andmed_kredex %>% 
  plyr::rbind.fill(andmed_kredex2) 
andmed_kredex <- andmed_kredex %>% 
  mutate(kredex_laenusumma = as.numeric(gsub(" ", "",gsub(",", ".", kredex_laenusumma))),
         kredex_kaendussumma = as.numeric(gsub(" ", "",gsub(",", ".", kredex_kaendussumma))))
save(andmed_kredex, file="Andmed/R_andmed/andmed_kredex.RData")

#MES
andmed_mes <- read_excel("Andmed/Meetmed/MES/Info kodulehele 27.11.2020.xls", sheet = "COVID-19 laen", range = "B3:C255") %>% 
  select(nimi = `Laenu saaja`, mes_laenusumma = laenusumma)
andmed_mes2 <- read_excel("Andmed/Meetmed/MES/Info kodulehele 27.11.2020.xls", sheet = "COVID-19 laen", range = "G3:H76") %>% 
  select(nimi = `Käenduse saaja`, mes_kaendussumma= `käenduse summa`)
andmed_mes <- andmed_mes %>% 
  plyr::rbind.fill(andmed_mes2) 
save(andmed_mes, file="Andmed/R_andmed/andmed_mes.RData")


#TK
andmed_tk <- read_excel("Andmed/Meetmed/TK/asutuste_nimekiri_06.09.2020.xlsx", sheet = "Asutuste kokkuvõte_märts", skip = 1)
andmed_tk <- read_excel("Andmed/Meetmed/TK/asutuste_nimekiri_06.09.2020.xlsx", sheet = "Asutuste kokkuvõte_aprill", skip = 1) %>% 
  rbind(andmed_tk)
andmed_tk <- read_excel("Andmed/Meetmed/TK/asutuste_nimekiri_06.09.2020.xlsx", sheet = "Asutuste kokkuvõte_mai", skip = 1) %>% 
  rbind(andmed_tk)
andmed_tk <- read_excel("Andmed/Meetmed/TK/asutuste_nimekiri_06.09.2020.xlsx", sheet = "Asutuste kokkuvõte_juuni", skip = 1) %>% 
  rbind(andmed_tk)
andmed_tk <- andmed_tk %>% 
  select(registrikood = `Asutuse registrikood`, tk_kuu = `Kuu, mille eest hüvitis määrati`, tk_saajate_arv = `Hüvitise saajate arv`, 
         tk_brutosumma = `Hüvitiste brutosumma EUR`, tk_kogukulu = `Hüvitiste kogukulu EUR`)
save(andmed_tk, file="Andmed/R_andmed/andmed_tk.RData")

andmed_tkkum <- read_excel("Andmed/Meetmed/TK/asutuste_nimekiri_06.09.2020.xlsx", sheet = "Asutuste kokkuvõte_koond", skip = 1)
andmed_tkkum <- andmed_tkkum %>% 
  select(registrikood = `Asutuse registrikood`, tkkum_saajate_arv = `Hüvitise saajate arv`, 
         tkkum_saajate_arv1 = `Sh hüvitise saajad, kes saanud hüvitist ühe kuu eest`,
         tkkum_saajate_arv2 = `Sh hüvitise saajad, kes saanud hüvitist kahe kuu eest`,
         tkkum_saajate_arv3 = `Sh hüvitise saajad, kes saanud hüvitist kolme kuu eest`,
         tkkum_brutosumma = `Hüvitiste brutosumma EUR`, tkkum_kogukulu = `Hüvitiste kogukulu EUR`)
save(andmed_tkkum, file="Andmed/R_andmed/andmed_tkkum.RData")
