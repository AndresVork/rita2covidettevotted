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

#Kredex
#eelmised andmed "Kriisiabi_käenduslepingud_13082020.csv" ja "Kriisiabi_laenulepingud_13082020.csv"
andmed_kredex <- read.csv2("Andmed/Meetmed/Kredex/Kriisiabi_käenduslepingud_31012021.csv") %>% 
  select(registrikood = Toetuse.saaja.registrikood, kredex_meede = Meetme.nimetus, kredex_teenus = Teenuse.nimetus, 
         kredex_laenusumma = Laenusumma, kredex_kaendussumma = Käenduse.summa, kredex_kp = Lepingu.sõlmimise.kuupäev)
andmed_kredex2 <- read.csv2("Andmed/Meetmed/Kredex/Kriisiabi_laenulepingud_31012021.csv") %>% 
  select(registrikood = Toetuse.saaja.registrikood, kredex_meede = Meetme.nimetus, kredex_teenus = Teenuse.nimetus, 
         kredex_laenusumma = Laenusumma, kredex_kp = Lepingu.sõlmimise.kuupäev)
andmed_kredex <- andmed_kredex %>% 
  plyr::rbind.fill(andmed_kredex2) 
andmed_kredex <- andmed_kredex %>% 
  mutate(kredex_laenusumma = as.numeric(gsub(" ", "",gsub(",", ".", kredex_laenusumma))),
         kredex_kaendussumma = as.numeric(gsub(" ", "",gsub(",", ".", kredex_kaendussumma))))
save(andmed_kredex, file="Andmed/R_andmed/andmed_kredex.RData")

#MES
#eelmised andmed "Info kodulehele 27.11.2020.xls, range = "B3:C255" ja "G3:H76"
andmed_mes <- read_excel("Andmed/Meetmed/MES/Info COVID kohta seisuga 28.02.2021.xls", sheet = "COVID-19 laen", range = "B2:D302") %>% 
  select(nimi = `Laenu saaja`, registrikood = `reg nr`, mes_laenusumma = laenusumma)
andmed_mes2 <- read_excel("Andmed/Meetmed/MES/Info COVID kohta seisuga 28.02.2021.xls", sheet = "COVID-19 laen", range = "H3:I137") %>% 
  select(nimi = `Käenduse saaja`, mes_kaendussumma= `käenduse summa`)
andmed_mes <- andmed_mes %>% 
  plyr::rbind.fill(andmed_mes2) 
save(andmed_mes, file="Andmed/R_andmed/andmed_mes.RData")


#TK 2020
#Töötasu hüvitist maksti esimest korda 2020. aasta märtsi, aprilli ja mai eest
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

#TK 2021
#Töötasu hüvitist maksti 2021. aasta märtsi, aprilli ja mai eest
#Eelmised versioonid "tootasu_huvitis_2021_asutuste_nimekiri_25.04.2021.xlsx"
andmed_tk21 <- read_excel("Andmed/Meetmed/TK/tootasu_huvitis_2021_asutuste_nimekiri_06.12.2021.xlsx", 
                          sheet = "Asutuste kokkuvõte_koond2021", skip = 1) %>% 
  select(registrikood = `Asutuse registrikood`, tk21_saajate_arv = `Hüvitise saajate arv`,
         tk21_brutosumma = `Hüvitiste brutosumma EUR`, tk21_kogukulu = `Hüvitiste kogukulu EUR`)
save(andmed_tk21, file="Andmed/R_andmed/andmed_tk21.RData")

#TK 2021 Harju ja Ida-Viru 
#eelmised andmefaili versioonid: asutuste_nimekiri_07_04_2021.xlsx
#Harjumaa ja Ida-Virumaa tööandjatele, kelle tegevus on olnud erakorraliste asjaolude tõttu perioodil 
#28. detsember 2020 kuni 31. jaanuar 2021 märkimisväärselt häiritud.
#NB! Töötukassa andmetel on ka ettevõtteid väljaspool Harjumaad ja Ida-Virumaad, mis on võimalik, kui töötaja töökoht oli siiski neis maakondades
andmed_tk21_hi <- read_excel("Andmed/Meetmed/TK/tootasu_toetuse_asutuste_nimekiri_06_12_2021.xlsx", 
                             sheet = "Asutuste koondvaade", skip = 1) %>% 
  select(registrikood = `Asutuse registrikood`, tk21_hi_saajate_arv = `Saajate arv`,
         tk21_hi_summa = `Määratud summa`)
save(andmed_tk21_hi, file="Andmed/R_andmed/andmed_tk21_hi.RData")
