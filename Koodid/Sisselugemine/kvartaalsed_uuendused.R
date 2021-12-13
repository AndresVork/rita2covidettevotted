#Seda faili ei kasuta
#Võimalus oleks teha kvartaalsete andmete lisamine eraldi failis, 
#aga et peab nagunii käsitsi kontrollima eraldi uute andmefailide struktuuri ja tunnuste nimetusi, siis ei ole automatiseerimisel suurt võitu

#paketid
# library(tidyverse)
# library(readxl)
# library(stringr)
# library(data.table)

#EMTA kvartaalsete andmete lisamine  - ei kasuta
#Printsiibina võiks teha nii, et loeme sisse "andmed_emta.RData" ja lisame vaid uute andmete read

# load("Andmed/R_andmed/andmed_emta.RData")
# andmed <- read_excel("Andmed/EMTA/ pane siia uus andmefaili nimi") %>% 
#   mutate(aasta = 2021,  #pane aasta
#          kvartal = 2)   #pane kvartal
# names(andmed) <- tolower(names(andmed))
# names(andmed)  #kontrolli, et veeru pealkirjad Excelis vastavad allolevatele nimedele
# andmed <- andmed %>% 
#   rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
#          emtak_emta = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
#          rmaksud = `riiklikud maksud`,
#          toomaksud = `tööjõumaksud ja maksed`,
#          tootajad = tootajaid
#   )  %>% select(names(andmed_emta))
# andmed_emta <- andmed_emta %>% 
#   rbind(andmed)
# 
# save(andmed_emta, file = "Andmed/R_andmed/andmed_emta.RData")

# #Äriregistri andmete uuendamine
# load("Andmed/R_andmed/andmed_ari.RData")
# andmed_ari_vana <- andmed_ari
# 
# andmed_ari <- fread("Andmed/Ariregister/ettevotja_rekvisiidid_2021-07-07.csv", encoding = "UTF-8" )
# # andmed_ari_vana <- fread("Andmed/Ariregister/ettevotja_rekvisiidid_2020-10-07.csv", encoding = "UTF-8" ) %>% 
# #   filter(!(ariregistri_kood %in% andmed_ari$ariregistri_kood)) #3485 rida
# andmed_ari_vana <- andmed_ari_vana %>% 
#   filter(!(registrikood %in% andmed_ari$ariregistri_kood)) #5157 rida
# 
# 
# andmed_ari <- andmed_ari %>% 
#   select(nimi, registrikood = ariregistri_kood, vorm = ettevotja_oiguslik_vorm, kmkr_nr, 
#          staatus = ettevotja_staatus_tekstina, asutamise_kp = ettevotja_esmakande_kpv, asukoht = asukoha_ehak_tekstina) %>% 
#   rbind(andmed_ari_vana)
# save(andmed_ari, file = "Andmed/R_andmed/andmed_ari.RData")

