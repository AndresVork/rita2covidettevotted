#Äriregistri ja ESA andmete sisselugemine

#Logi
#23.11.2020 Annegrete Molloka
#06.12.2020 AV ESA lisaandmed

#paketid
library(tidyverse)
library(data.table)
library(readxl)


#Äriregister
andmed_ari <- fread("Andmed/Ariregister/ettevotja_rekvisiidid_2020-10-07.csv", encoding = "UTF-8" ) 
andmed_ari <- andmed_ari %>% 
  select(nimi, registrikood = ariregistri_kood, vorm = ettevotja_oiguslik_vorm, kmkr_nr, 
         staatus = ettevotja_staatus_tekstina, asutamise_kp = ettevotja_esmakande_kpv, asukoht = asukoha_ehak_tekstina)
save(andmed_ari, file = "Andmed/R_andmed/andmed_ari.RData")

#ESA
andmed_esa <- read_excel("Andmed/ESAuksused/yksuste_nimekiri_01.12.2019_revid.23.01.20.xlsx") 
andmed_esa <- andmed_esa %>% 
  select(registrikood = Registrikood, 
         emtak_taht = `EMTAK2008 Tähtkoodiga valdkond`, 
         emtak4 = `EMTAK2008 4-kohaline kood`,
         registrkpv = `Üksuse registreerimise kuupäev`,
         kustutkpv = `Üksuse registrist kustutamise kuupäev`) %>% 
  mutate(registrkpv = as.Date(registrkpv),
         kustutkpv = as.Date(kustutkpv),
         registrikood = as.character(registrikood))

save(andmed_esa, file = "Andmed/R_andmed/andmed_esa.RData")
