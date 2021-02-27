#EMTA andmete sisselugemine

#paketid
library(tidyverse)
library(readxl)
library(stringr)

#EMTA andmed
#failid <- list.files("Andmed/EMTA")

#2017
andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2017_i_kv.xlsx") %>% 
  mutate(aasta = 2017, 
         kvartal = 1)
names(andmed) <- tolower(names(andmed))
andmed <- andmed %>% 
    rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         kaive = käive,
         tootajad = `töötajate arv`
  )
str(andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_05_07_2017.xlsx") %>% 
  mutate(aasta = 2017, 
         kvartal = 2)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_09.10.2017.xlsx") %>% 
  mutate(aasta = 2017, 
         kvartal = 3)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_10.01.2018.xlsx") %>% 
  mutate(aasta = 2017, 
         kvartal = 4)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

#2018
lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2018_i_kvartal.xlsx") %>% 
  mutate(aasta = 2018, 
         kvartal = 1)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2018_ii_kvartal.xlsx") %>% 
  mutate(aasta = 2018, 
         kvartal = 2)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2018_iii_kvartal.xlsx") %>% 
  mutate(aasta = 2018, 
         kvartal = 3)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2018_iv_kvartal.xlsx") %>% 
  mutate(aasta = 2018, 
         kvartal = 4)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

#2019
lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2019_i_kvartal.xlsx") %>% 
  mutate(aasta = 2019, 
         kvartal = 1)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  ) %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2019_ii_kvartal.xlsx") %>% 
  mutate(aasta = 2019, 
         kvartal = 2)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  ) %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2019_iii_kvartal.xlsx") %>% 
  mutate(aasta = 2019, 
         kvartal = 3)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  ) %>% select(names(andmed)) 
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2019_iv_kvartal.xlsx") %>% 
  mutate(aasta = 2019, 
         kvartal = 4)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  ) %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

#2020
lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2020_i_kvartal.xlsx") %>% 
  mutate(aasta = 2020, 
         kvartal = 1)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2020_ii_kvartal.xlsx") %>% 
  mutate(aasta = 2020, 
         kvartal = 2)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2020_iii_kvartal.xlsx") %>% 
  mutate(aasta = 2020, 
         kvartal = 3)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2020_iv_kvartal.xlsx") %>% 
  mutate(aasta = 2020, 
         kvartal = 4)
names(lisa_andmed) <- tolower(names(lisa_andmed))
names(lisa_andmed)
lisa_andmed <- lisa_andmed %>% 
  rename(KMK = `registreeritud käibemaksukohustuslaste registrisse`,
         EMTAK = `emtak tegevusvaldkond, mis on emtaki struktuuris tähistatud tähtkoodiga`,
         rmaksud = `riiklikud maksud`,
         toomaksud = `tööjõumaksud ja maksed`,
         tootajad = tootajaid
  )  %>% select(names(andmed))
str(lisa_andmed)
andmed <- andmed %>% 
  rbind(lisa_andmed)

#Õ täht on varasemates Exceli andmefailides teistsuguse kujuga. Kasutan gsub ja pattern kombinatsiooni.
#Vaata: table(andmed$EMTAK, andmed$aasta)
andmed_emta <- andmed %>% 
  mutate(emtak_emta =  gsub(pattern = ".HUGA", replacement = "ÕHUGA", EMTAK),
         emtak_emta =  gsub(pattern = "S.IDUKITE", replacement = "SÕIDUKITE", emtak_emta),
         emtak_emta =  gsub(pattern = "M.ELDUD", replacement = "MÕELDUD", emtak_emta),
         emtak_emta =  gsub(pattern = "P.LLUMAJANDUS", replacement = "PÕLLUMAJANDUS", emtak_emta),
         emtak_emta = str_to_sentence(emtak_emta))  %>% #Väiketähtedeks  
  select(registrikood, nimi, liik, KMK, maakond, rmaksud, toomaksud, kaive, tootajad, aasta, kvartal, emtak_emta)

#mahakirjutamine
save(andmed_emta, file = "Andmed/R_andmed/andmed_emta.RData")
