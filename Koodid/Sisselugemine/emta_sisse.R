#EMTA andmete sisselugemine

#Sisendid:
# Kõik kvartaalsed EMTA kodulehelt alla laaditud andmed kas Excelis või (vahel harva) csv kujul.

#Vajalikud tegevused uute andmete lisandumisel
#1) lisada lõppu blokk iga uue kvartali kohta 
#2) kontrollida üle, et sisendandmete nimed vastavad koodis toodule
#3) Jooksutada kogu kood läbi

#Esimesed EMTA andmed loetakse sisse andmetabelisse "andmed", 
#järgmised kõik üle kirjutatavasse andmetabelisse "lisaandmed" ja lisatakse eelmisele tabelile
#

#Väljundid:
#Koodi jooksutamise tulemusena salvestatakse andmetabel "andmed_emta" faili "Andmed/R_andmed/andmed_emta.RData"

#paketid
library(tidyverse)
library(readxl)
library(stringr)

#EMTA andmed
#(failid <- list.files("Andmed/EMTA"))

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

#2021
lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2021_i_kvartal.xlsx") %>% 
  mutate(aasta = 2021, 
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

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2021_ii_kvartal.xlsx") %>% 
  mutate(aasta = 2021, 
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

lisa_andmed <- read_excel("Andmed/EMTA/tasutud_maksud_2021_iii_kvartal.xlsx") %>% 
  mutate(aasta = 2021, 
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


#Õ täht on varasemates Exceli andmefailides teistsuguse kujuga. Kasutan gsub ja pattern kombinatsiooni.
#Vaata: table(andmed$EMTAK, andmed$aasta)
#Siin tehakse uus nimetus "emtak_emta"
andmed_emta <- andmed %>% 
  mutate(emtak_emta =  gsub(pattern = ".HUGA", replacement = "ÕHUGA", EMTAK),
         emtak_emta =  gsub(pattern = "S.IDUKITE", replacement = "SÕIDUKITE", emtak_emta),
         emtak_emta =  gsub(pattern = "M.ELDUD", replacement = "MÕELDUD", emtak_emta),
         emtak_emta =  gsub(pattern = "P.LLUMAJANDUS", replacement = "PÕLLUMAJANDUS", emtak_emta),
         emtak_emta = str_to_sentence(emtak_emta))  %>% #Väiketähtedeks  
  select(registrikood, nimi, liik, KMK, maakond, rmaksud, toomaksud, kaive, tootajad, aasta, kvartal, emtak_emta)

#mahakirjutamine
save(andmed_emta, file = "Andmed/R_andmed/andmed_emta.RData")
