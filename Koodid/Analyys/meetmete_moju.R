#Meetmete mõju 
#Annegrete Molloka
#06.01.21

#paketid
library(zoo)
library(tidyverse)

#andmed
load("Andmed/R_andmed/df_koik.RData")


# Aasta tunnused ----------------------------------------------------------
df_aasta <- df_koik %>% 
  filter(aasta >= 2018) %>% 
  group_by(registrikood, aasta) %>% 
  summarise(kaive = sum(kaive),
            tootajad = mean(tootajad)) %>% 
  ungroup() %>% 
  #teeme tunnused kaive_2018, kaive_2019, kaive_2020, tootajad_2018, tootajad_2019, tootajad_2020
  pivot_wider(names_from = aasta, values_from = c(kaive, tootajad)) %>% 
  filter(!is.na(kaive_2020)) #%>% 
  # mutate(dkaive = kaive_2019/kaive_2018,
  #        dtootajad = tootajad_2019/tootajad_2018)

df_aasta2 <- df_koik %>% 
  group_by(registrikood) %>% 
  slice_tail(n = 1) %>% #viimane rida 
  ungroup() %>% 
  select(registrikood, nimi, maakond, emtaktahttekst, emtak2tekst, meede_EAS, meede_TK, meede_kredex, meede_MES) %>% 
  #asendame puuduvad väärtused tühja stringiga
  mutate(maakond = ifelse(is.na(maakond), "", maakond),
         emtak2tekst = ifelse(is.na(emtak2tekst), "", as.character(emtak2tekst)))

df_aasta <- df_aasta %>% 
  left_join(df_aasta2, by = "registrikood") 


#AV: tegin parameetriks
lubatudkaibeerinevus <- 0.2
lubatudtootajateerinevus <- 0.2

# Kredexi saajad ja sarnased ----------------------------------------------
kredex_saajad <- df_aasta %>% 
  filter(meede_kredex == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(kredex_saajad)) {
  #Kui ettevõttel on 5 või vähem töötajat, siis töötajate mõttes sarnaseks peetakse +-1 töötajaga ettevõtteid
  if (!is.na(kredex_saajad$tootajad_2019[i]) & kredex_saajad$tootajad_2019[i] <= 5) { 
    abi <- df_aasta %>% 
      filter(meede_kredex == 0, 
             maakond == kredex_saajad$maakond[i],
             emtak2tekst == kredex_saajad$emtak2tekst[i],
             kaive_2019 >= kredex_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= kredex_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= kredex_saajad$tootajad_2019[i]-1,
             tootajad_2019 <= kredex_saajad$tootajad_2019[i]+1
             # dkaive >= kredex_saajad$dkaive[i]*0.95,
             # dkaive <= kredex_saajad$dkaive[i]*1.05,
             # dtootajad >= kredex_saajad$dtootajad[i]*0.95,
             # dtootajad <= kredex_saajad$dtootajad[i]*1.05,
             # meede_EAS == kredex_saajad$meede_EAS[i],
             # meede_TK == kredex_saajad$meede_TK[i],
             # meede_MES == kredex_saajad$meede_MES[i]
      ) %>% 
      pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_kredex == 0, 
             maakond == kredex_saajad$maakond[i],
             #emtaktahttekst == kredex_saajad$emtaktahttekst[i],
             emtak2tekst == kredex_saajad$emtak2tekst[i],
             kaive_2019 >= kredex_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= kredex_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= kredex_saajad$tootajad_2019[i]*(1-lubatudtootajateerinevus),
             tootajad_2019 <= kredex_saajad$tootajad_2019[i]*(1+lubatudtootajateerinevus)
             # dkaive >= kredex_saajad$dkaive[i]*0.95,
             # dkaive <= kredex_saajad$dkaive[i]*1.05,
             # dtootajad >= kredex_saajad$dtootajad[i]*0.95,
             # dtootajad <= kredex_saajad$dtootajad[i]*1.05,
             # meede_EAS == kredex_saajad$meede_EAS[i],
             # meede_TK == kredex_saajad$meede_TK[i],
             # meede_MES == kredex_saajad$meede_MES[i]
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    kredex_saajad$sarnased[i] <- toString(abi)    
    kredex_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(kredex_saajad$sarnaste_arv != 0)/nrow(kredex_saajad)

# EASi saajad ja sarnased -------------------------------------------------
EAS_saajad <- df_aasta %>% 
  filter(meede_EAS == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(EAS_saajad)) {
  if (!is.na(EAS_saajad$tootajad_2019[i]) & EAS_saajad$tootajad_2019[i] <= 5) {
  abi <- df_aasta %>% 
    filter(meede_EAS == 0, 
           maakond == EAS_saajad$maakond[i],
           emtak2tekst == EAS_saajad$emtak2tekst[i],
           kaive_2019 >= EAS_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
           kaive_2019 <= EAS_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
           tootajad_2019 >= EAS_saajad$tootajad_2019[i]-1,
           tootajad_2019 <= EAS_saajad$tootajad_2019[i]+1
           # dkaive >= EAS_saajad$dkaive[i]*0.95,
           # dkaive <= EAS_saajad$dkaive[i]*1.05,
           # dtootajad >= EAS_saajad$dtootajad[i]*0.95,
           # dtootajad <= EAS_saajad$dtootajad[i]*1.05,
           # meede_kredex == EAS_saajad$meede_kredex[i],
           # meede_TK == EAS_saajad$meede_TK[i],
           # meede_MES == EAS_saajad$meede_MES[i]
    ) %>% 
    pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_EAS == 0, 
             maakond == EAS_saajad$maakond[i],
             emtak2tekst == EAS_saajad$emtak2tekst[i],
             kaive_2019 >= EAS_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= EAS_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= EAS_saajad$tootajad_2019[i]*(1-lubatudtootajateerinevus),
             tootajad_2019 <= EAS_saajad$tootajad_2019[i]*(1+lubatudtootajateerinevus)
             # dkaive >= EAS_saajad$dkaive[i]*0.95,
             # dkaive <= EAS_saajad$dkaive[i]*1.05,
             # dtootajad >= EAS_saajad$dtootajad[i]*0.95,
             # dtootajad <= EAS_saajad$dtootajad[i]*1.05,
             # meede_kredex == EAS_saajad$meede_kredex[i],
             # meede_TK == EAS_saajad$meede_TK[i],
             # meede_MES == EAS_saajad$meede_MES[i]
      ) %>% 
      pull(registrikood)
  }
  #print(abi)
  if (length(abi) != 0){
    EAS_saajad$sarnased[i] <- toString(abi)  
    EAS_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(EAS_saajad$sarnaste_arv != 0)/nrow(EAS_saajad)

# MES saajad ja sarnased --------------------------------------------------
MES_saajad <- df_aasta %>% 
  filter(meede_MES == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(MES_saajad)) {
  if (!is.na(MES_saajad$tootajad_2019[i]) & MES_saajad$tootajad_2019[i] <= 5) {
  abi <- df_aasta %>% 
    filter(meede_MES == 0, 
           maakond == MES_saajad$maakond[i],
           emtak2tekst == MES_saajad$emtak2tekst[i],
           kaive_2019 >= MES_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
           kaive_2019 <= MES_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
           tootajad_2019 >= MES_saajad$tootajad_2019[i]-1,
           tootajad_2019 <= MES_saajad$tootajad_2019[i]+1
           # dkaive >= MES_saajad$dkaive[i]*0.95,
           # dkaive <= MES_saajad$dkaive[i]*1.05,
           # dtootajad >= MES_saajad$dtootajad[i]*0.95,
           # dtootajad <= MES_saajad$dtootajad[i]*1.05,
           # meede_kredex == MES_saajad$meede_kredex[i],
           # meede_TK == MES_saajad$meede_TK[i],
           # meede_EAS == MES_saajad$meede_EAS[i]
    ) %>% 
    pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_MES == 0, 
             maakond == MES_saajad$maakond[i],
             emtak2tekst == MES_saajad$emtak2tekst[i],
             kaive_2019 >= MES_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= MES_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= MES_saajad$tootajad_2019[i]*(1-lubatudtootajateerinevus),
             tootajad_2019 <= MES_saajad$tootajad_2019[i]*(1+lubatudtootajateerinevus)
             # dkaive >= MES_saajad$dkaive[i]*0.95,
             # dkaive <= MES_saajad$dkaive[i]*1.05,
             # dtootajad >= MES_saajad$dtootajad[i]*0.95,
             # dtootajad <= MES_saajad$dtootajad[i]*1.05,
             # meede_kredex == MES_saajad$meede_kredex[i],
             # meede_TK == MES_saajad$meede_TK[i],
             # meede_EAS == MES_saajad$meede_EAS[i]
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    MES_saajad$sarnased[i] <- toString(abi)  
    MES_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(MES_saajad$sarnaste_arv != 0)/nrow(MES_saajad)

# TK saajad ja sarnased ---------------------------------------------------

TK_saajad <- df_aasta %>% 
  filter(meede_TK == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(TK_saajad)) {
  if (!is.na(TK_saajad$tootajad_2019[i]) & TK_saajad$tootajad_2019[i] <= 5) {
  abi <- df_aasta %>% 
    filter(meede_TK == 0, 
           maakond == TK_saajad$maakond[i],
           emtak2tekst == TK_saajad$emtak2tekst[i],
           kaive_2019 >= TK_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
           kaive_2019 <= TK_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
           tootajad_2019 >= TK_saajad$tootajad_2019[i]-1,
           tootajad_2019 <= TK_saajad$tootajad_2019[i]+1
           # dkaive >= TK_saajad$dkaive[i]*0.95,
           # dkaive <= TK_saajad$dkaive[i]*1.05,
           # dtootajad >= TK_saajad$dtootajad[i]*0.95,
           # dtootajad <= TK_saajad$dtootajad[i]*1.05,
           # meede_kredex == TK_saajad$meede_kredex[i],
           # meede_MES == TK_saajad$meede_MES[i],
           # meede_EAS == TK_saajad$meede_EAS[i]
    ) %>% 
    pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_TK == 0, 
             maakond == TK_saajad$maakond[i],
             emtak2tekst == TK_saajad$emtak2tekst[i],
             kaive_2019 >= TK_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= TK_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= TK_saajad$tootajad_2019[i]*(1-lubatudtootajateerinevus),
             tootajad_2019 <= TK_saajad$tootajad_2019[i]*(1+lubatudtootajateerinevus)
             # dkaive >= TK_saajad$dkaive[i]*0.95,
             # dkaive <= TK_saajad$dkaive[i]*1.05,
             # dtootajad >= TK_saajad$dtootajad[i]*0.95,
             # dtootajad <= TK_saajad$dtootajad[i]*1.05,
             # meede_kredex == TK_saajad$meede_kredex[i],
             # meede_MES == TK_saajad$meede_MES[i],
             # meede_EAS == TK_saajad$meede_EAS[i]
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    TK_saajad$sarnased[i] <- toString(abi)  
    TK_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(TK_saajad$sarnaste_arv != 0)/nrow(TK_saajad)

# Andmed kokku ------------------------------------------------------------

df_meetmed <- kredex_saajad %>% mutate(meede = "Kredex") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv) %>% 
  rbind(EAS_saajad %>% mutate(meede = "EAS") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv)) %>% 
  rbind(MES_saajad %>% mutate(meede = "MES") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv)) %>% 
  rbind(TK_saajad %>% mutate(meede = "Töötukassa") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv))
save(df_meetmed, file = "Andmed/R_andmed/df_meetmed.RData")
