#Meetmete mõju 
#Annegrete Molloka
#06.01.21

#paketid
require(zoo)
require(tidyverse)

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
  select(registrikood, nimi, maakond, emtaktahttekst, emtak2tekst, meede_EAS, 
         meede_TK20, meede_TK21, meede_TK21_hi, meede_kredex_kaendus,
         meede_kredex_laen, meede_MES_kaendus, meede_MES_laen) %>% 
  #asendame puuduvad väärtused tühja stringiga
  mutate(maakond = ifelse(is.na(maakond), "", maakond),
         emtak2tekst = ifelse(is.na(emtak2tekst), "", as.character(emtak2tekst)))

df_aasta <- df_aasta %>% 
  left_join(df_aasta2, by = "registrikood") 


#AV: tegin parameetriks
lubatudkaibeerinevus <- 0.2
lubatudtootajateerinevus <- 0.2

# Kredexi saajad ja sarnased ----------------------------------------------
kredex_kaendus_saajad <- df_aasta %>% 
  filter(meede_kredex_kaendus == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(kredex_kaendus_saajad)) {
  #Kui ettevõttel on 5 või vähem töötajat, siis töötajate mõttes sarnaseks peetakse +-1 töötajaga ettevõtteid
  if (!is.na(kredex_kaendus_saajad$tootajad_2019[i]) & kredex_kaendus_saajad$tootajad_2019[i] <= 5) { 
    abi <- df_aasta %>% 
      filter(meede_kredex_kaendus == 0, 
             maakond == kredex_kaendus_saajad$maakond[i],
             emtak2tekst == kredex_kaendus_saajad$emtak2tekst[i],
             kaive_2019 >= kredex_kaendus_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= kredex_kaendus_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= kredex_kaendus_saajad$tootajad_2019[i]-1,
             tootajad_2019 <= kredex_kaendus_saajad$tootajad_2019[i]+1
             # dkaive >= kredex_kaendus_saajad$dkaive[i]*0.95,
             # dkaive <= kredex_kaendus_saajad$dkaive[i]*1.05,
             # dtootajad >= kredex_kaendus_saajad$dtootajad[i]*0.95,
             # dtootajad <= kredex_kaendus_saajad$dtootajad[i]*1.05,
             # meede_kredex_laen == kredex_kaendus_saajad$meede_kredex_laen[i],
             # meede_EAS == kredex_kaendus_saajad$meede_EAS[i],
             # meede_TK20 == kredex_kaendus_saajad$meede_TK20[i],
             # meede_MES_kaendus == kredex_kaendus_saajad$meede_MES_kaendus[i],
             # meede_MES_laen == kredex_kaendus_saajad$meede_MES_laen[i]
      ) %>% 
      pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_kredex_kaendus == 0, 
             maakond == kredex_kaendus_saajad$maakond[i],
             #emtaktahttekst == kredex_kaendus_saajad$emtaktahttekst[i],
             emtak2tekst == kredex_kaendus_saajad$emtak2tekst[i],
             kaive_2019 >= kredex_kaendus_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= kredex_kaendus_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= kredex_kaendus_saajad$tootajad_2019[i]*(1-lubatudtootajateerinevus),
             tootajad_2019 <= kredex_kaendus_saajad$tootajad_2019[i]*(1+lubatudtootajateerinevus)
             # dkaive >= kredex_kaendus_saajad$dkaive[i]*0.95,
             # dkaive <= kredex_kaendus_saajad$dkaive[i]*1.05,
             # dtootajad >= kredex_kaendus_saajad$dtootajad[i]*0.95,
             # dtootajad <= kredex_kaendus_saajad$dtootajad[i]*1.05,
             # meede_kredex_laen == kredex_kaendus_saajad$meede_kredex_laen[i],
             # meede_EAS == kredex_kaendus_saajad$meede_EAS[i],
             # meede_TK20 == kredex_kaendus_saajad$meede_TK20[i],
             # meede_MES_kaendus == kredex_kaendus_saajad$meede_MES_kaendus[i],
             # meede_MES_laen == kredex_kaendus_saajad$meede_MES_laen[i]
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    kredex_kaendus_saajad$sarnased[i] <- toString(abi)    
    kredex_kaendus_saajad$sarnaste_arv[i] <- length(abi)
  }
}

kredex_laen_saajad <- df_aasta %>% 
  filter(meede_kredex_laen == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(kredex_laen_saajad)) {
  #Kui ettevõttel on 5 või vähem töötajat, siis töötajate mõttes sarnaseks peetakse +-1 töötajaga ettevõtteid
  if (!is.na(kredex_laen_saajad$tootajad_2019[i]) & kredex_laen_saajad$tootajad_2019[i] <= 5) { 
    abi <- df_aasta %>% 
      filter(meede_kredex_laen == 0, 
             maakond == kredex_laen_saajad$maakond[i],
             emtak2tekst == kredex_laen_saajad$emtak2tekst[i],
             kaive_2019 >= kredex_laen_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= kredex_laen_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= kredex_laen_saajad$tootajad_2019[i]-1,
             tootajad_2019 <= kredex_laen_saajad$tootajad_2019[i]+1
             # dkaive >= kredex_laen_saajad$dkaive[i]*0.95,
             # dkaive <= kredex_laen_saajad$dkaive[i]*1.05,
             # dtootajad >= kredex_laen_saajad$dtootajad[i]*0.95,
             # dtootajad <= kredex_laen_saajad$dtootajad[i]*1.05,
             # meede_kredex_kaendus == kredex_laen_saajad$meede_kredex_kaendus[i],
             # meede_EAS == kredex_laen_saajad$meede_EAS[i],
             # meede_TK20 == kredex_laen_saajad$meede_TK20[i],
             # meede_MES_kaendus == kredex_laen_saajad$meede_MES_kaendus[i],
             # meede_MES_laen == kredex_laen_saajad$meede_MES_laen[i]
      ) %>% 
      pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_kredex_laen == 0, 
             maakond == kredex_laen_saajad$maakond[i],
             #emtaktahttekst == kredex_laen_saajad$emtaktahttekst[i],
             emtak2tekst == kredex_laen_saajad$emtak2tekst[i],
             kaive_2019 >= kredex_laen_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= kredex_laen_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= kredex_laen_saajad$tootajad_2019[i]*(1-lubatudtootajateerinevus),
             tootajad_2019 <= kredex_laen_saajad$tootajad_2019[i]*(1+lubatudtootajateerinevus)
             # dkaive >= kredex_laen_saajad$dkaive[i]*0.95,
             # dkaive <= kredex_laen_saajad$dkaive[i]*1.05,
             # dtootajad >= kredex_laen_saajad$dtootajad[i]*0.95,
             # dtootajad <= kredex_laen_saajad$dtootajad[i]*1.05,
             # meede_kredex_kaendus == kredex_laen_saajad$meede_kredex_kaendus[i],
             # meede_EAS == kredex_laen_saajad$meede_EAS[i],
             # meede_TK20 == kredex_laen_saajad$meede_TK20[i],
             # meede_MES_kaendus == kredex_laen_saajad$meede_MES_kaendus[i],
             # meede_MES_laen == kredex_laen_saajad$meede_MES_laen[i]
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    kredex_laen_saajad$sarnased[i] <- toString(abi)    
    kredex_laen_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(kredex_kaendus_saajad$sarnaste_arv != 0)/nrow(kredex_kaendus_saajad)
sum(kredex_laen_saajad$sarnaste_arv != 0)/nrow(kredex_laen_saajad)

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
           # meede_kredex_kaendus == EAS_saajad$meede_kredex_kaendus[i],
           # meede_kredex_laen == EAS_saajad$meede_kredex_laen[i],
           # meede_TK20 == EAS_saajad$meede_TK20[i],
           # meede_MES_kaendus == EAS_saajad$meede_MES_kaendus[i],
           # meede_MES_laen == EAS_saajad$meede_MES_laen[i]
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
             # meede_kredex_kaendus == EAS_saajad$meede_kredex_kaendus[i],
             # meede_kredex_laen == EAS_saajad$meede_kredex_laen[i],
             # meede_TK20 == EAS_saajad$meede_TK20[i],
             # meede_MES_kaendus == EAS_saajad$meede_MES_kaendus[i],
             # meede_MES_laen == EAS_saajad$meede_MES_laen[i]
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
MES_kaendus_saajad <- df_aasta %>% 
  filter(meede_MES_kaendus == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(MES_kaendus_saajad)) {
  if (!is.na(MES_kaendus_saajad$tootajad_2019[i]) & MES_kaendus_saajad$tootajad_2019[i] <= 5) {
  abi <- df_aasta %>% 
    filter(meede_MES_kaendus == 0, 
           maakond == MES_kaendus_saajad$maakond[i],
           emtak2tekst == MES_kaendus_saajad$emtak2tekst[i],
           kaive_2019 >= MES_kaendus_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
           kaive_2019 <= MES_kaendus_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
           tootajad_2019 >= MES_kaendus_saajad$tootajad_2019[i]-1,
           tootajad_2019 <= MES_kaendus_saajad$tootajad_2019[i]+1
           # dkaive >= MES_kaendus_saajad$dkaive[i]*0.95,
           # dkaive <= MES_kaendus_saajad$dkaive[i]*1.05,
           # dtootajad >= MES_kaendus_saajad$dtootajad[i]*0.95,
           # dtootajad <= MES_kaendus_saajad$dtootajad[i]*1.05,
           # meede_kredex_kaendus == MES_kaendus_saajad$meede_kredex_kaendus[i],
           # meede_kredex_laen == MES_kaendus_saajad$meede_kredex_laen[i],
           # meede_EAS == MES_kaendus_saajad$meede_EAS[i],
           # meede_TK20 == MES_kaendus_saajad$meede_TK20[i],
           # meede_MES_laen == MES_kaendus_saajad$meede_MES_laen[i]
    ) %>% 
    pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_MES_kaendus == 0, 
             maakond == MES_kaendus_saajad$maakond[i],
             emtak2tekst == MES_kaendus_saajad$emtak2tekst[i],
             kaive_2019 >= MES_kaendus_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= MES_kaendus_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= MES_kaendus_saajad$tootajad_2019[i]*(1-lubatudtootajateerinevus),
             tootajad_2019 <= MES_kaendus_saajad$tootajad_2019[i]*(1+lubatudtootajateerinevus)
             # dkaive >= MES_kaendus_saajad$dkaive[i]*0.95,
             # dkaive <= MES_kaendus_saajad$dkaive[i]*1.05,
             # dtootajad >= MES_kaendus_saajad$dtootajad[i]*0.95,
             # dtootajad <= MES_kaendus_saajad$dtootajad[i]*1.05,
             # meede_kredex_kaendus == MES_kaendus_saajad$meede_kredex_kaendus[i],
             # meede_kredex_laen == MES_kaendus_saajad$meede_kredex_laen[i],
             # meede_EAS == MES_kaendus_saajad$meede_EAS[i],
             # meede_TK20 == MES_kaendus_saajad$meede_TK20[i],
             # meede_MES_laen == MES_kaendus_saajad$meede_MES_laen[i]
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    MES_kaendus_saajad$sarnased[i] <- toString(abi)  
    MES_kaendus_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

MES_laen_saajad <- df_aasta %>% 
  filter(meede_MES_laen == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(MES_laen_saajad)) {
  if (!is.na(MES_laen_saajad$tootajad_2019[i]) & MES_laen_saajad$tootajad_2019[i] <= 5) {
    abi <- df_aasta %>% 
      filter(meede_MES_laen == 0, 
             maakond == MES_laen_saajad$maakond[i],
             emtak2tekst == MES_laen_saajad$emtak2tekst[i],
             kaive_2019 >= MES_laen_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= MES_laen_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= MES_laen_saajad$tootajad_2019[i]-1,
             tootajad_2019 <= MES_laen_saajad$tootajad_2019[i]+1
             # dkaive >= MES_laen_saajad$dkaive[i]*0.95,
             # dkaive <= MES_laen_saajad$dkaive[i]*1.05,
             # dtootajad >= MES_laen_saajad$dtootajad[i]*0.95,
             # dtootajad <= MES_laen_saajad$dtootajad[i]*1.05,
             # meede_kredex_kaendus == MES_laen_saajad$meede_kredex_kaendus[i],
             # meede_kredex_laen == MES_laen_saajad$meede_kredex_laen[i],
             # meede_EAS == MES_laen_saajad$meede_EAS[i],
             # meede_TK20 == MES_laen_saajad$meede_TK20[i],
             # meede_MES_kaendus == MES_laen_saajad$meede_MES_kaendus[i]
      ) %>% 
      pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_MES_laen == 0, 
             maakond == MES_laen_saajad$maakond[i],
             emtak2tekst == MES_laen_saajad$emtak2tekst[i],
             kaive_2019 >= MES_laen_saajad$kaive_2019[i]*(1-lubatudkaibeerinevus),
             kaive_2019 <= MES_laen_saajad$kaive_2019[i]*(1+lubatudkaibeerinevus),
             tootajad_2019 >= MES_laen_saajad$tootajad_2019[i]*(1-lubatudtootajateerinevus),
             tootajad_2019 <= MES_laen_saajad$tootajad_2019[i]*(1+lubatudtootajateerinevus)
             # dkaive >= MES_laen_saajad$dkaive[i]*0.95,
             # dkaive <= MES_laen_saajad$dkaive[i]*1.05,
             # dtootajad >= MES_laen_saajad$dtootajad[i]*0.95,
             # dtootajad <= MES_laen_saajad$dtootajad[i]*1.05,
             # meede_kredex_kaendus == MES_laen_saajad$meede_kredex_kaendus[i],
             # meede_kredex_laen == MES_laen_saajad$meede_kredex_laen[i],
             # meede_EAS == MES_laen_saajad$meede_EAS[i],
             # meede_TK20 == MES_laen_saajad$meede_TK20[i],
             # meede_MES_kaendus == MES_laen_saajad$meede_MES_kaendus[i]
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    MES_laen_saajad$sarnased[i] <- toString(abi)  
    MES_laen_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(MES_kaendus_saajad$sarnaste_arv != 0)/nrow(MES_kaendus_saajad)
sum(MES_laen_saajad$sarnaste_arv != 0)/nrow(MES_laen_saajad)



# TK20 saajad ja sarnased ---------------------------------------------------

TK_saajad <- df_aasta %>% 
  filter(meede_TK20 == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(TK_saajad)) {
  if (!is.na(TK_saajad$tootajad_2019[i]) & TK_saajad$tootajad_2019[i] <= 5) {
  abi <- df_aasta %>% 
    filter(meede_TK20 == 0, 
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
           # meede_kredex_kaendus == TK_saajad$meede_kredex_kaendus[i],
           # meede_kredex_laen == TK_saajad$meede_kredex_laen[i],
           # meede_EAS == TK_saajad$meede_EAS[i],
           # meede_MES_kaendus == TK_saajad$meede_MES_kaendus[i],
           # meede_MES_laen == TK_saajad$meede_MES_laen[i]
    ) %>% 
    pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_TK20 == 0, 
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
             # meede_kredex_kaendus == TK_saajad$meede_kredex_kaendus[i],
             # meede_kredex_laen == TK_saajad$meede_kredex_laen[i],
             # meede_EAS == TK_saajad$meede_EAS[i],
             # meede_MES_kaendus == TK_saajad$meede_MES_kaendus[i],
             # meede_MES_laen == TK_saajad$meede_MES_laen[i]
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    TK_saajad$sarnased[i] <- toString(abi)  
    TK_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(TK_saajad$sarnaste_arv != 0)/nrow(TK_saajad)


# TK21 saajad ja sarnased ---------------------------------------------------

#Võrdlusaasta on 2020

TK21_saajad <- df_aasta %>% 
  filter(meede_TK21 == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(TK21_saajad)) {
  if (!is.na(TK21_saajad$tootajad_2020[i]) & TK21_saajad$tootajad_2020[i] <= 5) {
    abi <- df_aasta %>% 
      filter(meede_TK21 == 0, 
             maakond == TK21_saajad$maakond[i],
             emtak2tekst == TK21_saajad$emtak2tekst[i],
             kaive_2020 >= TK21_saajad$kaive_2020[i]*(1-lubatudkaibeerinevus),
             kaive_2020 <= TK21_saajad$kaive_2020[i]*(1+lubatudkaibeerinevus),
             tootajad_2020 >= TK21_saajad$tootajad_2020[i]-1,
             tootajad_2020 <= TK21_saajad$tootajad_2020[i]+1
      ) %>% 
      pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_TK21 == 0, 
             maakond == TK21_saajad$maakond[i],
             emtak2tekst == TK21_saajad$emtak2tekst[i],
             kaive_2020 >= TK21_saajad$kaive_2020[i]*(1-lubatudkaibeerinevus),
             kaive_2020 <= TK21_saajad$kaive_2020[i]*(1+lubatudkaibeerinevus),
             tootajad_2020 >= TK21_saajad$tootajad_2020[i]*(1-lubatudtootajateerinevus),
             tootajad_2020 <= TK21_saajad$tootajad_2020[i]*(1+lubatudtootajateerinevus)
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    TK21_saajad$sarnased[i] <- toString(abi)  
    TK21_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(TK21_saajad$sarnaste_arv != 0)/nrow(TK21_saajad)


# TK21 Harju ja Ida-Viru saajad ja sarnased ---------------------------------------------------

#Võrdlusaasta on 2020

TK21_hi_saajad <- df_aasta %>% 
  filter(meede_TK21_hi == 1) %>% 
  mutate(sarnased = 0, 
         sarnaste_arv = 0)

for (i in 1:nrow(TK21_hi_saajad)) {
  if (!is.na(TK21_hi_saajad$tootajad_2020[i]) & TK21_hi_saajad$tootajad_2020[i] <= 5) {
    abi <- df_aasta %>% 
      filter(meede_TK21_hi == 0, 
             maakond == TK21_hi_saajad$maakond[i],
             emtak2tekst == TK21_hi_saajad$emtak2tekst[i],
             kaive_2020 >= TK21_hi_saajad$kaive_2020[i]*(1-lubatudkaibeerinevus),
             kaive_2020 <= TK21_hi_saajad$kaive_2020[i]*(1+lubatudkaibeerinevus),
             tootajad_2020 >= TK21_hi_saajad$tootajad_2020[i]-1,
             tootajad_2020 <= TK21_hi_saajad$tootajad_2020[i]+1
      ) %>% 
      pull(registrikood)
  } else {
    abi <- df_aasta %>% 
      filter(meede_TK21_hi == 0, 
             maakond == TK21_hi_saajad$maakond[i],
             emtak2tekst == TK21_hi_saajad$emtak2tekst[i],
             kaive_2020 >= TK21_hi_saajad$kaive_2020[i]*(1-lubatudkaibeerinevus),
             kaive_2020 <= TK21_hi_saajad$kaive_2020[i]*(1+lubatudkaibeerinevus),
             tootajad_2020 >= TK21_hi_saajad$tootajad_2020[i]*(1-lubatudtootajateerinevus),
             tootajad_2020 <= TK21_hi_saajad$tootajad_2020[i]*(1+lubatudtootajateerinevus)
      ) %>% 
      pull(registrikood)
  }
  if (length(abi) != 0){
    TK21_hi_saajad$sarnased[i] <- toString(abi)  
    TK21_hi_saajad$sarnaste_arv[i] <- length(abi)
  }
}  

sum(TK21_hi_saajad$sarnaste_arv != 0)/nrow(TK21_hi_saajad)

# Andmed kokku ------------------------------------------------------------

df_meetmed <- kredex_kaendus_saajad %>% mutate(meede = "Kredex käendus") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv) %>% 
  rbind(kredex_laen_saajad %>% mutate(meede = "Kredex laen") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv)) %>% 
  rbind(EAS_saajad %>% mutate(meede = "EAS") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv)) %>% 
  rbind(MES_kaendus_saajad %>% mutate(meede = "MES käendus") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv)) %>% 
  rbind(MES_laen_saajad %>% mutate(meede = "MES laen") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv)) %>% 
  rbind(TK_saajad %>% mutate(meede = "Töötukassa 2020") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv))

df_meetmed <- df_meetmed %>% 
  rbind(TK21_saajad %>% mutate(meede = "Töötukassa 2021") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv)) %>% 
  rbind(TK21_hi_saajad %>% mutate(meede = "Töötukassa 2021 Harju/Ida-Viru") %>% select(registrikood, nimi, emtaktahttekst, emtak2tekst, meede, sarnased, sarnaste_arv))
save(df_meetmed, file = "Andmed/R_andmed/df_meetmed.RData")
