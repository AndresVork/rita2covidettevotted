#server.R

function(input, output, session) {

# Olulisuse tab -----------------------------------------------------------

  output$outputkaalud <- renderUI({
    kaal <- round(1/length(input$inputtunnused),3)
    w <- lapply(input$inputtunnused, function(i) {
      numericInput(paste0(i, "_kaal"), i, value = kaal, min = 0, max = 1, step = 0.01)
    })

    do.call(fluidRow, w)
  })
  
  observeEvent(input$inputtase, {
    if (input$inputtase == "emtak2tekst") {
      updateSelectInput(session, "inputsektor", label = "Vali EMTAK2", choices = emtak2)
    } else {
      updateSelectInput(session, "inputsektor", label = "Vali sektor", choices = sektorid)
    }
    })

  filtreeritudandmed <- eventReactive(input$button_uuenda, {
    req(input$inputtunnused)

    if (!is.null(input[[paste0(input$inputtunnused[1], "_kaal")]])) {
      negatiivne <- 0
      yle_yhe <- 0
      kaal_kokku <- 0
      for (x in input$inputtunnused) {
        if (input[[paste0(x, "_kaal")]] < 0) negatiivne <- 1 
        if (input[[paste0(x, "_kaal")]] > 1) yle_yhe <- 1 
        kaal_kokku <- kaal_kokku + abs(input[[paste0(x, "_kaal")]])
      }
      if (kaal_kokku < 0.99 | kaal_kokku > 1.01) {
        for (x in input$inputtunnused) {
          vana_kaal <- abs(input[[paste0(x, "_kaal")]])
          updateNumericInput(session, inputId = paste0(x, "_kaal"), value = round(vana_kaal/kaal_kokku,3))
        }
        abitekst <- case_when(negatiivne == 1 ~ "Vähemalt üks kaal oli negatiivne. Uute kaalude arvutamisel on arvestatud absoluutväätuseid, suhted on sisestatud kaaludega samad, aga nende summa on 1.",
                              yle_yhe == 1 ~ "Vähemalt üks kaal oli suurem kui 1. Uute kaalude suhted on sisestatud kaaludega samad, aga nende summa on 1.",
                              TRUE ~ "Sisestatud kaalud ei anna kokku 1. Uute kaalude suhted on sisestatud kaaludega samad, aga nende summa on 1.")
        showModal(modalDialog(
          title = "Kaalud on ümber arvutatud!",
          abitekst,
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
    
    temp_df <- df_koik %>% 
      #jätame alles valitud sektori, aasta, kvartali
      filter(!!sym(input$inputtase) == input$inputsektor, #emtak_taht varasemas versioonid
             aasta %in% input$inputaasta,
             kvartal %in% input$inputkvartal,
             liider_kood %in% c(0,1)) #KMK grupide puhul ainult liidrit näidata
    
    #kui on maakond valitud
    if (!is.null(input$inputmaakond)) {
      temp_df <- temp_df %>% 
        filter(maakond %in% input$inputmaakond)
    }
    
    #võtame valitud pidevatest muutujatest keskmised
    temp_means <- temp_df %>% group_by(registrikood) %>% 
      summarise_at(input$inputtunnused, ~mean(.,na.rm = TRUE))
    #jätame alles viimased perioodi taustatunnused
    temp_taust <- temp_df %>% 
      group_by(registrikood) %>% 
      filter(row_number()==n()) %>% 
      select(registrikood, nimi, maakond, valdlinn, emtak4, viimane_kaive = kaive, viimane_tootajad = tootajad)
    
    #koos
    temp <- left_join(temp_means, temp_taust, by = "registrikood") %>% 
      #filtreerime viimase kvratali töötajate arvu järgi. 
      filter(viimane_tootajad >= input$inputtootajad[1],
             viimane_tootajad <= input$inputtootajad[2])
    
    #Tsükli algväärtus nulliks
    temp$jrksumma = 0
    #käin ükshaaval muutujad läbis
    for (x in input$inputtunnused) {
      kaal <- ifelse(is.null(input[[paste0(x, "_kaal")]]), 
                     round(1/length(input$inputtunnused), 3), 
                     round(abs(input[[paste0(x, "_kaal")]])/kaal_kokku, 3))
      temp <- temp %>%  
        #sorteerin kahanevas järjekorras - seega mida suurem arv, seda parem
        arrange(desc(get(x))) %>% 
        #lisame järjekorranumbrite summa, jagame läbi kaaluga
        mutate(jrksumma = round(jrksumma + row_number()/kaal, 3))
    }
    
    #sorteerime jrksumma alusel - mida eespool (väiksem) seda parem
    temp <- temp %>%  arrange(jrksumma) %>% 
      select(jrksumma, registrikood, nimi, maakond, valdlinn, emtak4, 
             input$inputtunnused, viimane_kaive, viimane_tootajad) #aasta, kvartal
    temp
    #})
  })
  
  output$tabel <- DT::renderDT({
    DT::datatable(filtreeritudandmed(),
                  options = list(
                    pageLength = 100,
                    scrollX = TRUE,
                    paging = TRUE,
                    searching = TRUE
                  )) %>% formatRound(columns=c('jrksumma', input$inputtunnused), digits=3, mark = " ")
  })
  
# Meedete tab -------------------------------------------------------------
  #annab vastavalt sektori või emtak2 valikud
  observeEvent(input$inputtase2, {
    if (input$inputtase2 == "emtak2tekst") {
      updateSelectInput(session, "inputsektor2", label = "Vali EMTAK2", choices = emtak2)
    } else {
      updateSelectInput(session, "inputsektor2", label = "Vali sektor", choices = sektorid)
    }
  })
  
  meetmeandmed <- eventReactive(input$button_joonista, {
    
    #Arvutame 2019 aasta näitajad ja filtreerime
    temp_abi <- df_koik %>% 
      filter(aasta == 2019,
             !!sym(input$inputtase2) == input$inputsektor2, 
             liider_kood != 1)

    #kui on maakond valitud
    if (input$inputmaakond2 != "") {
      temp_abi <- temp_abi %>%
        filter(maakond == input$inputmaakond2)
    }
    #arvutame aasta käibe ja töötajate arvu filtreerimiseks
    temp_abi <- temp_abi %>% 
      group_by(registrikood) %>% 
      summarise(aasta_kaive = sum(kaive),
                aasta_tootajad = mean(tootajad)) %>% 
      ungroup() %>% 
      filter(aasta_kaive >= input$inputkaive[1],
             aasta_kaive <= input$inputkaive[2],
             aasta_tootajad >= input$inputtootajad2[1],
             aasta_tootajad <= input$inputtootajad2[2]) %>% 
      pull(registrikood)
    
    temp_df <- df_koik %>%
      filter(registrikood %in% temp_abi, 
             liider_kood != 1) %>% 
      #tunnus meede saab valitud meete väärtused
      mutate(meede = ifelse(.data[[input$inputmeede]] == 1, 1, 0)) %>% 
      select(registrikood, aeg, kaive, tootajad, meede) %>% 
      #jagame meetme saajateks ja mittesaajateks
      group_by(meede) %>% 
      mutate(ettevotteid = length(unique(registrikood))) %>% 
      ungroup() %>% 
      group_by(meede, aeg, ettevotteid) %>% 
      summarise(kaive = mean(kaive),
                tootajad = mean(tootajad)) %>% 
      ungroup() 

    return(temp_df)
  })

  output$saajate_arv <- renderValueBox({
      saajad <-  meetmeandmed() %>% filter(meede == 1) %>% pull(ettevotteid) %>% unique()
      if (length(saajad) == 0 ) saajad <- 0
    valueBox(value = saajad, subtitle = "meetme saajat", icon = icon("building"))
  })

  output$mittesaajate_arv <- renderValueBox({
    mittesaajad <- meetmeandmed() %>% filter(meede == 0) %>% pull(ettevotteid) %>% unique()
    if (length(mittesaajad) == 0 ) mittesaajad <- 0
    valueBox(value = mittesaajad, subtitle = "mittesaajat", icon = icon("building"))
  })
  
  output$kaivejoonis <- renderPlot({
    ggplot(meetmeandmed(), aes(aeg, kaive, group = meede, color = factor(meede))) + 
      geom_line() +
      scale_color_discrete(name = "", labels = c("Ei saanud meedet", "Sai meedet")) +
      labs(title = "Grupi keskmine käive kvartalis", x = "", y = "€") +
      theme_bw()
      
  })
  
  output$tootajajoonis <- renderPlot({
    ggplot(meetmeandmed(), aes(aeg, tootajad, group = meede, color = factor(meede))) + 
      geom_line() +
      scale_color_discrete(name = "", labels = c("Ei saanud meedet", "Sai meedet")) +
      labs(title = "Grupi keskmine töötajate arv kvartalis", x = "", y = "") +
      theme_bw()
  })

# Meetme mõju -------------------------------------------------------------
  #annab vastavalt sektori või emtak2 valikud
  observeEvent(input$inputtase3, {
    if (input$inputtase3 == "emtak2tekst") {
      updateSelectInput(session, "inputsektor3", label = "Vali EMTAK2", choices = emtak2)
    } else {
      updateSelectInput(session, "inputsektor3", label = "Vali sektor", choices = sektorid)
    }
  })
  
  meetmemoju_andmed <- eventReactive(input$button_joonista2, {
    
    meetme_andmed <- df_meetmed %>% 
      filter(meede == input$inputmeede2)
    #kui on sektor või emtak2 valitud
    if (input$inputtase3 != "") {
      meetme_andmed <- meetme_andmed %>% 
        filter(!!sym(input$inputtase3) == input$inputsektor3)
    }
    #saajad, kel on sarnased
    saajad_kood1 <- meetme_andmed %>% 
      filter(sarnaste_arv != 0) %>% 
      pull(registrikood)
    #saajad, kel ei ole sarnaseid
    saajad_kood2 <- meetme_andmed %>% 
      filter(sarnaste_arv == 0) %>% 
      pull(registrikood)
    
    #Filtreerime saajate, kel oli vähemalt 1 sarnane ettevõte, aegread
    saajad <- df_koik %>% 
      filter(registrikood %in% saajad_kood1,
             liider_kood != 1) %>% 
      mutate(grupp = "Sai meedet")

    #Leiame sarnaste ettevõtete kaalutud aegread
    #Igale saajale vastab üks kaalutud sarnaste etevõtete aegrida
    sarnased <- meetme_andmed %>% 
      filter(sarnaste_arv != 0) %>% 
      select(registrikood, sarnased) %>% 
      separate_rows(sarnased)
    #sarnaste ettevõtete registrikoodid
    sarnased_kood <- sarnased %>% pull(sarnased) %>% unique() %>% length()
    
    sarnased <- sarnased %>% 
    left_join(df_koik, by = c("sarnased"= "registrikood")) %>% 
      group_by(registrikood, aeg) %>% 
      summarise(kaive = mean(kaive), 
                tootajad = mean(tootajad)) %>% 
      ungroup() %>% 
      mutate(grupp = "Sarnased ettevõtted") %>% 
      inner_join(saajad %>% select(registrikood, aeg)) #ainult samad kvartalid, mis saaja ettevõttel

    kokku <- sarnased %>% 
      select(registrikood, aeg) %>% 
      #valime ainult need kvartalid, mil on sarnaseid ettevõtteid
      left_join(saajad) %>% 
      select(names(sarnased)) %>% 
      #Lisame sarnaste read
      rbind(sarnased) %>% 
      #kokku on 2 korda rohkem ridu kui saajate tabelis
      group_by(grupp, aeg) %>% 
      summarise(kaive = mean(kaive),
                tootajad = mean(tootajad), 
                arv = n()) %>% 
      ungroup() %>% 
      mutate(grupp = factor(grupp, levels = c("Sarnased ettevõtted", "Sai meedet")))
    
    return(list(kokku, saajad_kood1, saajad_kood2, sarnased_kood))
  })
  
  #põhilehe outputid
  output$meetmemoju_leht <- renderUI({
    req(input$button_joonista2)
    tagList(
      fluidRow(valueBoxOutput("saajate_arv2"),
               valueBoxOutput("saajate_sarnaste_arv"),
               valueBoxOutput("sarnaste_arv")),
      shinycssloaders::withSpinner(plotOutput("kaivejoonis2")),
      shinycssloaders::withSpinner(plotOutput("tootajajoonis2")),
      box(width = 12,
        title = "Saajate nimed", 
        collapsible = TRUE,
        collapsed = TRUE,
        htmlOutput("nimekiri")
      )
    )
  })

  output$saajate_arv2 <- renderValueBox({
    valueBox(value = length(meetmemoju_andmed()[[2]]) + length(meetmemoju_andmed()[[3]]), subtitle = "meetme saajat", icon = icon("building"))
  })
  output$saajate_sarnaste_arv <- renderValueBox({
    valueBox(value = length(meetmemoju_andmed()[[2]]), subtitle = "meetme saajat, kellele leiti sarnaseid", icon = icon("building"))
  })
  output$sarnaste_arv <- renderValueBox({
    valueBox(value = meetmemoju_andmed()[[4]], subtitle = "sarnast ettevõtet", icon = icon("building"))
  })
  
  output$kaivejoonis2 <- renderPlot({
    ggplot(meetmemoju_andmed()[[1]], aes(aeg, kaive, group = grupp, color = factor(grupp))) + 
      geom_line() +
      scale_color_discrete(name = "") +
      labs(title = "Grupi keskmine käive kvartalis", x = "", y = "€") +
      theme_bw()
    
  })
  
  output$tootajajoonis2 <- renderPlot({
    ggplot(meetmemoju_andmed()[[1]], aes(aeg, tootajad, group = grupp, color = factor(grupp))) + 
      geom_line() +
      scale_color_discrete(name = "") +
      labs(title = "Grupi keskmine töötajate arv kvartalis", x = "", y = "") +
      theme_bw()
  })
  
  output$nimekiri <- renderUI({
    tekst <- NULL
    if (length(meetmemoju_andmed()[[2]]) != 0 | length(meetmemoju_andmed()[[3]]) != 0) {
      #ettevõtted, kel on sarnaseid
      saajad <- df_koik %>% 
        filter(registrikood %in% meetmemoju_andmed()[[2]], liider_kood != 1) %>% 
        group_by(registrikood) %>% 
        summarise(nimi = last(nimi)) %>% 
        pull(nimi)
    #ettevõtted, kelle ei leitud sarnaseid
      saajad2 <- df_koik %>% 
        filter(registrikood %in% meetmemoju_andmed()[[3]], liider_kood != 1) %>% 
        group_by(registrikood) %>% 
        summarise(nimi = last(nimi)) %>% 
        pull(nimi)
      nimed <- c(saajad,"<br/>", "<strong>Järgmistele ettevõtetetele ei leitud sarnaseid</strong>", saajad2)
      tekst <- paste(nimed, collapse = "<br/>")
    }
    HTML(tekst)
  }) 

# Üksik ettevõtte ---------------------------------------------------------
  #Vali ettevõtte valikud
  observe({
    updateSelectizeInput(session, "inputettevote",
                      choices = ettevotte_nimekiri$nimi,
                      server = TRUE
    )
    })

  #valitud ettevõtte andmed 
  ettevotte_andmed <- reactive({
    req(input$inputettevote)
    kood <- ettevotte_nimekiri$registrikood[ettevotte_nimekiri$nimi == input$inputettevote]
    liider <- grepl("(KMK grupp)", input$inputettevote)
    if (liider == TRUE) {
      andmed <- df_koik %>% 
        filter(registrikood == kood, liider_kood == 1)
    } else {
      andmed <- df_koik %>% 
        filter(registrikood == kood, liider_kood != 1)
    }
    return(andmed)
  })
  #vasakul kuvatav ettevõtte info
  output$taustainfo_ettevote <- renderUI({
    viimane <- ettevotte_andmed() %>% slice(n()) #viimane rida
  
    str <- paste("<br/><b>", viimane$nimi, "</b><br/>",
                 "<b>Registrikood</b>", viimane$registrikood, "<br/>",
                 "<b>Sektor</b>", viimane$emtaktahttekst, "<br/>",
                 "<b>EMTAK2</b>", viimane$emtak2tekst, "<br/>",
                 "<b>Maakond</b>", viimane$maakond, "<br/>",
                 "<b>Vald/linn</b>", viimane$valdlinn, "<br/>",
                 "<b>Käibemaksukohuslane</b>", viimane$KMK, "<br/>")
    #saadud meetmed
    if (viimane$meede_EAS + viimane$meede_TK + viimane$meede_kredex + viimane$meede_MES > 0) {
      meetmed <- c("EAS", "TK", "Kredex", "MES")[c(viimane$meede_EAS == 1, viimane$meede_TK == 1, viimane$meede_kredex == 1,viimane$meede_MES == 1)]
      str <- paste(str, paste("Ettevõte sai", paste(meetmed, collapse = ", "), "meedet/meetmeid.<br/>"), sep = '<br/>')
    } else {
      str <- paste(str, "Ettevõte ei saanud meetmeid.<br/>", sep = '<br/>')
    }
    #kas kuulub KMK gruppi
    if (viimane$liider_kood == 0) {
      str <- paste(str, "Ettevõte ei kuulu KMK gruppi.", sep = '<br/>')
    } else if  (viimane$liider_kood == 1) { #on KMK grupi summeeritud ettevõte
      ettevotted <- df_koik %>% 
        filter(aeg == viimane$aeg, liider_kood == viimane$registrikood) %>% 
        pull(nimi)
      str <- paste(str, "Antud ettevõte on grupeeritud KMK grupp.", sep = '<br/>')
      str <- paste(str, paste("Antud gruppi kuuluvad", paste(ettevotted, collapse = ",<br/>"), sep = '<br/>'), sep = '<br/>')
    } else { #kuulub KMK gruppi
      ettevotted <- df_koik %>% 
        filter(aeg == viimane$aeg, liider_kood == viimane$liider_kood, registrikood != viimane$registrikood) %>%
        mutate(nimi = ifelse(registrikood == liider_kood, paste(nimi, "(liider)"), nimi)) %>% 
        pull(nimi)
      str <- paste(str, "Antud ettevõte kuulub KMK grupp.", sep = '<br/>')
      str <- paste(str, paste("Antud gruppi kuuluvad ka", paste(ettevotted, collapse = ",<br/>"), sep = '<br/>'), sep = '<br/>')
    }
    HTML(str)
  })
  
  #Sarnaste ettevõttete nõuded
  output$sarnaste_nouded <- renderUI({
    req(ettevotte_andmed())
    viimane <- ettevotte_andmed() %>% slice(n()) #viimane rida
    
    #Saab lisada maakondi (va sama maakond)
    maakond1 <- viimane %>% pull(maakond)
    if (is.na(maakond1)) {
      maakonnad1 <- maakonnad
    } else {
      maakonnad1 <- maakonnad[maakonnad != maakond1]
    }
    #baasaastaks saab valida aastad, mil valitud ettevõtte kohta on andmeid
    aastad <- ettevotte_andmed() %>% pull(aasta) %>% unique() 
    valik <- ifelse(2019 %in% aastad, 2019, tail(aastad,1))
    
    #renderdame ainult saadud meetmete linnukesed
    meetmed <- c("EAS", "TK", "Kredex", "MES")[c(viimane$meede_EAS == 1, viimane$meede_TK == 1, viimane$meede_kredex == 1,viimane$meede_MES == 1)]
    w <- lapply(meetmed, function(i) {
      checkboxInput(paste0("input", i), label = paste("Ei saanud", i, "meedet"), value = NULL)
    })
    
    #sarnaste ettevõtete nõuete valikud
    tagList(     
      checkboxInput("inputsektor_sama", label = "Sama sektor", value = TRUE),
      checkboxInput("inputemtak2_sama", label = "Sama EMTAK2", value = TRUE),
      checkboxInput("inputmaakond_sama", label = "Sama maakond", value = TRUE),
      conditionalPanel(condition = "input.inputmaakond_sama",
                       selectInput("inputmaakond2_sama", "lisa ka", maakonnad1, multiple = TRUE)),
      checkboxInput("inputkaive_sama", label = "Sarnane käive", value = TRUE),
      conditionalPanel(condition = "input.inputkaive_sama",
                       numericInputIcon("inputdkaive", NULL, value = 20, min = 0,
                                        max = 100, icon = list(plusmiinus, icon("percent")))),
      checkboxInput("inputtootajad_sama", label = "Sarnane töötajate arv", value = TRUE),
      conditionalPanel(condition = "input.inputtootajad_sama",
                       numericInputIcon("inputdtootajad", NULL, value = 20, min = 0, 
                                        max = 100, icon = list(plusmiinus, icon("percent")))),
      conditionalPanel(condition = "input.inputkaive_sama || input.inputtootajad_sama",
                       selectInput("inputbaasaasta", "Vali võrreldav aasta", aastad, selected = valik)
      ),
      w
      )
  })
  
  sarnased_reg <- eventReactive(input$button_joonista3, {
    if (input$inputsarnased) {

    #valitud ettevõtte aastane käive ja keskmine töötajate arv ja viimase kvartali taustatunnused
    mina_koond <- ettevotte_andmed() %>% slice(n()) %>% select(registrikood, maakond, emtaktahttekst, emtak2tekst) #viimane rida
    mina_koond <- ettevotte_andmed() %>% 
      filter(aasta == input$inputbaasaasta) %>% 
      group_by(registrikood) %>% 
      summarise(kaive = sum(kaive),
                tootajad  = mean(tootajad)) %>% 
      ungroup() %>% 
      left_join(mina_koond)

    #leia ettevõtted, kes on sarnased
    #kui sarnaste nõudeks on käive või töötajate arv, ainult siis arvutame need
    if (input$inputtootajad_sama | input$inputkaive_sama) {
      andmed_temp <- df_koik %>% 
        filter(registrikood != mina_koond$registrikood, 
               liider_kood %in% c(0,1)) %>% #sarnaste seas on KMK grupid koos või ettevõtted, kes ei kuulu KMK gruppi
        select(registrikood, nimi, maakond, emtaktahttekst, emtak2tekst, meede_EAS, meede_kredex, meede_MES, meede_TK) %>% 
        group_by(registrikood) %>% 
        slice(n()) %>% #viimane rida iga ettevõtte kohta
        ungroup()
      andmed_temp <- df_koik %>% 
        filter(registrikood != mina_koond$registrikood, aasta == input$inputbaasaasta, liider_kood %in% c(0,1)) %>% 
        group_by(registrikood) %>% 
        summarise(kaive = sum(kaive),
                  tootajad  = mean(tootajad)) %>% 
        ungroup() %>% 
        left_join(andmed_temp)
    } else {
      #võtame viimased andmed, sest käivet ja töötajate arvu põhjal ei leie sarnaseid
      andmed_temp <- df_koik %>% 
        filter(registrikood != mina_koond$registrikood, liider_kood %in% c(0,1)) %>% 
        select(registrikood, nimi, maakond, emtaktahttekst, emtak2tekst) %>% 
        group_by(registrikood) %>% 
        slice(n())
    }
    # filtreerime valikute põhjal
    #sama sektor
    if (input$inputsektor_sama) {
      if (!is.na(mina_koond$emtaktahttekst)){
        andmed_temp <- andmed_temp %>% filter(emtaktahttekst == mina_koond$emtaktahttekst)
      }
    } 
    #sama emtak2
    if (input$inputemtak2_sama) {
      if (!is.na(mina_koond$emtak2tekst)) {
        andmed_temp <- andmed_temp %>% filter(emtak2tekst == mina_koond$emtak2tekst)
      }
    }
    #sama maakond + lisatud maakonnad
    if (input$inputmaakond_sama) {
      if(!is.na(mina_koond$maakond) | length(input$inputmaakond2_sama) != 0) {
        andmed_temp <- andmed_temp %>% filter(maakond %in% c(mina_koond$maakond, input$inputmaakond2_sama))
      }
    } 
    #sarnane käive
    if (input$inputkaive_sama) andmed_temp <- andmed_temp %>% 
        filter(kaive >= mina_koond$kaive*(1-input$inputdkaive/100), 
               kaive <= mina_koond$kaive*(1+input$inputdkaive/100))
    #sarnane töötajate arv
    if (input$inputtootajad_sama) andmed_temp <- andmed_temp %>% 
        filter(tootajad >= mina_koond$tootajad*(1-input$inputdtootajad/100), 
               tootajad <= mina_koond$tootajad*(1+input$inputdtootajad/100))
    
    #meetme inputid eksisteerivad ainult kui valitud ettevõte sai meedet
    if ("inputEAS"  %in% names(input)) {
      #läheb if sisse, kui selle linnuke on tehtud
      if (input$inputEAS)  andmed_temp <- andmed_temp %>% filter(meede_EAS != 1)
    }
    if ("inputKredex"  %in% names(input)) {
      if (input$inputKredex)  andmed_temp <- andmed_temp %>% filter(meede_kredex != 1)
    }
    if ("inputMES"  %in% names(input)) {
      if (input$inputMES)  andmed_temp <- andmed_temp %>% filter(meede_MES != 1)
    } 
    if ("inputTK" %in% names(input)) {
      if (input$inputTK)  andmed_temp <- andmed_temp %>% filter(meede_TK != 1)
    }
    
    andmed_temp <- andmed_temp %>% select(nimi, registrikood) %>% arrange(nimi)
    
    } else {
      #tühi andmestik, kui sarnaseid ei otsita
      andmed_temp <- ettevotte_andmed()[FALSE,] 
    }
    return(andmed_temp)
  })

  jooniste_andmed <- reactive({
    req(sarnased_reg()) #need muutuvad nupu vajutusel
    isolate(ettevotte_andmed()) # nende andmete muutumine ei kutsu seda funktsiooni esile
    #kui on sarnased
    if (nrow(sarnased_reg()) != 0) {
        sarnased <- df_koik %>%
          select(registrikood, aeg, kaive, tootajad) %>%
          filter(registrikood %in% sarnased_reg()$registrikood) %>% #mis teha KMK gruppide puhul
          group_by(aeg) %>%
          summarise_at(vars(kaive:tootajad), mean, na.rm = TRUE) %>%
          mutate(grupp = "Sarnased ettevõtted")
        mina <- ettevotte_andmed() %>%
          mutate(grupp = last(nimi)) %>%
          select(names(sarnased))
        andmed <- rbind(sarnased, mina) %>%
          mutate(grupp = factor(grupp, levels = c("Sarnased ettevõtted", mina$grupp[1])))
    } else {
      #kui sarnased_reg on tühi tabel (sarnaseid ei ole vaja leida või ei leitud)
      andmed <- ettevotte_andmed() %>%
        mutate(grupp = last(nimi))
    }
    return(andmed)
  })

  output$kaivejoonis3 <- renderPlot({
    ggplot(jooniste_andmed(), aes(aeg, kaive, color = grupp, group = grupp)) + 
      geom_line() +
      scale_color_discrete(name = "") +
      labs(title = "Ettevõtte käive kvartalis", x = "", y = "€") +
      theme_bw()
    
  })
  
  output$tootajajoonis3 <- renderPlot({
    ggplot(jooniste_andmed(), aes(aeg, tootajad, color = grupp, group = grupp)) + 
      geom_line() +
      scale_color_discrete(name = "") +
      labs(title = "Ettevõtte töötajate arv kvartalis", x = "", y = "") +
      theme_bw()
  })
  
  output$nimekiri2 <- renderUI({
    jooniste_andmed() #kui valitakse uus ettevõte, siis kustutab vana nimekirja ära
    tekst <- NULL
    if (nrow(sarnased_reg()) != 0) {
      sarnased <- sarnased_reg() %>% 
        pull(nimi)
      tekst <- paste(sarnased, collapse = "<br/>")
    } else {
      tekst <- "Sarnaseid ettevõtteid ei leitud."
    }
    HTML(tekst)
  }) 
  
}