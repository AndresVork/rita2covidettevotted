#ui.R

navbarPage(title = "", 
           header = tagList(useShinydashboard()),
           theme = "custom.css",
           position = c("fixed-top"),
           collapsible = TRUE, 
           tabPanel("Olulisus",
                    # Parameetrid
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   radioGroupButtons(inputId = "inputtase", "Vali tase", 
                                                     choices = c("Sektor" = "emtaktahttekst", "EMTAK2" = "emtak2tekst"), selected = "emtaktahttekst"),
                                   selectInput(inputId = "inputsektor", "Vali sektor", sektorid),
                                   h5("Periood üle mille võetakse näitajate keskmised"),
                                   selectInput(inputId = "inputaasta", "Vali aasta", c(2017, 2018, 2019, 2020), selected = 2019, multiple = TRUE),
                                   selectInput(inputId = "inputkvartal", "Vali kvartal", c(1,2,3,4), selected = c(3,4), multiple = TRUE),
                                   selectInput(inputId = "inputtunnused", "Vali järjestamise aluseks tunnused",
                                               tunnusedjarjestamiseks, selected = "tootajad_olulisus_valdlinn", multiple = TRUE),
                                   conditionalPanel(condition = "input.inputtunnused != ''",
                                                    dropdownButton(
                                                      circle = FALSE, label = "Kaalud", width = "100px", margin = "20px",
                                                      uiOutput("outputkaalud")
                                                    )),
                                   numericRangeInput(inputId = "inputtootajad", label = "Töötajate arv:",
                                                     value = c(0, 5000), separator = "-"),
                                   selectInput(inputId = "inputmaakond", "Vali maakond",
                                               c("Kõik" = '', maakonnad), multiple = TRUE),
                                   actionButton("button_uuenda", "Järjesta")
                                   
                      ),
                      #põhileht
                      mainPanel(width = 9,
                                shinycssloaders::withSpinner(
                                  DT::DTOutput("tabel")
                                ))
                    )
           ),
           tabPanel("Meetmed",
                    # Parameetrid
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   selectInput(inputId = "inputmeede", "Vali meede",
                                               choices = c("EAS" = "meede_EAS",
                                                           "Kredex" = "meede_kredex",
                                                           "MES" = "meede_MES",
                                                           "Töötukassa" = "meede_TK"),
                                               selected = "meede_kredex"),
                                   radioGroupButtons(inputId = "inputtase2", "Vali tase", 
                                                     choices = c("Sektor" = "emtaktahttekst", "EMTAK2" = "emtak2tekst"), selected = "emtaktahttekst"),
                                   selectInput(inputId = "inputsektor2", "Vali sektor", sektorid),
                                   selectInput(inputId = "inputmaakond2", "Vali maakond", c("Kõik" = '', maakonnad)),
                                   numericRangeInput(inputId = "inputkaive", label = "2019.aasta käive:",
                                                     value = c(0, max_kaive), separator = "-"),
                                   numericRangeInput(inputId = "inputtootajad2", label = "2019.aasta keskmine töötajate arv:",
                                                     value = c(0, max_tootajad), separator = "-"),
                                   actionButton("button_joonista", "Joonista")
                                   
                      ),
                      #põhileht
                      mainPanel(width = 9,
                                fluidRow(valueBoxOutput("saajate_arv", width = 6),
                                         valueBoxOutput("mittesaajate_arv", width = 6)),
                                shinycssloaders::withSpinner(plotOutput("kaivejoonis")),
                                shinycssloaders::withSpinner(plotOutput("tootajajoonis"))
                      )
                    )
           ),
           tabPanel("Meetme mõju",
                    # Parameetrid
                    sidebarLayout(
                      sidebarPanel(width = 3,
                                   selectInput(inputId = "inputmeede2", "Vali meede",
                                               choices = c("EAS", "Kredex", "MES", "Töötukassa"),
                                               selected = "Kredex"),
                                   radioGroupButtons(inputId = "inputtase3", "Vali tase", 
                                                     choices = c("Kõik" = "", "Sektor" = "emtaktahttekst", "EMTAK2" = "emtak2tekst"), selected = ""),
                                   
                                   conditionalPanel(condition = "input.inputtase3 != ''",
                                                    selectInput(inputId = "inputsektor3", "Vali sektor", sektorid)),
                                   actionButton("button_joonista2", "Joonista")
                                   
                      ),
                      #põhileht
                      mainPanel(width = 9,
                                uiOutput("meetmemoju_leht")
                      )
                    )
           ),
           tabPanel("Üksik ettevõte",
                    # Parameetrid
                    sidebarLayout(
                      sidebarPanel(width = 3, 
                                   selectizeInput("inputettevote", "Vali ettevõte", 
                                                  choices = NULL, 
                                                  options = list(searchConjunction = 'and')),
                                   checkboxInput("inputsarnased", strong("Leia sarnased")),
                                   conditionalPanel(condition = "input.inputsarnased",
                                                    uiOutput("sarnaste_nouded")
                                   ),
                                   
                                   actionButton("button_joonista3", "Joonista"),
                                   htmlOutput("taustainfo_ettevote")
                      ),
                      #põhileht
                      mainPanel(width = 9,
                                shinycssloaders::withSpinner(plotOutput("kaivejoonis3")),
                                shinycssloaders::withSpinner(plotOutput("tootajajoonis3")),
                                conditionalPanel(condition = "input.inputsarnased",
                                                 box(width = 12,
                                                     title = "Sarnaste ettevõtete nimed", 
                                                     collapsible = TRUE,
                                                     collapsed = TRUE,
                                                     htmlOutput("nimekiri2"))
                                )
                      ))
                    
           )
           
           
           
)