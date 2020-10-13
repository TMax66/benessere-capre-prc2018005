
ui<-navbarPage("PRC2018005",
               
  ########PANEL DESCRIZIONE PROGETTO#####             
               tabPanel("Descrizione",
                        fluidPage(
                          #fluidRow(),
                          fluidRow(
                            tabsetPanel(type="tabs",
                                        tabPanel("Dati identificativi del progetto", includeHTML("generale.html")),
                                        tabPanel("Razionale", includeHTML("intro.html")),
                                        tabPanel("Conoscenze disponbili",includeHTML("d1.html")),
                                        tabPanel("Nuove conoscenze",includeHTML("d2.html")),
                                        tabPanel("Metodologia",includeHTML("d3.html")),
                                        tabPanel("Trasferibilità e diffusione dei risultati",includeHTML("d4.html")),
                                        tabPanel("Valore aggiunto",includeHTML("d5.html")),
                                        tabPanel("Fasi del progetto", includeHTML("d6.html"))
                                        #,
                                        # tabPanel("Risorse",
                                        #          br(),
                                        #          br(),
                                        #          column(12, div(align="center",
                                        #          tableOutput("risorse")))),
                                        # tabPanel("Timing",
                                        #          br(),
                                        #          br(),
                                        #          hr(),
                                        #          timevisOutput("timeline"),hr())
                                        
                                        
                                        )
                        ), 
                        hr()
                        
                        )
               ),
               
               tabPanel("Unità Operative",
                        fluidPage(
                          fluidRow(
                            column(12, div(align="center",
                                           DT::dataTableOutput("UO")))
                          )
                          
                           )
               ),
               
  tabPanel("Inserimento dati",
                     fluidPage(
                       fluidRow(
                         tabsetPanel(type="tabs",
                           ######dati aziendali
                           tabPanel("Dati aziendali",

                             div(id = "form",

                           fluidRow(
                             
                             column(2,
                                    textInput("anno", "Anno", value = "")),
                             column(2,
                                    textInput("mese", "Mese", value = "")),

                             column(2,
                                    textInput( "azienda","Azienda",value="")
                             ),
                             column(2,
                                    textInput( "ncapre","N. capre in lattazione", value = "")
                                  
                             ),
                             
                             column(2,
                                    textInput( "asciutta","N. capre in asciutta", value = "")
                                    
                             ),

                             
                             column(2,
                                    textInput( "rimonta","N. capre da rimonta ", value = "")
                                    
                             ),
                             
                             column(2,
                                    textInput( "capretti","N. capretti", value = "")
                                    
                             ),
                             
                             column(2,
                                    textInput( "becchi","N.becchi", value = "")
                                    
                             ),
                             
                             column(2,
                                    textInput( "capog","Kg/capo/die", value = "")
                             ),
                             column(2,
                                    textInput( "adultdead","N.capre adulte morte", value = "")
                             ),
                             column(2,
                                    textInput("puppydead", "N.capretti morti", value = "")
                             )

                           ),

                           fluidRow(

                             column(2,
                                    textInput( "rim","N.rimonta morte", value = "")),
                             column(2,
                                    textInput( "abo","N.aborti", value = "")),
                             column(2,
                                    textInput("ter", "N.terapie/lattazione", value = "")),
                             column(2,
                                    textInput("asc", "N.terapie/asciutta", value = "")),

                             column(2,
                                    textInput("trim", "N.terapie/rimonta", value = "")),
                             column(2,
                                    textInput("tpuppy", "N.terapie/capretti", value = "")),




                           fluidRow(
                             column(9,div(align="center",actionButton("submit", "Salva", class = "btn-primary")

                             )),

                             column(3, shinyjs::hidden(
                               div(
                                 id = "datainputed_msg",
                                 "Dati inseriti",
                                 actionLink("submit_another", "Inserisci altri dati")
                               )
                             )
                             )),
                           
                           hr(),
                           div(
                             
                             p("",
                               a(href="https://docs.google.com/spreadsheets/d/1kJTXGh4op0gawkiVkLchfbFn5Mb_E8oMqbHNDnQ_Iks/edit?usp=sharing",target="_blank",
                                 "Modifica dati"),align="center", style = "font-size:12pt")
                           ),
                           hr(),
                           
                           


                           fluidRow(
                             column(1
                             ),
                             column(10,DT::dataTableOutput("responsesTable")),
                             column(1)
                           )

                       )
                     )),
                     #####dati latte######
                     tabPanel("Qualità latte",

                       div(id = "form2",
                          fluidRow(
                            column(2,
                                   textInput("manno", "Anno", value = "")),
                            column(2,
                                   textInput("mmese", "Mese", value = "")),
                            
                            column(2,
                                   textInput( "mazienda","Azienda",value="")
                            ),

                            column(2,
                                   textInput("scc", "SCC", value = "")),

                            column(2,
                                   textInput("prot", "Proteine", value = "")),


                            column(2,
                                   textInput("cas", "Caseine", value = "")),

                            column(2,
                                   textInput("grasso", "Grasso", value = "")),
                            column(2,
                                   textInput("latt", "Lattosio", value = "")),
                            column(2,
                                   textInput("cbt", "CBT", value = "")),
                            column(2,
                                   textInput("stau", "STAU", value = "")),
                            
                            column(2,
                                   textInput("ureaFTIR", "UreaFTIR", value = "")),
                            column(2,
                                   textInput("ureapHm", "UreapHm", value = "")),
                            column(2,
                                   textInput("inib", "Inibenti", value = "")),


                            fluidRow(
                              column(9,div(align="center",actionButton("submit2", "Salva", class = "btn-primary")

                              )),

                              column(3, shinyjs::hidden(
                                div(
                                  id = "2datainputed_msg",
                                  "Dati inseriti",
                                  actionLink("msubmit_another", "Inserisci altri dati")
                                )
                              )
                              )),
                            
                            hr(),
                            div(
                              
                              p("",
                                a(href="https://docs.google.com/spreadsheets/d/1kJTXGh4op0gawkiVkLchfbFn5Mb_E8oMqbHNDnQ_Iks/edit?usp=sharing",target="_blank",
                                  "Modifica dati"),align="center", style = "font-size:12pt")
                            ),
                            hr(),

                            fluidRow(
                              column(1
                              ),
                              column(10,DT::dataTableOutput("responsesTable2")),
                              column(1)
                            )
                            


                                   )
                       )


                     ),
                     tabPanel("Dati sanitari",

                              div(id = "form3",
                                  fluidRow(
                                    column(2,
                                           textInput("sanno", "Anno", value = "")),
                                    column(2,
                                           textInput("smese", "Mese", value = "")),

                                    column(2,
                                           textInput( "sazienda","Azienda",value="")),

                                    column(2,
                                           textInput("parat", "Paratbc", value = "")),

                                    column(2,
                                           textInput("agal", "Agalassia", value = "")),


                                    column(2,
                                           textInput("caev", "CAEV", value = "")),

                                    column(2,
                                           textInput("ascessi", "Mal.Ascessi", value = "")),
                                    column(2,
                                           textInput("mast", "Mastite", value = "")),

                                    fluidRow(
                                      column(9,div(align="center",actionButton("submit3", "Salva", class = "btn-primary")

                                      )),

                                      column(3, shinyjs::hidden(
                                        div(
                                          id = "3datainputed_msg",
                                          "Dati inseriti",
                                          actionLink("ssubmit_another", "Inserisci altri dati")
                                        )
                                      )
                                      )),
                                    hr(),
                                    div(
                                      
                                      p("",
                                        a(href="https://docs.google.com/spreadsheets/d/1kJTXGh4op0gawkiVkLchfbFn5Mb_E8oMqbHNDnQ_Iks/edit?usp=sharing",target="_blank",
                                          "Modifica dati"),align="center", style = "font-size:12pt")
                                    ),
                                    hr(),

                                    fluidRow(
                                      column(1
                                      ),
                                      column(10,DT::dataTableOutput("responsesTable3")),
                                      column(1)
                                    )


                                  )
                              )

                     ),

                     tabPanel("Parassitologico",
                      
                      div(id = "form4",
                          fluidRow(
                            column(2,
                                   textInput("panno", "Anno", value = "")),
                            column(2,
                                   textInput("pmese", "Mese", value = "")),
                            
                            column(2,
                                   textInput( "pazienda","Azienda",value="")),
                            
                            column(2,
                                   selectInput("cat", "Categoria", 
                                               choices = c("Capretti", "Adulti", "Rimonta"),
                                             selected = "")),
                            
                            column(2,
                                   textInput("coccidi", "Coccidi", value = "")),
                            
                            
                            column(2,
                                   textInput("strGE", "StrGE", value = "")),
                            
                            column(2,
                                   textInput("strPO", "StrPO", value = "")),
                         
                            
                            fluidRow(
                              column(9,div(align="center",actionButton("submit4", "Salva", class = "btn-primary")
                                           
                              )),
                              
                              column(3, shinyjs::hidden(
                                div(
                                  id = "4datainputed_msg",
                                  "Dati inseriti",
                                  actionLink("psubmit_another", "Inserisci altri dati")
                                )
                              )
                              )),
                            hr(),
                            div(
                              
                              p("",
                                a(href="https://docs.google.com/spreadsheets/d/1kJTXGh4op0gawkiVkLchfbFn5Mb_E8oMqbHNDnQ_Iks/edit?usp=sharing",target="_blank",
                                  "Modifica dati"),align="center", style = "font-size:12pt")
                            ),
                            hr(),
                            
                            fluidRow(
                              column(1
                              ),
                              column(10,DT::dataTableOutput("responsesTable4")),
                              column(1)
                            )
                            
                            
                          )
                      )
                      
                     ),
                     
                     
                     
                     
                     tabPanel("Diagnostica",
                              
                              div(id = "form5",
                                  fluidRow(
                                    column(2,
                                           textInput("danno", "Anno", value = "")),
                                    column(2,
                                           textInput("dmese", "Mese", value = "")),
                                    
                                    column(2,
                                           textInput( "dazienda","Azienda",value="")),
                                    
                                    column(2,
                                           selectInput("dcat", "Categoria", 
                                                       choices = c("Capretti", "Adulti", "Rimonta"),
                                                       selected = "")),
                                    
                                    column(2,
                                           textInput("necro", "Necroscopia", value = "")),
                                    
                                    
                                    column(2,
                                           textInput("bat", "Batteriologico", value = "")),
                                    
                                    column(2,
                                           textInput("diagnosi", "Diagnosi", value = "")),
                                    
                                    
                                    
                                    fluidRow(
                                      column(9,div(align="center",actionButton("submit5", "Salva", class = "btn-primary")
                                                   
                                      )),
                                      
                                      column(3, shinyjs::hidden(
                                        div(
                                          id = "5datainputed_msg",
                                          "Dati inseriti",
                                          actionLink("dsubmit_another", "Inserisci altri dati")
                                        )
                                      )
                                      )),
                                    hr(),
                                    div(
                                      
                                      p("",
                                        a(href="https://docs.google.com/spreadsheets/d/1kJTXGh4op0gawkiVkLchfbFn5Mb_E8oMqbHNDnQ_Iks/edit?usp=sharing",target="_blank",
                                          "Modifica dati"),align="center", style = "font-size:12pt")
                                    ),
                                    hr(),
                                    
                                    fluidRow(
                                      column(1
                                      ),
                                      column(10,DT::dataTableOutput("responsesTable5")),
                                      column(1)
                                    )
                                    
                                    
                                  )
                              )
                              
                     ),
                     
                     
                     
                     
                     tabPanel("Benessere",
                              
                              div(id = "form6",
                                  fluidRow(
                                    column(2,
                                           textInput("banno", "Anno", value = "")),
                                    column(2,
                                           textInput("bmese", "Mese", value = "")),
                                    
                                    column(2,
                                           textInput( "bazienda","Azienda",value="")),
                                    
                                
                                    
                                    column(2,
                                           textInput("bcompl", "ComplBen", value = "")),
                                    
                                    
                                    column(2,
                                           textInput("A", "Area A", value = "")),
                                    
                                    column(2,
                                           textInput("B", "Area B", value = "")),
                                    column(2,
                                           textInput("C", "Area C", value = "")),
                                    column(2,
                                           textInput("Biosic", "Biosic", value = "")),
                                    column(2,
                                           textInput("GR", "GR", value = "")),
                                    
                                    
                                    
                                    fluidRow(
                                      column(9,div(align="center",actionButton("submit6", "Salva", class = "btn-primary")
                                                   
                                      )),
                                      
                                      column(3, shinyjs::hidden(
                                        div(
                                          id = "6datainputed_msg",
                                          "Dati inseriti",
                                          actionLink("bsubmit_another", "Inserisci altri dati")
                                        )
                                      )
                                      )),
                                    hr(),
                                    div(
                                      
                                      p("",
                                        a(href="https://docs.google.com/spreadsheets/d/1kJTXGh4op0gawkiVkLchfbFn5Mb_E8oMqbHNDnQ_Iks/edit?usp=sharing",target="_blank",
                                          "Modifica dati"),align="center", style = "font-size:12pt")
                                    ),
                                    hr(),
                                    
                                    fluidRow(
                                      column(1
                                      ),
                                      column(10,DT::dataTableOutput("responsesTable6")),
                                      column(1)
                                    )
                                    
                                    
                                  )
                              )
                              
                     )
                     )
                     ),

           shinyjs::useShinyjs()
                    )),
  tabPanel("Visualizza dati per azienda",
           fluidPage(
             sidebarPanel(
               selectInput("codaz", "Codice Aziendale",
                           c(unique(as.character(d6$azienda))))
             ),
             
             mainPanel(
               hr(),
               fluidRow(
                 column(12, 
               h2("Dati aziendali")
               ,
               hr(), 
               tableOutput("t1"))
               
             ),
             hr(),
             fluidRow(
               column(12,
                      
                      div(id='clickdiv',
                          h2("Latte di massa")),
                      bsModal("m", "Qualità latte", "clickdiv",plotOutput("pmassa"), size = "large"),
                      hr(),
                      tableOutput("t2"))
             ),
             
             hr(),
             fluidRow(
               column(12,
                      h2("Dati sanitari"),
                      hr(),
                      tableOutput("t3"))
             ),
             hr(),
             fluidRow(
               column(12,
                      div(id='clickdiv2',
                      h2("Parassitologico")),
                      bsModal("p", "Parassitologico", "clickdiv2",plotOutput("paras"), size = "large"),
                      hr(),
                      tableOutput("t4"))
             ),
             hr(),
             fluidRow(
               column(12,
                      h2("Diagnostica"),
                      hr(),
                      tableOutput("t5"))
             ),
             hr(),
             fluidRow(
               column(12,
                      h2("Benessere"),
                      hr(),
                      tableOutput("t6"))
             )
             
             
             
             )
             
             
           )
           
           
           
           
           
           
           
           )

  )
               
               
               
######INSERIMENTO DATI########

# tabPanel("Inserimento dati",
#          div(id = "form",
#              fluidPage(
#                sidebarPanel(
#                  
#                  # textInput("azienda", "Azienda", ""),
#                  # 
#                  # dateInput("dtrisk","Data compilazione", value = Sys.Date(),format = "dd-mm-yyyy"),
#                  
#                  selectInput("settore", "Settore", 
#                              c("Dati Aziendali","Latte di massa",
#                                "Stato Sanitario","Benessere"),""),
#                  br(),
#                  br(),
#                  hr(),
#                  
#                  
#                  shinyjs::hidden(
#                    div(
#                      id = "datainputed_msg",
#                      "Dati inseriti",
#                      actionLink("submit_another", "Inserisci un'altra scheda")
#                    )),
#                  
#                  DT::dataTableOutput("responsesTable")
#                  
#                )
#                
#                ,
#                mainPanel(
#                  wellPanel(
#                    conditionalPanel(
#                      condition = "input.settore == 'Dati Aziendali'",
#                      textInput("mese", "Mese",""),
#                      textInput( "azienda","Azienda",value=""),
#                      textInput( "ncapre","N. capre in lattazione", value = ""),
#                      textInput( "capog","Kg/capo/die", value = ""),
#                      textInput( "adultdead","N.capre adulte morte", value = ""),
#                      textInput("puppydead", "N.capretti morti", value = ""),
#                      textInput( "rim","N.rimonta morte", value = ""),
#                      textInput( "abo","N.aborti", value = ""),
#                      textInput("asc", "N.terapie/asciutta", value = ""),
#                      textInput("trim", "N.terapie/rimonta", value = ""),
#                      textInput("tpuppy", "N.terapie/capretti", value = "")
# 
#                    ),
#                    conditionalPanel(
#                      condition = "input.settore == 'Latte di massa'",
#                      # textInput("mese", "Mese",""),
#                      # textInput( "azienda","Azienda",value=""),
#                      textInput("scc", "SCC", value = ""),
#                      textInput("prot", "Proteine", value = ""),
#                      textInput("cas", "Caseine", value = ""),
#                      textInput("grasso", "Grasso", value = ""),
#                      textInput("latt", "Lattosio", value = ""),
#                      textInput("cbt", "CBT", value = ""),
#                      textInput("urea", "Urea", value = ""),
#                      textInput("inib", "Inibenti", value = "")
# 
#                    ),
#                    conditionalPanel(
#                      condition = "input.settore == 'Stato Sanitario'",
#                      # textInput("mese", "Mese", value = ""),
#                      # textInput( "azienda","Azienda",value=""),
#                      textInput("parat", "Paratbc", value = ""),
#                      textInput("agal", "Agalassia", value = ""),
#                      textInput("caev", "CAEV", value = ""),
#                      textInput("ascessi", "Mal.Ascessi", value = ""),
#                      textInput("mast", "Mastite", value = "")
# 
#                    ),
#                    conditionalPanel(
#                      condition = "input.settore == 'Benessere'",
#                      textInput("mese", "Azienda",""),
#                      textInput( "azienda","Azienda",value=""),
#                      hr(),
#                      
#                      actionButton("submit", "Salva", class = "btn-primary")
#                      
#                      
#                    )
#                    
#                    
#                  )#wellpanel
#                )#mainpanel
#              )#fluidpage
#          )#div form2
# )#,#chiude il tabelPanel

