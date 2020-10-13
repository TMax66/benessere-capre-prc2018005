
server<-function(input, output, session) {
  theme_set(theme_bw())
 
   
  # df<-reactive({      
  # sheet <- gs_read(dati, ws="dataset" )
  #   
  # })
  # 
  # output$dati<- DT::renderDataTable(
  #   loadData(),  rownames = FALSE, options = list(
  #     autoWidth = TRUE,columnDefs = list(list(width = '5%', targets = list(6:15)))
  # ))
  
  df2<-reactive({ 
    dati<-gs_title("prc2018005")
    sheet2 <- gs_read(dati, "UO")
  })
  
  output$UO<- DT::renderDataTable(
    df2(),  rownames = FALSE, options = list(
      autoWidth = TRUE))
  
  tabfin<-reactive({
    sheet3<-gs_read(dati, "finanziamento")
  })
  
  output$risorse<-renderTable(
    tabfin()
  )
  
  tabcons<-reactive({
    
    sheet4<-gs_read(dati, "consumi")
    
  })
  
  
  output$cons<-renderTable(
    tabcons() %>% 
      dplyr::select(c(2,3,9))
  )
  
  ana<-reactive({ ana<-gs_read(analisi,ws="dati") })
  
  output$analisi<- DT::renderDataTable(
    ana(),  rownames = FALSE, options = list(
      autoWidth = TRUE,columnDefs = list(list(width = '5%'))
    ))
  

  
  consumi<-reactive({  
    tabcons() %>% 
      summarise("Utilizzati"=sum(Valore)) %>%
      mutate(
        "Stanziati"=10500,
        "Residuo"= 10500-Utilizzati) %>% 
      dplyr::select(Stanziati, Utilizzati, Residuo)    
    })
 output$consumi<-renderTable(
   consumi()
 )
  
 output$timeline <- renderTimevis({
   timevis(timing)
 })
 
 
 ########INSERIMENTO DATI- dati aziendali##############

 formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- t(data)
      data
    })

    saveData <- function(data) {
      sheet <- gs_title("prc2018005")
      gs_add_row(sheet, ws="dataset",input = data)
    }

###azione al click del bottone save data#####
 observeEvent(input$submit, {
   saveData(formData())
   shinyjs::reset("form")
   output$responsesTable <- DT::renderDataTable(
   loadData(),
   rownames = FALSE,class = 'cell-border stripe',
   options = list(searching = TRUE, lengthChange = FALSE))
   shinyjs::show("datainputed_msg")
 })
 
 ###azione al click del bottone inserisci nuovi dati####
 observeEvent(input$submit_another, {
   shinyjs::show("form")
   shinyjs::hide("datainputed_msg")
 })

 session$onSessionEnded(function() {
   stopApp()
   #q("no")

 })


 output$responsesTable <- DT::renderDataTable(
   loadData(),
   rownames = FALSE,class = 'cell-border stripe',
   options = list(searching = FALSE, lengthChange = FALSE))


 

 
 
###################################################################### 
 
# #####INSERIMENTO dati MASSA######################

 formData2 <- reactive({
   data <- sapply(fieldsAll2, function(x) input[[x]])
   data <- t(data)
   data
 })

 saveData2 <- function(data) {
   # Grab the Google Sheet
   sheet <- gs_title("prc2018005")
   # Add the data as a new row
   gs_add_row(sheet,ws="massa", input = data)
 }

 observeEvent(input$submit2, {
   saveData2(formData2())
   shinyjs::reset("form2")
   output$responsesTable2 <- DT::renderDataTable(
     loadData2(),
     rownames = FALSE,class = 'cell-border stripe',
     options = list(searching = FALSE, lengthChange = FALSE
                    ))
   shinyjs::show("2datainputed_msg")
 })
 observeEvent(input$msubmit_another, {
   shinyjs::show("form2")
   shinyjs::hide("2datainputed_msg")
 })

 session$onSessionEnded(function() {
   stopApp()
   #q("no")

 })


 output$responsesTable2 <- DT::renderDataTable(
   loadData2(),
   rownames = FALSE,class = 'cell-border stripe',
   options = list(searching = FALSE, lengthChange = FALSE
                  ))


######################Inserimento Dati Sanitari#################
 formData3 <- reactive({
   data <- sapply(fieldsAll3, function(x) input[[x]])
   data <- t(data)
   data
 })

 saveData3 <- function(data) {
   # Grab the Google Sheet
   sheet <- gs_title("prc2018005")
   # Add the data as a new row
   gs_add_row(sheet, ws="san",input = data)
 }

 observeEvent(input$submit3, {
   saveData3(formData3())
   shinyjs::reset("form3")
   output$responsesTable3 <- DT::renderDataTable(
     loadData3(),
     rownames = FALSE,class = 'cell-border stripe',
     options = list(searching = FALSE, lengthChange = FALSE))
   shinyjs::show("3datainputed_msg")
 })
 observeEvent(input$ssubmit_another, {
   shinyjs::show("form3")
   shinyjs::hide("3datainputed_msg")
 })

 session$onSessionEnded(function() {
   stopApp()
   #q("no")

 })


 output$responsesTable3 <- DT::renderDataTable(
   loadData3(),
   rownames = FALSE,class = 'cell-border stripe',
   options = list(searching = FALSE, lengthChange = FALSE))



 
######inserimento parassitologico##############
 
 formData4 <- reactive({
   data <- sapply(fieldsAll4, function(x) input[[x]])
   data <- t(data)
   data
 })
 
 saveData4 <- function(data) {
   # Grab the Google Sheet
   sheet <- gs_title("prc2018005")
   # Add the data as a new row
   gs_add_row(sheet, ws="par",input = data)
 }
 
 observeEvent(input$submit4, {
   saveData4(formData4())
   shinyjs::reset("form4")
   output$responsesTable4 <- DT::renderDataTable(
     loadData4(),
     rownames = FALSE,class = 'cell-border stripe',
     options = list(searching = FALSE, lengthChange = FALSE))
   shinyjs::show("4datainputed_msg")
 })
 observeEvent(input$psubmit_another, {
   shinyjs::show("form4")
   shinyjs::hide("4datainputed_msg")
 })
 
 session$onSessionEnded(function() {
   stopApp()
   #q("no")
   
 })
 
 
 output$responsesTable4 <- DT::renderDataTable(
   loadData4(),
   rownames = FALSE,class = 'cell-border stripe',
   options = list(searching = FALSE, lengthChange = FALSE))
 
 
 
 
####inserimento diagnostica#####
 
 formData5 <- reactive({
   data <- sapply(fieldsAll5, function(x) input[[x]])
   data <- t(data)
   data
 })
 
 saveData5 <- function(data) {
   # Grab the Google Sheet
   sheet <- gs_title("prc2018005")
   # Add the data as a new row
   gs_add_row(sheet, ws="diagn",input = data)
 }
 
 observeEvent(input$submit5, {
   saveData5(formData5())
   shinyjs::reset("form5")
   output$responsesTable5 <- DT::renderDataTable(
     loadData5(),
     rownames = FALSE,class = 'cell-border stripe',
     options = list(searching = FALSE, lengthChange = FALSE))
   shinyjs::show("5datainputed_msg")
 })
 observeEvent(input$dsubmit_another, {
   shinyjs::show("form5")
   shinyjs::hide("5datainputed_msg")
 })
 
 session$onSessionEnded(function() {
   stopApp()
   #q("no")
   
 })
 
 
 output$responsesTable5 <- DT::renderDataTable(
   loadData5(),
   rownames = FALSE,class = 'cell-border stripe',
   options = list(searching = FALSE, lengthChange = FALSE))
 
 
 #####inserimento benessere#############
 
 formData6 <- reactive({
   data <- sapply(fieldsAll6, function(x) input[[x]])
   data <- t(data)
   data
 })
 
 saveData6 <- function(data) {
   # Grab the Google Sheet
   sheet <- gs_title("prc2018005")
   # Add the data as a new row
   gs_add_row(sheet, ws="ben",input = data)
 }
 
 observeEvent(input$submit6, {
   saveData6(formData6())
   shinyjs::reset("form6")
   output$responsesTable6 <- DT::renderDataTable(
     loadData6(),
     rownames = FALSE,class = 'cell-border stripe',
     options = list(searching = FALSE, lengthChange = FALSE))
   shinyjs::show("6datainputed_msg")
 })
 observeEvent(input$bsubmit_another, {
   shinyjs::show("form6")
   shinyjs::hide("6datainputed_msg")
 })
 
 session$onSessionEnded(function() {
   stopApp()
   #q("no")
   
 })
 
 
 output$responsesTable6 <- DT::renderDataTable(
   loadData6(),
   rownames = FALSE,class = 'cell-border stripe',
   options = list(searching = FALSE, lengthChange = FALSE))
 
 
##################################################################
##################################################################
 
 output$t1<-renderTable(
   
   d1 %>% 
     filter(azienda==input$codaz) %>% 
     select(-azienda) %>% 
     arrange(mese)
 )

 output$t2<-renderTable(
   
   d2 %>% 
     filter(azienda==input$codaz) %>% 
     select(-azienda) %>% 
     arrange(mese)
 )

 output$t3<-renderTable(
  
  d3 %>% 
     filter(azienda==input$codaz) %>% 
     select(-azienda)%>% 
    arrange(mese)
 )

 output$t4<-renderTable(
  
   d4 %>% 
     filter(azienda==input$codaz) %>% 
     select(-azienda)%>% 
     arrange(mese)
 )

 output$t5<-renderTable(
 
   d5 %>% 
     filter(azienda==input$codaz) %>% 
     select(-azienda)%>% 
     arrange(mese)
 )

 output$t6<-renderTable(
  
  d6 %>% 
     filter(azienda==input$codaz) %>% 
     select(-azienda)%>% 
    arrange(mese)
 )
 
 

 sc<-reactive({d2 %>% 
   filter(azienda==input$codaz) %>% 
   group_by(mese) %>% 
   summarise(scc=geometric.mean(scc, na.rm=T))%>% 
   ggplot(aes(x=mese, y=scc, group=1))+geom_point()+geom_line()})
 
 cb<-reactive({d2 %>% 
   filter(azienda==input$codaz) %>% 
   group_by(mese) %>% 
   summarise(cbt=geometric.mean(cbt, na.rm=T))%>% 
   ggplot(aes(x=mese, y=cbt, group=1))+geom_point()+geom_line()})
 
 pr<-reactive({d2 %>% 
   filter(azienda==input$codaz) %>% 
   group_by(mese) %>% 
   summarise(proteine=mean(proteine, na.rm=T))%>% 
   ggplot(aes(x=mese, y=proteine, group=1))+geom_point()+geom_line()})

  gr<-reactive({d2 %>% 
   filter(azienda==input$codaz) %>% 
   group_by(mese) %>% 
   summarise(grasso= mean(grasso, na.rm=T))%>% 
   ggplot(aes(x=mese, y=grasso, group=1))+geom_point()+geom_line()})
 
  lt<-reactive({d2 %>% 
   filter(azienda==input$codaz) %>% 
   group_by(mese) %>% 
   summarise(lattosio=geometric.mean(lattosio, na.rm=T))%>% 
   ggplot(aes(x=mese, y=lattosio, group=1))+geom_point()+geom_line()})
 
  cs<-reactive({d2 %>% 
   filter(azienda==input$codaz) %>% 
   group_by(mese) %>% 
   summarise(caseina=mean(caseina, na.rm=T))%>% 
   ggplot(aes(x=mese, y=caseina, group=1))+geom_point()+geom_line()})
 
 ur<-reactive({
   d2%>% 
     filter(azienda==input$codaz)%>% 
     group_by(mese)%>% 
     summarise(ureaFTIR=mean(ureaFTIR, na.rm=T),
               ureapHm=mean(ureapHm, na.rm=T))%>%
     pivot_longer(-mese, 
                  names_to = "tecnica", values_to = "urea") %>% 
     ggplot(aes(x=mese, y=urea,color=tecnica) )+geom_line(aes(group=tecnica))+geom_point()
   
 })
 

 
 output$pmassa<-renderPlot(
 ( sc()|cb()|pr()|gr())/(lt()|cs()|ur())
 
     )
 
 
 sc<-reactive({d2 %>% 
     filter(azienda==input$codaz) %>% 
     group_by(mese) %>% 
     summarise(scc=geometric.mean(scc, na.rm=T))%>% 
     ggplot(aes(x=mese, y=scc, group=1))+geom_point()+geom_line()})
 
 
 
stro<-reactive({ d4 %>% 
    filter(azienda==input$codaz) %>% 
    group_by(mese, cat) %>% 
    summarise(strongili=mean(strongili, na.rm=T))%>% 
    ggplot(aes(x=mese, y=strongili, group=1))+geom_point()+geom_line()+ facet_wrap(~cat)  }) 
 
 
cocc<-reactive({ d4 %>% 
    filter(azienda==input$codaz) %>% 
    group_by(mese, cat) %>% 
    summarise(coccidi=mean(coccidi, na.rm=T))%>% 
    ggplot(aes(x=mese, y=coccidi, group=1))+geom_point()+geom_line()+ facet_wrap(~cat)  }) 

 
output$paras<-renderPlot(
  ( cocc()/stro())
)
 
 
 
 
 
 
 
 
}
