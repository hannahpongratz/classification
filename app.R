# Bitte shiny & shinyjs installieren!
library(shiny) # install.packages("shiny")
library(shinyjs) # install.packages("shinyjs")

# Wenn direkt in Word exportiert werden soll bitte auch Pakete officer & flextable installieren
officer_loaded = require(officer) # install.packages("officer")
flextable_loaded = require(flextable) # install.packages("flextable")

scale_values <- data.frame(mw=c(100,100,50,5),std=c(15,10,10,2),row.names = c("IQ-Skala","Z-Skala","T-Skala","Stanine"))

# Define UI for application 
ui <- fluidPage(
  
  useShinyjs(),
  # Application title
  titlePanel("Klassifikation von Testwerten"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("parameter", "Name",""),
      selectInput("confidence","Alpha",choices=c("10%"=90,"5%"=95)),
      sliderInput("reliability","Reliabilität",min=0,max=1,step=0.01,value=0.8),
      selectInput("scale","Skala",choices=c("IQ-Skala","Z-Skala","T-Skala","Stanine","Benutzerdefiniert")),
      textInput("value","Testwert",value=""),
      conditionalPanel(condition="input.scale=='Benutzerdefiniert'",
                       textInput("mw","Mittelwert"),
                       textInput("std","Standardabweichung")
      ),
      splitLayout(
        numericInput("roundTo",label="Nachkommastellen",value=0,min=0,max=10, step=1),
        div(checkboxInput("autoRound",label="auto",value = F),style="padding-top:20px;padding-left:5px;"),
        cellArgs=list(style="overflow:visible"), cellWidths = c("60%","40%")
      ),
      actionButton("save_combination","Wert speichern"),
      actionButton("load_combination","Wert bearbeiten"),
      actionButton("new_combination","Neuer Wert"),
      actionButton("delete_combination","Wert(e) loeschen"),
      
    ),
    
    mainPanel(
      textOutput("classification"),
      textOutput("confidence_bounds"),
      div(plotOutput("confidence_plot"),style="height:300px"),
      textOutput("dataset_name"),
      div(tableOutput("saved_values"), style="font-size:65%"),
      actionButton("save_table","Datensatz speichern"),
      actionButton("load_table","Datensatz laden"),
      actionButton("new_table","Neuer Datensatz"),
      actionButton("export","Exportieren (Word)"),
      style="font-size:20px"
      
    )
  )
)

# Define server logic 
server <- function(input, output,session) {
  # Setup 
  if (!(officer_loaded)&flextable_loaded){
    hide("export")
  } 
  loaded_table <- reactiveVal()
  edit_mode <- 0
  saved_tables <- list.files(pattern=".RData")
  if (length(saved_tables)==0){
    saved_values <- reactiveVal(empty_table_row())
  } else {
    saved_tables_info <- file.info(saved_tables)
    saved_tables <- saved_tables[order(saved_tables_info$mtime,decreasing = T)]
    load(saved_tables[1])
    loaded_table(sub(".RData", "", saved_tables[1]))
    saved_values <- reactiveVal(s_values)
  }
  
  # Outputs
  output$classification <- renderText({
    confidence <- as.numeric(input$confidence)
    test_value <- as.numeric(gsub(",", ".", input$value))
    if (input$scale == 'Benutzerdefiniert'){
      std <- as.numeric(gsub(",", ".", input$std))
      mw <- as.numeric(gsub(",", ".", input$mw))
    } else {
      std <- scale_values[input$scale, "std"]
      mw <- scale_values[input$scale, "mw"]
    }
    if (!(is.na(std) | is.na(mw) | is.na(test_value))){
      boundaries <- c(mw-std, mw+std)
      confidence_interval <- calc_confidence_interval(test_value,  std, input$reliability, confidence,input$roundTo)
      c1 = min(which(confidence_interval[1]<c(boundaries, Inf)))
      c2 = max(which(confidence_interval[2]>c(-Inf, boundaries)))
      cs <- unique(c(c1,c2))
      labels <- c("Unterdurchschnittlich", "Durchschnittlich","Ueberdurchschnittlich")
      return(paste("Klassifikation:", paste(labels[cs],collapse=" - ")))
      
    } else {
      return("Keine zulaessigen Angaben")
    }
  })
  output$confidence_bounds <- renderText({
    confidence <- as.numeric(input$confidence)
    test_value <- as.numeric(gsub(",", ".", input$value))
    if (input$scale == 'Benutzerdefiniert'){
      std <- as.numeric(gsub(",", ".", input$std))
    } else {
      std <- scale_values[input$scale, "std"]
    }
    if (!(is.na(test_value) | is.na(std))){
      confidence_interval <- calc_confidence_interval(test_value,  std, input$reliability, confidence,input$roundTo)
      return(paste("Grenzen Konfidenzintervall: [", 
                   round(confidence_interval[1],2),";",
                   round(confidence_interval[2],2),"]",
                   sep=""))
    }
  })
  
  output$confidence_plot <- renderPlot({
    confidence <- as.numeric(input$confidence)
    test_value <- as.numeric(gsub(",", ".", input$value))
    if (input$scale == 'Benutzerdefiniert'){
      std <- as.numeric(gsub(",", ".", input$std))
      mw <- as.numeric(gsub(",", ".", input$mw))
    } else {
      std <- scale_values[input$scale, "std"]
      mw <- scale_values[input$scale, "mw"]
    }
    if (!(is.na(std) | is.na(mw) | is.na(test_value))){
      x <- seq(mw-3*std, mw+3*std, length = 501)
      y <- dnorm(x, mw, std)
      conf_interval = calc_confidence_interval(test_value, std, input$reliability, confidence,input$roundTo)
      axis_bounds <- c(mw-3*std,mw-2*std,mw,mw+2*std,mw+3*std)
      par(mar=c(3,0,0,0))
      plot(c(test_value, test_value), c(0, dnorm(test_value, mw, std)), type = "l", axes = FALSE, 
           xlab = "", ylab = "",ylim=c(0,max(y)*1.1),xlim=axis_bounds[c(1,5)],col=rgb(0.7,0.1,0.1),lwd=4) # Plot value of subject
      
      lines(x,y,lty=1,lwd=3) # Add normal distribution
      lines(c(mw-std,mw-std), c(0, dnorm(mw-std, mw, std)),lwd = 1.5, lty = 3) # Plot lines at +/-1 SD
      lines(c(mw+std,mw+std), c(0, dnorm(mw+std, mw, std)),lwd = 1.5, lty = 3)
      
      x_errorshade = c(conf_interval[1],x[x>=conf_interval[1] & x<=conf_interval[2]], conf_interval[2])
      y_errorshade = c(0,y[x>=conf_interval[1] & x<=conf_interval[2]],0)
      polygon(x_errorshade, y_errorshade, col = rgb(0.7,0.1,0.1,0.2),border = NA) # Plot confidence interval
      axis(side = 1, at = axis_bounds, pos = 0, lwd.ticks = 0,labels=c("","Unterdurchschnittlich", "Durchschnittlich","Ueberdurchschnittlich","")) # Plot axis
      
      # Plot legend
      legend(x=mw-3*std,y=max(y)*1.1,"Testwert", lty=1, col=rgb(0.7,0.1,0.1), box.lty = 0,lwd=2, bg="transparent")
      legend(x=mw-3*std,y=max(y)*1.03,"Konfidenzintervall",fill=rgb(0.7,0.1,0.1,0.2), box.lty = 0, bg="transparent")
      legend(x=mw-3*std,y=max(y)*0.96,"Normverteilung", lty=1,lwd=2, col="black", box.lty = 0, bg="transparent")
    }
  },height=300)
  
  output$dataset_name <- renderText({loaded_table()})
  
  output$saved_values <- renderTable({
    s_values <- saved_values()
    if (length(unique(s_values$scale))== 1 & unique(s_values$scale)[1]!= "Benutzerdefiniert"){
      table_names <- c("Mass",sub("Skala", "Wert", unique(s_values$scale)),"Reliabilitaet","KI","Klassifikation")
      s_values <- s_values[,c(1,2,6,7,8)]
      names(s_values) <- table_names
    } else{
      s_values <- s_values[,1:8]
      names(s_values) <- c("Mass","Rohwert","MW","STD","Durchschnittb.","Reliabilitaet","KI","Klassifikation")
    }
    return(s_values)
  },align='l')
  
  # Observes
  observeEvent(input$autoRound, {
    if (input$autoRound){
      shinyjs::disable("roundTo")
    }else{
      shinyjs::enable("roundTo")
    }
  })
  observe({
    confidence <- as.numeric(input$confidence)
    test_value <- as.numeric(gsub(",", ".", input$value))
    if (input$scale == 'Benutzerdefiniert'){
      std <- as.numeric(gsub(",", ".", input$std))
      mw <- as.numeric(gsub(",", ".", input$mw))
    }else{
      std <- scale_values[input$scale, "std"]
      mw <- scale_values[input$scale, "mw"]
    }
    if (input$autoRound){
      auto_decimal <- 0
      while(T){
        values <- c(mw, std, test_value, confidence) * (10^auto_decimal)
        if (all(values == round(values),na.rm=T)){break}
        auto_decimal = auto_decimal+1
      }
      updateNumericInput(session,"roundTo",label="Nachkommastellen",value=auto_decimal,min=0,max=10, step=1)
    }
  })
  observeEvent(input$save_combination, {
    if (input$parameter != ""){
      s_values <- saved_values()
      if (edit_mode>0){
        s_values[edit_mode,] <- table_row(input$parameter, as.numeric(input$value), input$mw, input$std, input$reliability, input$confidence, input$roundTo,input$autoRound, input$scale)
      }else{
        s_values[nrow(s_values)+1,] <- table_row(input$parameter, as.numeric(input$value), input$mw, input$std, input$reliability, input$confidence, input$roundTo, input$autoRound, input$scale)
      }
      saved_values(s_values)
    } else {
      showModal(modalDialog(
        "Bitte zuerst Name eingeben!", br(),
        modalButton("Ok"),
        easyClose = T,
        footer = NULL
      ))
    }
    
  })
  observeEvent(input$load_combination, {
    s_values <- saved_values()
    if (nrow(s_values>0)){
      options <- s_values$name
      showModal(modalDialog(
        title = "Wert laden",
        selectInput("select_edit","Wert auswaehlen",choices = options),
        actionButton("confirm_edit","Bestaetigen"),
        modalButton("Abbrechen"),
        easyClose = T,
        footer = NULL
      ))
    } else{
      showModal(modalDialog(
        title = "Wert laden",
        "Noch keine gespeicherten Werte",br(),
        modalButton("Abbrechen"),
        easyClose = T,
        footer = NULL
      ))
    }
  })
  observeEvent(input$confirm_edit, {
    s_values <- saved_values()
    edit_mode <<- which(s_values$name==input$select_edit)
    selected <- s_values[s_values$name==input$select_edit,]
    updateSelectInput(session, "scale", selected=selected$scale)
    if(selected$scale == 'Benutzerdefiniert'){
      updateTextInput(session, "mw", value=selected$MW)
      updateTextInput(session, "std", value=selected$STD)
    }
    updateTextInput(session, "parameter", value=selected$name)
    updateTextInput(session, "value", value=selected$value)
    updateCheckboxInput(session, "autoRound",value=F)
    updateNumericInput(session, "roundTo",value=selected$roundTo)
    updateSliderInput(session, "reliability", value=selected$reliability)
    updateSelectInput(session, "confidence", selected=selected$confidence)
    updateActionButton(session, "save_combination","Aenderungen speichern")
    removeModal()
  })
  observeEvent(input$new_combination, {
    edit_mode <<- 0
    updateActionButton(session, "save_combination","Wert speichern")
    updateTextInput(session, "parameter", value="")
    updateTextInput(session, "value", value="")
  })
  observeEvent(input$delete_combination,{
    s_values = saved_values()
    if (nrow(s_values)>0){
      choices = s_values$name
      showModal(modalDialog(
        title = "Werte loeschen",
        selectInput("values_to_delete", "Auswaehlen:", choices = choices, multiple=T),
        actionButton("confirm_delete","Loeschen"),
        modalButton("Abbrechen"),
        easyClose = T,
        footer = NULL
      ))
    }else{
      showModal(modalDialog(
        title = "Wert laden",
        "Noch keine gespeicherten Werte",br(),
        modalButton("Abbrechen"),
        easyClose = T,
        footer = NULL
      ))
    }
  })
  observeEvent(input$confirm_delete,{
    s_values = saved_values()
    s_values <- s_values[!is.element(s_values$name, input$values_to_delete),]
    saved_values(s_values)
    removeModal()
  })
  observeEvent(input$save_table,{
    showModal(modalDialog(
      title = "Datensatz speichern",
      textInput("dataset_name","Speichern als:",loaded_table()),
      actionButton("confirm_save","Bestaetigen"),
      modalButton("Abbrechen"),
      easyClose = T,
      footer = NULL
    ))
  })
  observeEvent(input$confirm_save,{
    s_values <- saved_values()
    save(s_values,file=paste(input$dataset_name,".RData",sep=""))
    loaded_table(input$dataset_name)
    removeModal()
  })
  observeEvent(input$load_table,{
    file_names <-  sub(".RData","",list.files(pattern=".RData"))
    if (length(file_names)>0){
      showModal(modalDialog(
        title = "Datensatz speichern",
        selectInput("to_load","Datensatz:", choices = file_names),
        actionButton("confirm_load","Bestaetigen"),
        modalButton("Abbrechen"),
        easyClose = T,
        footer = NULL
      ))
    }else{
      showModal(modalDialog(
        "Noch keine gespeicherten Datensaetze", br(),
        modalButton("Abbrechen"),
        easyClose = T,
        footer = NULL
      ))
    }
  })
  observeEvent(input$confirm_load, {
    load(paste(input$to_load, ".RData",sep=""))
    saved_values(s_values)
    loaded_table <<- paste(input$to_load, ".RData",sep="")
    removeModal()
  })
  observeEvent(input$export,{
    choices <- unique(c(loaded_table(),sub(".RData","",list.files(pattern=".RData"))))
    showModal(modalDialog(
      title="Exportieren",
      selectInput("datasets_to_export","Dastensaetze auswaehlen",choices = choices, multiple = T, selected =loaded_table()),
      textInput("export_to","Dateiname:","ZKEA"),
      "Hinweis: Existiert bereits ein Word-Dokument mit diesem Namen wird der Inhalt ueberschrieben",br(),
      checkboxInput("rel_table","Tabelle mit Reliabilitaeten hinzufuegen",T),
      actionButton("confirm_export","Bestaetigen"),
      modalButton("Abbrechen"),
      easyClose = T,
      footer = NULL
    ))
  })
  observeEvent(input$new_table, {
    showModal(modalDialog(
      title="Neuer Datensatz",
      textInput("new_dataset_name","Testname:"),
      "Hinweis: Alle nicht gespeicherten Aenderungen gehen verloren.",
      br(),
      actionButton("confirm_new_table","Bestaetigen"),
      modalButton("Abbrechen"),
      easyClose = T,
      footer = NULL
    ))
  })
  observeEvent(input$confirm_new_table, {
    s_values <- empty_table_row()
    loaded_table(input$new_dataset_name)
    saved_values(s_values)
    removeModal()
  })
  observeEvent(input$confirm_export,{
    d <- read_docx()
    datasets_tests <- input$datasets_to_export
    for (i in 1:length(datasets_tests)){
      if (datasets_tests[i] != loaded_table()){
        load(paste(datasets_tests[i],"RData",sep="."))
        d <- add_tables(d,s_values,datasets_tests[i])
      } else {
        d <- add_tables(d,saved_values(),loaded_table())
      }
    }
    print(d, target = paste(input$export_to,".docx",sep=""))
    removeModal()
  })
  format_flex_table <- function(tab){
    s_flex <- flextable(tab)
    s_flex <- font(s_flex,fontname="Times New Roman",part="all")
    s_flex <- border_remove(s_flex)
    s_flex <- hline_top(s_flex, border = NULL, part = "body")
    s_flex <- border_inner_h(s_flex, border = NULL, part = "all")
    s_flex <- bold(s_flex,bold=F,part="header")
    s_flex <- align(s_flex, align="left",part="all")
    s_flex <- padding(s_flex, padding = 0, part = "all")
    return(s_flex)
  }
  add_tables <- function(d,s_values, loaded_table){
    my_format <- fp_text(font = "Times New Roman", font.size=11)
    set_flextable_defaults(font.size = 11, theme_fun = theme_vanilla)
    if (is.null(loaded_table)){
      testname <- "Test"
    } else {
      testname <- loaded_table
    }
    if(input$rel_table){
      rel_table <- s_values[,c("name","reliability")]
      rel_table$classification <- unlist(lapply(rel_table[,2], function(x){
        classes <- c("Inadaequat", "Adaequat", "Gut","Exzellent")
        classes[max(which(x >= c(0, 0.6, 0.7, 0.8)))]
      }))
      names(rel_table) <- c("Mass","Wert", "Klassifikation nach Evers et al. (2013)")
      rel_table_flex <- format_flex_table(rel_table)
      rel_table_flex <- width(rel_table_flex, width = c(3,1.5,6), unit = "cm")
      rel_table_flex <- set_caption(rel_table_flex, as_paragraph(as_i(as_chunk("Tabelle 0",my_format))
                                                                 ,as_chunk(paste(". Zufallskritische Einzelfallauswertung des ",testname,", Reliabilitaet",sep=""),props = my_format)),fp_p = fp_par(padding = 0))
    }
    if (length(unique(s_values$scale))== 1 & unique(s_values$scale)[1]!= "Benutzerdefiniert"){
      table_names <- c("Mass",sub("Skala", "Wert", unique(s_values$scale)),"Reliabilitaet","Konfidenzintervall des Testwerts","Klassifikation nach Westhoff und Kluck (2014)")
      s_values <- s_values[,c(1,2,6,7,8)]
      names(s_values) <- table_names
      s_flex <- format_flex_table(s_values)
      s_flex <- width(s_flex, width = c(2,1.5,2.1,4.8,5), unit = "cm")
    } else{
      s_values <- s_values[,1:8]
      names(s_values) <- c("Mass","Rohwert","Mittelwert der Rohwerte","Standardabweichung der Rohwerte",
                           "Durchschnittbereich der Rohwerte nach Westhoff und Kluck (2014)","Reliabilitaet",
                           "Konfidenzintervall des Testwerts","Klassifikation nach Westhoff und Kluck (2014)")
      s_flex <- format_flex_table(s_values)
      s_flex <- width(s_flex, width = c(1.5,1.4,2,2,3,1.5,2.5,2.5), unit = "cm")
    }
    s_flex <- set_caption(s_flex, as_paragraph(as_i(as_chunk("Tabelle 0",my_format))
                                               ,as_chunk(paste(". Zufallskritische Einzelfallauswertung des ",testname,", Konfidenzintervalle der wahren Testwerte",sep=""),
                                                         props = my_format)),fp_p = fp_par(padding = 0))
    
    if(input$rel_table){
      d <- body_add_flextable(d, value = rel_table_flex,align="left")
      d <- body_add_par(d,"")
    }
    d <- body_add_flextable(d,value = s_flex,align="left")
    return(d)
  }
  
}
calc_confidence_interval <- function(test_value,  std, reliability, confidence,decimal_places=0){
  confidence_interval <- qnorm(c((1-confidence/100)/2, 1-(1-confidence/100)/2)) * std * sqrt(1-reliability) + test_value
  return(round(confidence_interval, decimal_places))
}
get_classification <- function(confidence_interval, boundaries){
  c1 = min(which(confidence_interval[1]<c(boundaries, Inf)))
  c2 = max(which(confidence_interval[2]>c(-Inf, boundaries)))
  cs <- unique(c(c1,c2))
  labels <- c("Unterdurchschnittlich", "Durchschnittlich","Ueberdurchschnittlich")
  return(paste(paste(labels[cs],collapse=" - ")))
}

empty_table_row <- function(){
  return(data.frame(name=character(),
                    value=character(),
                    mw=character(),
                    std=character(),
                    avg_interval=character(),
                    reliability=double(),
                    conf_interval=character(),
                    classification=character(),
                    scale=character(),
                    confidence=integer(),
                    roundTo=integer(),
                    autoRound=logical()))
}
table_row <- function(name, value, mw, std, rel, conf, roundTo, autoRound, scale_name){
  if (scale_name != "Benutzerdefiniert"){
    mw <- scale_values[scale_name, "mw"]
    std <- scale_values[scale_name, "std"]
  }
  conf_int <- calc_confidence_interval(as.numeric(value), std,rel, as.numeric(conf),decimal_places=roundTo)
  data.frame(name=name,
             value=paste(round(value,roundTo)),
             mw=paste(round(mw,roundTo)),
             std=paste(round(std,roundTo)),
             avg_interval=paste("[",round(mw-std,roundTo),"; ",round(mw+std,roundTo),"]",sep=""),
             reliability=rel,
             conf_interval=paste("[",round(conf_int[1],roundTo),"; ",round(conf_int[2],roundTo),"]",sep=""),
             classification=get_classification(conf_int, c(mw-std, mw+std)),
             scale=scale_name,
             confidence=conf,
             roundTo=roundTo,
             autoRound=autoRound)
}
# Run the application 
shinyApp(ui = ui, server = server)
