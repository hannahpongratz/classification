# Bitte Shiny & Shinyjs installieren!
library(shiny) # install.packages("shiny")
library(shinyjs) # install.packages("shinyjs")

scale_values <- data.frame(mw=c(100,100,50,5),std=c(15,10,10,2),row.names = c("IQ-Skala","Z-Skala","T-Skala","Stanine"))
calc_confidence_interval <- function(test_value,  std, reliability, confidence,decimal_places=0){
  confidence_interval <- qnorm(c((1-confidence/100)/2, 1-(1-confidence/100)/2)) * std * sqrt(1-reliability) + test_value
  return(round(confidence_interval, decimal_places))
}
get_classification <- function(confidence_interval, boundaries){
  c1 = min(which(confidence_interval[1]<c(boundaries, Inf)))
  c2 = max(which(confidence_interval[2]>c(-Inf, boundaries)))
  cs <- unique(c(c1,c2))
  labels <- c("Unterdurchschnittlich", "Durchschnittlich","Überdurchschnittlich")
  return(paste("Klassifikation:", paste(labels[cs],collapse=" - ")))
}
# Define UI for application 
ui <- fluidPage(
  
  useShinyjs(),
  # Application title
  titlePanel("Klassifikation von Normen"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("confidence","Alpha",choices=c("10%"=90,"5%"=95)),
      sliderInput("reliability","Reliabilität",min=0,max=1,step=0.01,value=0.8),
      selectInput("scale","Skala",choices=c("IQ-Skala","Z-Skala","T-Skala","Stanine","Benutzerdefiniert")),
      textInput("value","Testwert",value="105"),
      conditionalPanel(condition="input.scale=='Benutzerdefiniert'",
                       textInput("mw","Mittelwert"),
                       textInput("std","Standardabweichung")
      ),
      splitLayout(
        numericInput("roundTo",label="Nachkommastellen",value=0,min=0,max=10, step=1),
        div(checkboxInput("autoRound",label="auto",value = F),style="padding-top:20px;padding-left:5px;"),
        cellArgs=list(style="overflow:visible"), cellWidths = c("60%","40%")
      ),
      splitLayout(
        actionButton("save_combination","Speichern",width="100%"),
        actionButton("load_combination","Laden",width="100%"),
        cellWidths = c("50%","50%")
      )
    ),
    
    mainPanel(
      textOutput("classification"),
      textOutput("confidence_bounds"),
      div(plotOutput("confidence_plot"),style="height:300px"),
      div(tableOutput("saved_values"), style="font-size:50%"),
      actionButton("export","Exportieren"),
      style="font-size:20px"
      
    )
  )
)

# Define server logic 
server <- function(input, output,session) {
  saved_values <- reactiveVal(data.frame(Maß=character(),
                                         Rohwert=character(),
                                         Mittelwert=character(),
                                         Standardabweichung=character(),
                                         Durchschnittsbereich=character(),
                                         Reliabilität=double(),
                                         Konfidenzintervall=character(),
                                         Klassifikation=character()))
  table_row <- function(name, value, mw, std, rel, conf,roundTo,scale_name){
    if (scale_name != "Benutzerdefiniert"){
      mw <- scale_values[scale_name, "mw"]
      std <- scale_values[scale_name, "std"]
    }
    conf_int <- calc_confidence_interval(as.numeric(value), std,rel, as.numeric(conf),decimal_places=roundTo)
    data.frame(Maß=name,
               Rohwert=paste(round(value,roundTo)),
               Mittelwert=paste(round(mw,roundTo)),
               Standardabweichung=paste(round(std,roundTo)),
               Durchschnittsbereich=paste("[",round(mw-std,roundTo),";",round(mw+std,roundTo),"]",sep=""),
               Reliabilität=rel,
               Konfidenzintervall=paste("[",round(conf_int[1],roundTo),";",round(conf_int[2],roundTo),"]",sep=""),
               Klassifikation=get_classification(conf_int, c(mw-std, mw+std)))
  }
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
      labels <- c("Unterdurchschnittlich", "Durchschnittlich","Überdurchschnittlich")
      return(paste("Klassifikation:", paste(labels[cs],collapse=" - ")))
      
    } else {
      return("Keine zulässigen Angaben")
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
      axis(side = 1, at = axis_bounds, pos = 0, lwd.ticks = 0,labels=c("","Unterdurchschnittlich", "Durchschnittlich","Überdurchschnittlich","")) # Plot axis
      
      # Plot legend
      legend(x=mw-3*std,y=max(y)*1.1,"Testwert", lty=1, col=rgb(0.7,0.1,0.1), box.lty = 0,lwd=2, bg="transparent")
      legend(x=mw-3*std,y=max(y)*1.03,"Konfidenzintervall",fill=rgb(0.7,0.1,0.1,0.2), box.lty = 0, bg="transparent")
      legend(x=mw-3*std,y=max(y)*0.96,"Normverteilung", lty=1,lwd=2, col="black", box.lty = 0, bg="transparent")
    }
  },height=300)
  output$saved_values <- renderTable({
    saved_values()
  },align='l')
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
    showModal(modalDialog(
      title = "Werte speichern",
      textInput("save_name","Maß:",
                value=input$scale),
      actionButton("confirm_save","Bestätigen"),
      modalButton("Abbrechen"),
      easyClose = T,
      footer = NULL
    ))
  })
  observeEvent(input$confirm_save, {
    s_values <- saved_values()
    if (is.element(input$save_name,s_values$Maß)){
      s_values[s_values$Maß==input$save_name,] <- table_row(input$save_name, as.numeric(input$value), input$mw, input$std, input$reliability, input$confidence, input$roundTo, input$scale)
    }else{
      s_values[nrow(s_values)+1,] <- table_row(input$save_name, as.numeric(input$value), input$mw, input$std, input$reliability, input$confidence, input$roundTo, input$scale)
    }
    saved_values(s_values)
    removeModal()
  })
  observeEvent(input$load_combination, {
    if (file.exists("SavedCombinations.csv")){
      saved_combinations <- read.csv2("SavedCombinations.csv", row.names = 1)
      options <- rownames(saved_combinations)
      showModal(modalDialog(
        title = "Werte laden",
        selectInput("select_load","Werte auswählen",choices <- options),
        actionButton("confirm_load","Bestätigen"),
        modalButton("Abbrechen"),
        easyClose = T,
        footer = NULL
      ))
    } else{
      showModal(modalDialog(
        title = "Werte laden",
        "Noch keine gespeicherten Werte",
        modalButton("Abbrechen"),
        easyClose = T,
        footer = NULL
      ))
    }
  })
  observeEvent(input$confirm_load, {
    saved_combinations <- read.csv2("SavedCombinations.csv", row.names = 1)
    selected <- saved_combinations[input$select_load,]
    updateSelectInput(session, "scale", selected=selected$scale)
    if(selected$scale == 'Benutzerdefiniert'){
      updateTextInput(session, "mw", value=selected$MW)
      updateTextInput(session, "std", value=selected$STD)
    }
    updateTextInput(session, "value", value=selected$Testwert)
    updateCheckboxInput(session, "autoRound",value=F)
    updateNumericInput(session, "roundTo",value=selected$roundTo)
    updateSliderInput(session, "reliability", value=selected$reliability)
    updateSelectInput(session, "confidence", selected=selected$confidence)
    removeModal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
