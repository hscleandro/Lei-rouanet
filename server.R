# Dean Attali
# November 21 2014

# This is the server portion of a shiny app shows cancer data in the United
# States

#source("helpers.R")  # have the helper functions avaiable

library(shiny)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyjs)
library(plotly)

# Get the raw data
#cDatRaw <- getData()
load("./rouanet.Rdata")
cDatRaw <- rouanet.tab
# Get the list of colours to use for plotting
#plotCols <- getPlotCols()

shinyServer(function(input, output, session) {
  
  # =========== BUILDING THE INPUTS =========== 
  
  printInShinyBox <- function(phrase){
    foo <- function(message_teste) {
      message(message_teste)
      Sys.sleep(0.5)
    }
    
    withCallingHandlers({
      shinyjs::html("text", "")
      foo(phrase)
    },
    message = function(m) {
      shinyjs::html(id = "text", html = m$message, add = TRUE)
    })
  }
  
  printInShinyBoxj <- function(phrase){
    foo <- function(message_teste) {
      message(message_teste)
      Sys.sleep(0.5)
    }
    
    withCallingHandlers({
      shinyjs::html("textj", "")
      foo(phrase)
    },
    message = function(m) {
      shinyjs::html(id = "textj", html = m$message, add = TRUE)
    })
  }
  
  
  output$pfvaluerUi <- renderUI({
    p.fisica <- cDatRaw$Captacao[which(cDatRaw$Tipo.de.Pessoa == "FÍSICA")]
    sliderInput("valuesf", 
                label = "",
                min = min(p.fisica), 
                max = max(p.fisica),
                value = range(p.fisica),
                step = 50000)
  })
  
  output$pjvaluerUi <- renderUI({
    p.juridica <- cDatRaw$Captacao[which(cDatRaw$Tipo.de.Pessoa == "JURÍDICA")]
    sliderInput("valuesj", 
                label = "",
                min = min(p.juridica), 
                max = max(p.juridica),
                value = range(p.juridica),
                step = 50000)
  })
  
  
	# ============== MANIPULATE THE DATA ================

	# The dataset to show/plot, which is the raw data after filtering based on
	# the user inputs
	cDat <- reactive({
		# Add dependency on the update button (only update when button is clicked)
	
		#data <- cDatRaw
		data <- cDatTable()
		
		# Add all the filters to the data based on the user inputs
		# wrap in an isolate() so that the data won't update every time an input
		# is changed
		#isolate({
		if (input$query == "Pessoa Fisica") {
		  data %<>%
		    filter(Captacao >= input$valuesf[1] & Captacao <= input$valuesf[2])
		  message <- paste0("<b>Média de captação</b></br> R$",format(mean(data$Captacao), nsmall = 2))
		  message <- paste0(message,"</br>","<b>Maior valor captado</b></br> R$",format(max(data$Captacao), nsmall = 2))
		  message <- paste0(message,"</br>","<b>Menor valor captado</b></br> R$",format(min(data$Captacao), nsmall = 2))
		  message <- paste0(message,"</br>","<b>Quantidade de aprovações:</b> ",nrow(data))
		  printInShinyBox(message)
		}
		
		if (input$query == "Pessoa Juridica") {
		  data %<>%
		    filter(Captacao >= input$valuesj[1] & Captacao <= input$valuesj[2])
		  message <- paste0("<b>Média de captação</b></br> R$",format(mean(data$Captacao), nsmall = 2))
		  message <- paste0(message,"</br>","<b>Maior valor captado</b></br> R$",format(max(data$Captacao), nsmall = 2))
		  message <- paste0(message,"</br>","<b>Menor valor captado</b></br> R$",format(min(data$Captacao), nsmall = 2))
		  message <- paste0(message,"</br>","<b>Quantidade de aprovações:</b> ",nrow(data))
		  printInShinyBoxj(message)
		}
			# Filter values
			
			
		#})

		data
	})
	
  # The data to show in a table, which is essentially the same data as above
	# with all the filters, but formatted differently:
	# - Format the numbers to look better in a table
	# - Change the data to wide/long format (the filtered data above is long)
	cDatTable <- reactive({
		#data <- cDat()
		data <- cDatRaw
		# In numeric columns show 2 digits past the decimal and don't show
		# decimal if the number is a whole integer
		#data %<>%
		#	mutate(value = formatC(data$value, format = "fg", digits = 2))		
		
		# Change the data to wide format if the user wants it
		if (input$query == "Pessoa Fisica") {
		  data %<>%
		    filter(Tipo.de.Pessoa == "FÍSICA")
		  #message <- paste0("<b>Média de captação</b> R$",format(mean(data$Captacao), nsmall = 2))
		  #message <- paste0(message,"</br>","<b>Maior valor captado</b> R$",format(max(data$Captacao), nsmall = 2))
		  #message <- paste0(message,"</br>","<b>Menor valor captado</b> R$",format(min(data$Captacao), nsmall = 2))
		  #message <- paste0(message,"</br>","<b>Quantidade de aprovações:</b> ",nrow(data))
		  #printInShinyBox(message)
		}
		
		if (input$query == "Pessoa Juridica") {
		  data %<>%
		    filter(Tipo.de.Pessoa == "JURÍDICA")
		  #message <- paste0("<b>Média de captação</b> R$",format(mean(data$Captacao), nsmall = 2))
		  #message <- paste0(message,"</br>","<b>Maior valor captado</b> R$",format(max(data$Captacao), nsmall = 2))
		  #message <- paste0(message,"</br>","<b>Menor valor captado</b> R$",format(min(data$Captacao), nsmall = 2))
		  #message <- paste0(message,"</br>","<b>Quantidade de aprovações:</b> ",nrow(data))
		  #printInShinyBoxj(message)
		}
		
		
		
		data[,c(1,3,5,9,10)]
	})
	
	cDatTablePersone <- reactive({
	  data <- cDat()
	  
	  if ((input$query == "Pessoa Fisica")&(input$searchpf == "Buscar Proponente")) {
	    if((input$proponentepf != "")&(input$todosproppf == FALSE)){
	      data %<>%
	        filter(Proponente == input$proponentepf)
	    }
	  }
	  else if((input$query == "Pessoa Fisica")&(input$searchpf == "Buscar Projeto")) {
	    
	    if((input$projetopf != "")&(input$todosprojpf == FALSE)){
	      data %<>%
	        filter(Projetos == input$projetopf)
	    }
	  }
	  else if((input$query == "Pessoa Juridica")&(input$searchpf == "Buscar Proponente")) {
	    
	    if((input$proponentepj != "")&(input$todosproppj == FALSE)){
	      data %<>%
	        filter(Proponente == input$proponentepj)
	    }
	  }
	  else if((input$query == "Pessoa Juridica")&(input$searchpf == "Buscar Projeto")) {
	  
	    if((input$projetopj != "")&(input$todosprojpj == FALSE)){
	      data %<>%
	        filter(Projetos == input$projetopj)
	    }
	    
	  }
	  
	  
	  data
	  
	})
	
	# ============= TAB TO SHOW DATA IN TABLE ===========
	
	# Show the data in a table
	output$dataTable <- renderTable({
			#cDatTable()
		  data <- cDatTablePersone()[,c(1,2,3,5)]
		  data
		},
		include.rownames = FALSE
	)
	
	# Allow user to download the data, simply save as csv
	
	# ============= TAB TO PLOT DATA ===========
	
	buildPlot <- reactive({
	  generic.tab <- cDatTable()
	  generic.tab$Color <- rep("#e5e5ff",nrow(generic.tab))
	  
	  temp.tab <- cDatTablePersone()
	  index <- which(generic.tab$Proponente %in% temp.tab$Proponente)
	  generic.tab$Color[index] <- "#0080ff"
	  
	  y <- list(
	    title = "Número de Proponentes",
	    zerolinecolor = toRGB("white")
	  )
	  
	  x <- list(
	    title = "Valor total capitado (Milhões)",
	    #zeroline = FALSE,
	    zerolinecolor = toRGB("white")
	  )
	  
	  p <- plot_ly(generic.tab, 
	          x = 1:nrow(generic.tab), 
	          y = Captacao, 
	          #type = "bar",
	          size = liberacoes, 
	          mode = "markers",
	          #color = rep("#009380",nrow(rouanet.tab)),
	          hoverinfo = "text", 
	          text = paste("Proponente:",generic.tab$Proponente,
	                       "<br>",
	                       "Valor capitado:",as.character(generic.tab$Captacao)),
	          marker = list(color = generic.tab$Color) 
	          #colors =  Color
	  ) %>%
	    layout(xaxis = y, yaxis = x, showlegend = FALSE)
	  
	  p
	  
	})
	
	buildBoxPlot <- reactive({
	  generic.tab <- cDatTablePersone()
	  #generic.tab$Color <- rep("#e5e5ff",nrow(generic.tab))
	  
	  x <- list(
	    title = "Valor total capitado (Milhões)",
	    #zeroline = FALSE,
	    zerolinecolor = toRGB("white")
	  )
	  
	  p <- plot_ly(y = generic.tab$Captacao, 
	               boxpoints = "all",
	               type = "box"
	               )%>%
	    layout(yaxis = x)
	  #%>%
	   # layout(xaxis = y, yaxis = x, showlegend = FALSE)
	  
	  p
	  
	})
	
	#buildBarPlot <- reactive({})
	# Function to build the plot object
	
	# Show the plot, use the width/height that javascript calculated
	output$dataPlot <- 
	   renderPlotly({
	     if(input$tploty == "Bubble chart")
	        buildPlot()
	     else if(input$tploty == "Boxplot")
	     {
	        buildBoxPlot()
	     }
	    })
	    #height = function(){ input$plotDim },
	    #width = function(){ input$plotDim },
	    #units = "px",
	    #res = 100


	# Allow user to download the plot
		
	
	
	# ========== LOADING THE APP ==========
	
	# We need to have a quasi-variable flag to indicate when the app is loaded
	dataValues <- reactiveValues(
		appLoaded = FALSE
	)
	
	# Wait for the values input to be rendered as a proxy to determine when the app
	# is loaded. Once loaded, call the javascript funtion to fix the plot area
	# (see www/helper-script.js for more information)
	# Show form content and hide loading message
	
	hide("loadingContent")
	show("allContent")
})
