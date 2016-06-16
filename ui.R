# Dean Attali
# November 21 2014

# This is the ui portion of a shiny app shows cancer data in the United States

library(shiny)
library(shinyjs)
library(plotly)
load("./rouanet.Rdata")
source("./contacts.R")
fluidPage(
  useShinyjs(),
  
	# add custom JS and CSS
	singleton(
		tags$head(includeScript(file.path('www', 'message-handler.js')),
							includeScript(file.path('www', 'helper-script.js')),
							includeCSS(file.path('www', 'style.css'))
		)
	),
	
	# enclose the header in its own section to style it nicer
	div(id = "headerSection",
		titlePanel("Captação de projetos da lei Rouanet (2011 - 2015)"),
	
		# author info
		span(
			span("Criado por "),
			a("Leandro Corrêa", href = "https://twitter.com/hscleando"),
			HTML("&bull;"),
			span("Código"),
			a("no GitHub", href = "https://github.com/daattali/shiny-server/tree/master/cancer-data"),
			br(),
			
			span("15 de Junho, 2016")
		)
	),
	
	# show a loading message initially
	div(
		id = "loadingContent",
		h2("Loading...")
	),	
	
	# all content goes here, and is hidden initially until the page fully loads
	hidden(div(id = "allContent",
		
		# sidebar - filters for the data
		sidebarLayout(
			sidebarPanel(
			  strong(span("Tipo de consulta:")),
			  #span("Tipo de consulta:"),
			  radioButtons(inputId = "query",
			               label = "",
			               choices = c("Pessoa Fisica" = "Pessoa Fisica", 
			                           "Pessoa Juridica" = "Pessoa Juridica"),
			               inline = FALSE),
			  
# ============================Pessoa_Física======================================			  
			  conditionalPanel(
			    condition = "input.query == 'Pessoa Fisica'",
			  
			    br(),
			    strong(span("Range de valores captados:")),
			    uiOutput("pfvaluerUi"), 
			    br(),
			    strong(span("Indicadores:")),
			    shinyjs::useShinyjs(),
			    wellPanel(
			      shiny::tags$head(shiny::tags$style(shiny::HTML(
			        "#text {id: text; font-size: 14px; height: 170px; overflow: auto; margin-left: -14px;}"
			      )),
			      shiny::tags$script(src = 'URL.js')),
			      
			      div(id = "text")
			    ),
			    
			    strong(span("Executar busca:")),
			    #span("Tipo de consulta:"),
			    radioButtons(inputId = "searchpf",
			                 label = "",
			                 choices = c("Buscar Proponente" = "Buscar Proponente", 
			                             "Buscar Projeto" = "Buscar Projeto"),
			                 inline = FALSE),
			    br(),
 # =====================Pessoa_Física__Proponente==================================	
			    conditionalPanel(
			      condition = "input.searchpf == 'Buscar Proponente'",
			      
			    strong(span("Proponente:")),
			    # show all the cancers or just specific types?
  			    selectInput(
  			      "proponentepf", "",
  			      c(" " = "", as.character(rouanet.tab$Proponente[
  			                               which(rouanet.tab$Tipo.de.Pessoa == "FÍSICA")])),
  			      selected = "all"),
  			    checkboxInput("todosproppf", "Buscar todos", FALSE)
			    ),
 # =====================Pessoa_Física__Projeto==================================	
			    conditionalPanel(
			      condition = "input.searchpf == 'Buscar Projeto'",
			    
			    #br(),
			    
			    strong(span("Projeto:")),
			    # show all the cancers or just specific types?
  			    selectInput(
  			      "projetopf", "",
  			      c(" " = "", as.character(rouanet.tab$Projeto[
  			        which(rouanet.tab$Tipo.de.Pessoa == "FÍSICA")])),
  			      selected = "all"),
  			    checkboxInput("todosprojpf", "Buscar todos", FALSE)
			    ),
			    
			    br(),
			    
			    #strong(span("Range de valores captados:")),
			    #uiOutput("pfvaluerUi"), 
			    br()
			  ),
# ============================Pessoa_Juridica======================================				  
			  conditionalPanel(
			    condition = "input.query == 'Pessoa Juridica'",
			    
			    br(),
			    strong(span("Range de valores captados:")),
			    uiOutput("pjvaluerUi"), 
			    br(),
			    
			    strong(span("Indicadores:")),
			    shinyjs::useShinyjs(),
			    wellPanel(
			      shiny::tags$head(shiny::tags$style(shiny::HTML(
			        "#textj {id: textj; font-size: 14px; height: 170px; overflow: auto; margin-left: -14px;}"
			      )),
			      shiny::tags$script(src = 'URL.js')),
			      
			      div(id = "textj")
			    ),
			    
			    strong(span("Executar busca:")),
			    #span("Tipo de consulta:"),
			    radioButtons(inputId = "searchpj",
			                 label = "",
			                 choices = c("Buscar Proponente" = "Buscar Proponente", 
			                             "Buscar Projeto" = "Buscar Projeto"),
			                 inline = FALSE),
# =====================Pessoa_Juridica__Proponente==================================	
  			    conditionalPanel(
  			      condition = "input.searchpj == 'Buscar Proponente'",
  			      
  			      strong(span("Proponente:")),
  			    # show all the cancers or just specific types?
      			    selectInput(
      			      "proponentepj", "",
      			      c(" " = "", as.character(rouanet.tab$Proponente[
      			        which(rouanet.tab$Tipo.de.Pessoa == "JURÍDICA")])),
      			      selected = "all"),
      			    checkboxInput("todosproppj", "Buscar todos", FALSE),
      			    br()
  			    ),
# =====================Pessoa_Juridica__Projeto=====================================
			      conditionalPanel(
			        condition = "input.searchpj == 'Buscar Projeto'",
			    
			        strong(span("Projeto:")),
    			    selectInput(
    			      "projetopj", "",
    			      c(" " = "", as.character(rouanet.tab$Projeto[
    			        which(rouanet.tab$Tipo.de.Pessoa == "JURÍDICA")])),
    			      selected = "all"),
    			    checkboxInput("todosprojpj", "Buscar todos", FALSE)
			      ),
# =================================================================================
    			  br()
			    
			      #uiOutput("pjvaluerUi"), 
          )
			  
			  # what variables to show
			  #uiOutput("variablesUi"),
			  
				# footer - where the data was obtaine
				#a(img(src = "us-cdc.png", alt = "US CDC"),
					#href = "http://wonder.cdc.gov/cancer.html",
					#target = "_blank")
			),
			
			# main panel has two tabs - one to show the data, one to plot it
			mainPanel(wellPanel(
				tabsetPanel(
					id = "resultsTab", type = "tabs",
					
					# tab showing the data in table format
					tabPanel(
						title = "Tabela", id = "tableTab",
						
						br(),
						#downloadButton("downloadData", "Download table"),
						br(), br(),
						tableOutput("dataTable")
					),
					
					# tab showing the data as plots
					tabPanel(
						title = "Plot data", id = "plotTab",
						#br(),
						#downloadButton("downloadPlot", "Save figure"),
						#br(),
						radioButtons(inputId = "tploty",
						             label = "",
						             choices = c("Bubble Chart" = "Bubble chart", 
						                         "Boxplot" = "Boxplot"),
						             inline = TRUE),
						br(),
						plotlyOutput("dataPlot",height = "400px")
					),
					tabPanel(
					  title = "Informações", id = "about",
					  br(),br(),
					  HTML('<ul>
                    <li>A lei Rouanet é uma Lei Federal de Incentivo à Cultura que institui politicas públicas 
                        para a cultura nacional, como o PRONAC - Programa Nacional de Apoio à 
                        Cultura. </li> </br> 
                    <li>Todos os dados utilizados podem ser obtidos 
                        <a href="http://www.bolsonaro.com.br/PLANILHA-LEI-ROUANET.xlsx"">
					              neste endereço</a>.</li> </br> 
					          <li>O objetivo da ferramenta é colaborar com a Lei nº 12.527 para que se tenha 
                        mais transparência na prestação de contas dos gastos públicos.</li> </br>
                    <li> Dúvidas, sujestões e críticas entre em contato.
					          
					       </ul>'
					       
					       ),
					  br(),br(),br(),
					  HTML(linkdin)
					)
				)
			))
		)
	))
)
