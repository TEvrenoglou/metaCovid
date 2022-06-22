########################## Building shiny dashboards ###########################
library(shinydashboard)
library(shiny)
library(tidyverse)
library(metafor)
library(stringr)
library(netmeta)
library(meta)
library(brglm)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(waiter)
library(DT)
library(shinyBS)



css <- HTML(
  "/* move logo to center */
    #logo {
        position: absolute;
        left: 50%;
        top: 50%;
        transform: translate(-50%, -50%);
    }
    /* remove hover effect */
    #logo > a:hover {
        background-color: transparent !important;
        color: transparent !important;
    }"
)



dbHeader <-   dashboardHeader(disable = F,titleWidth = 0,
  
  tags$li(class = "dropdown",
          id = "logo",
          tags$style(".main-header .logo {height: 80px;}"),
          tags$style(".main-header {max-height: 80px}"),
          tags$style(".navbar {min-height:1px !important}"),
          tags$style(".sidebar-toggle {height:40px; padding-top: 5px !important;}"),
          tags$a(tags$img(height = "80px",
                          src="logometa.png")
          ),
          tags$style(css)
  )
)




sidebar <-
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
    sidebarMenu(
      menuItem(
        "Home",
        tabName = "home",
        icon = icon("house-user")
      ),
      menuItem(
        "Covid-19 treatments",
        tabName = "treats",
        icon = icon("tablets")
      ),
      menuItem("Covid-19 vaccines", tabName = "vaccs", icon = icon("syringe")),
      # menuItem("About Covid-NMA", tabName = "about", icon = icon("lightbulb-on")),
      menuItem("How to cite us", tabName = "cite", icon = icon("book")),
      menuItem(
        "Contact",
        tabName = "contact",
        icon = icon("envelope-square")
      )
      
    ))



body <- dashboardBody(
  
  
  tags$head(
    tags$script("var array1 = ['1','3','14', '22',  '29',  '30',  '31', '33',  '36',  '43',  '58',  '63',  '85','86',
    '102','118','135', '138', '155', '159','160','165', '175' , '192', '211','245', '251','253','254', '277','287','289','290', 
    '291', '292', '293', '294','295', '324', '329', '350', '351', '371', '381', '396', '397', '416','423',
    '425', '428', '429','461','462','474','478','482' ,'493','495', '496','497', '512', '524', '525','526','746','777','780','786','787'];")
  
  ),
  
  tabItems(
    tabItem(
      tabName = "home",
      h1("Welcome to metaCOVID!"),
      br(),
      br(),
      p(h4(
      tags$div(
        "metaCOVID is an R-Shiny application that runs in parallel to the ",
        tags$a(href="https://covid-nma.com/", 
               "COVID-NMA"),
        "initiative. The aim of this application is to allow all the end-users of the ",
        tags$a(href="https://covid-nma.com/", 
               "COVID-NMA"),
        "platform to perform their own meta-analyses through a user-friendly environment. All data-analyses are based on the ",
        tags$a(href="https://covid-nma.com/", 
               "COVID-NMA"),"latest database with the most up to date data."
      )
      )
      ),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
  
   
      column(12,
             div(style="display: inline-block; width: 24.8%;",img(src="up.png",height=60, width=180)),
              div(style="display: inline-block; width: 24.8%;",img(src="nma.png", height=120, width=220)),
              div(style="display: inline-block; width: 24.8%;",img(src="https://www.inserm.fr/wp-content/uploads/2021/01/logo-inserm.svg", height=150, width=150)),
              div(style="display: inline-block; width: 24.8%;",img(src="CC.png", height=100, width=200))
      )
      
      
      
    ),
    
  tabItem(
    tabName = "treats",
    fluidRow(
      box(
        width = 2,
        title = "Select options",
        status = "warning",
        collapsible = T,
        solidHeader = T,
        selectInput(
          "comparison",
          h4("Select treatment comparison"),
          choices = order_comparisons(hospitalized_comparisons),
          selected = "",
          selectize = F,
          multiple = F
        ),
        selectInput(
          "outcome",
          h4("Select an outcome"),
          choices = c(
            "",
            "Clinical improvement D28",
            "Clinical improvement D60",
            "WHO Score 7 D28",
            "WHO Score 7 D60",
            "Mortality D28",
            "Mortality D60",
            "Hospitalization or death",
            "Negative conversion D7",
            "Adverse events",
            "Serious adverse events",
            "Time to clinical improvement",
            "Time to WHO score 7",
            "Time to death",
            "Time to Negative Conversion"
          ),
          selectize = F
        ),
        
        conditionalPanel(
          condition ="array1.indexOf(input.comparison) > -1",
          radioButtons(
            "patients",
            "Type of patients",
            choices = c("Hospitalized patients","Outpatients"),
            selected = "Hospitalized patients"
          )
        ),
        conditionalPanel(condition = "input.patients=='Hospitalized patients'",
        radioButtons(
          "sev",
          label = "Population of interest",
          choices = c(
            "All populations" = "All population",
            "Mild populations" = "Mild population"
            ,
            "Mixed populations" = "Mixed population",
            "Critical populations" =
              "Critical population"
          ),
          selected = "All population"
        )
        ),
        
        conditionalPanel(condition = "input.comparison",
        actionButton('info', 'View data table'),
        bsModal("info1", "Data Table", "info", size = "large",
                dataTableOutput("distTable"),
                tags$head(tags$style(HTML('

                                                  .modal-lg {
                                                  width: 1200px;
                                                  }
                                                  #abs_1 {background-color: white;;}
                                                  #clear{background-color: turquoise3;;}
                                                  #disease{background-color: turquoise3;;}
                                                  #bleach{background-color: turquoise3;;}
                                                  #regionSelect{background-color: turquoise3;;}
                                                  #yearSelect{background-color: turquoise3;;}
                                                  #speciesSelect{background-color: turquoise3;;}
                                                  ')))
             
        )
        )
        
        
        
      ),
      
      box(
        width = 2,
        height = 385,
        title = "Meta-analysis options",
        solidHeader = TRUE,
        status = "warning",
        collapsible = T,
        collapsed = F,
        radioButtons(
          "model",
          label = "Type of model",
          choices = c("Random-effects", "Common effect" =
                        "Fixed-effects"),
          selected = "Random-effects",
        ),
        conditionalPanel(
        condition ="input.model == 'Random-effects'", 
        
        radioButtons(
          "het",
          
          label = "Heterogeneity estimate method",
          
          choices = c("Restricted maximum likelihhod" ="REML",
                      "Maximum likelihood" ="ML",
                      "DerSimonian-Laird"="DL", 
                      "Sidik-Jonkman" ="SJ",
                      "Empirical Bayes" ="EB",
                      "Paule-Mandel" ="PM"
                        ),
          selected = "REML"
          
        )
        
        )

      ),
      
      
      
      box(
        width = 2,
        height = 205,
        title = "Subgroup analysis",
        solidHeader = TRUE,
        status = "warning",
        collapsible = T,
        collapsed = F,
        radioButtons(
          "subgroup",
          label = NULL,
          choices = c(
            "Severity",
            "Conflicts of interest",
            "Funding",
            "Location",
            "Type of Control" = "Separate Standard care/Placebo",
            "No subgroup analysis"
          ),
          selected = "Severity"
        )
      ),
      box(
        width = 3,
        height = 400,
        title = "Sensitivity analysis",
        solidHeader = TRUE,
        status = "warning",
        collapsible = T,
        collapsed = F,
        radioButtons(
          "checkRoB",
          label = "Risk of bias",
          choices = c(
            "All studies" = "No exclusion",
            "Exclude high RoB" = "High RoB",
            "Exclude high RoB and some concerns" = "High RoB/Some concerns"
          ),
          selected = "No exclusion"
        ),
        radioButtons(
          "pub",
          label = "Exclude preprints",
          choices = c("No" =
                        "FALSE", "Yes" = "TRUE"),
          selected = "FALSE"
        ),
        radioButtons(
          "denominator",
          label = "Missing outcome data",
          choices = c(
            "As non-events (randomized patients in the denominator)" = "Randomized",
            "Available case analysis" =
              "Analyzed"
          ),
          selected = "Randomized"
        ),
        conditionalPanel(
          condition = "input.comparison != '' && (input.outcome == 'Clinical improvement D28' || input.outcome == 'Clinical improvement D60'
          || input.outcome == 'WHO Score 7 D28' || input.outcome == 'Mortality D28' || input.outcome == 'Mortality D60'
          || input.outcome == 'Negative conversion D7' || input.outcome == 'Adverse events' || input.outcome == 'Serious adverse events'|| input.outcome == 'Hospitalized patients')",

          
          actionButton('models', 'Different meta-analysis models'),
          bsModal("models1", 
                  tags$div(
                  h2("Pooled results for odds ratio across different models"),
                  ),
                  "models", size = "large",
                  dataTableOutput("model_sensitivity"),
                  tags$head(tags$style(HTML('

                                                  .modal-lg {
                                                  width: 1200px;
                                                  }
                                                  #abs_1 {background-color: white;;}
                                                  #clear{background-color: turquoise3;;}
                                                  #disease{background-color: turquoise3;;}
                                                  #bleach{background-color: turquoise3;;}
                                                  #regionSelect{background-color: turquoise3;;}
                                                  #yearSelect{background-color: turquoise3;;}
                                                  #speciesSelect{background-color: turquoise3;;}
                                                  '))),
                  tags$div(
                    h6("Studies with zero events in one or all treatment arms are taken into account"),
                    
                    h6("Penalized likelihood, Mantel-Haenszel and Peto methods are more suitable for the analysis of rare events")
                  )
          )
        )
      ),
      
      box(
        width = 2,
        height = 320,
        title = "Presentation options",
        solidHeader = TRUE,
        status = "warning",
        collapsible = T,
        collapsed = F,
        radioButtons(
          "hide_dose",
          label = "Hide treatment dose",
          choices = c("No" =
                        "FALSE", "Yes" = "TRUE"),
          selected = "TRUE"
        ),
        
        conditionalPanel(
          condition = "input.hide_dose == 'FALSE'",
        sliderInput(
          "dose",
          "Align Dose",
          value = 0,
          min = -0.5,
          max = 0.5
        )),
        
        radioButtons(
          "hide_severity",
          label = "Hide population severity",
          choices = c("No" =
                        "FALSE", "Yes" = "TRUE"),
          selected = "TRUE"
        )
        
      ),
      
     
      downloadButton('downloadImage', 'Download Forest plot'),
      
      
      actionButton(inputId = "reset", label = "Reset all choices"),
  

      box(
       width = 11,
       height = 760,
       solidHeader = TRUE,
       status = "primary",
       collapsible = TRUE,
       collapsed = F,
       title = "Forest plot",
       imageOutput('forest')
       
     ),
     
       br(),
       br(),
       br(),
       box(
         width = 5,
         title = "References",
         solidHeader = T,
         status = "danger",
         h6("1. Viechtbauer W (2010). 'Conducting meta-analyses in R with the metafor package.' Journal of Statistical Software, 36(3), 1-48. https://doi.org/10.18637/jss.v036.i03." ),
           
        
         h6( "2. Balduzzi S, Ruecker G, Schwarzer G (2019). 'How to perform a meta-analysis with R: a practical tutorial.' Evidence-Based Mental Health, 153-160.")
          
        
     ),
       br(),
       br(),
     column(12,
            div(style="display: inline-block; width: 24.8%;",img(src="up.png",height=60, width=180)),
            div(style="display: inline-block; width: 24.8%;",img(src="nma.png", height=120, width=220)),
            div(style="display: inline-block; width: 24.8%;",img(src="https://www.inserm.fr/wp-content/uploads/2021/01/logo-inserm.svg", height=150, width=150)),
            div(style="display: inline-block; width: 24.8%;",img(src="CC.png", height=100, width=200))
     )

    )
    
 
    
  ),
  
  
  
  tabItem(tabName = "vaccs",
          fluidRow(
            box(
              width = 3,
              title = "Select options",
              status = "warning",
              collapsible = T,
              solidHeader = T,
              selectInput(
                "type_vacc",
                h4("Type of vaccine"),
                choices = list(
                  '',
                  "RNA based vaccine",
                  "Non replicating viral vector",
                  "Inactivated virus",
                  "Protein subunit",
                  "Virus-Like particle"
                ),
                selected = "",
                selectize = F
              ),
              
              selectInput(
                "outcome_vacc",
                h4("Outcome of interest"),
                choices = list(
                  '',
                  "Confirmed severe or critical disease due to Covid-19",
                  "Symptomatic Covid-19",
                  "SARS-CoV-2 infection",
                  "All-cause mortality",
                  "Serious adverse events",
                  "Any adverse events",
                  "Systemic adverse events",
                  "Local adverse events"
                ),
                selected = "",
                selectize = F
              ),
              
              conditionalPanel(condition = "input.type_vacc",
                               actionButton('info_vacc', 'View data table'),
                               bsModal("info_vacc1", "Data Table", "info_vacc", size = "large",
                                       dataTableOutput("distTable_vacc"),
                                       tags$head(tags$style(HTML('

                                                  .modal-lg {
                                                  width: 1200px;
                                                  }
                                                  #abs_1 {background-color: white;;}
                                                  #clear{background-color: turquoise3;;}
                                                  #disease{background-color: turquoise3;;}
                                                  #bleach{background-color: turquoise3;;}
                                                  #regionSelect{background-color: turquoise3;;}
                                                  #yearSelect{background-color: turquoise3;;}
                                                  #speciesSelect{background-color: turquoise3;;}
                                                  ')))
                                       
                               )
              )
              
              
            ),
            
            
            box(
              width=2,
              title="Analysis options",
              height =440,
              solidHeader=TRUE,
              status="warning",
              collapsible = T,
              collapsed = F,
              
              radioButtons(
                "model_vacc",
                label = "Type of model",
                choices = c("Random-effects", "Common effect" = "Fixed-effects"),
                selected = "Random-effects",
              ),
              conditionalPanel(
                condition ="input.model_vacc == 'Random-effects'", 
                
                radioButtons(
                  "het_vacc",
                  
                  label = "Heterogeneity estimate method",
                  
                  choices = c("Restricted maximum likelihhod" ="REML",
                              "Maximum likelihood" ="ML",
                              "DerSimonian-Laird"="DL", 
                              "Sidik-Jonkman" ="SJ",
                              "Empirical Bayes" ="EB",
                              "Paule-Mandel" ="PM"
                  ),
                  selected = "REML"
                  
                )
              ),
                radioButtons(
                  "diamond_vacc",
                  label = "Include pooled results",
                  choices=c("Yes"="TRUE","No"="FALSE"),
                  selected = "TRUE"
                )
                
                
              
          
          ),
          
          box(
              width=2,
              title="Sensitivity analysis",
              height =270,
              solidHeader=TRUE,
              status="warning",
              collapsible = T,
              collapsed = F,
              radioButtons(
                "checkRoB_vaccs",
                label = "Risk of bias",
                choices = c(
                  "All studies" = "No exclusion",
                  "Exclude high RoB" = "High RoB",
                  "Exclude high RoB and some concerns" = "High RoB/Some concerns"
                ),
                selected = "No exclusion"
              ),
              radioButtons(
                "pub_vaccs",
                label = "Exclude preprints",
                choices = c("No" =
                              "FALSE", "Yes" = "TRUE"),
                selected = "FALSE"
              )
              
            ),
            
            
            
            downloadButton('downloadImage_vaccs', 'Download Forest plot'),
          actionButton(inputId = "reset_vaccs", label = "Reset all choices"),
        
            
          box(
              width = 11,
              title = "Forest plot",
              height = 760,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              collapsed = F,
              imageOutput('forest_vacc')
            ),
          box(
              width = 5,
              title = "References",
              solidHeader = T,
              status = "danger",
              h6(
                "1. Viechtbauer W (2010). 'Conducting meta-analyses in R with the metafor package.' Journal of Statistical Software, 36(3), 1-48. https://doi.org/10.18637/jss.v036.i03."
              ),
              h6(
                "2. Balduzzi S, Ruecker G, Schwarzer G (2019). 'How to perform a meta-analysis with R: a practical tutorial.' Evidence-Based Mental Health, 153-160."
              )
            ),
            br(),
            br(),
          column(12,
                 div(style="display: inline-block; width: 24.8%;",img(src="up.png",height=60, width=180)),
                 div(style="display: inline-block; width: 24.8%;",img(src="nma.png", height=120, width=220)),
                 div(style="display: inline-block; width: 24.8%;",img(src="https://www.inserm.fr/wp-content/uploads/2021/01/logo-inserm.svg", height=150, width=150)),
                 div(style="display: inline-block; width: 24.8%;",img(src="CC.png", height=100, width=200))
          )
            
            
          )
   
          
          
          ),
  
  tabItem(tabName = "cite",
          fluidRow(
          column(width = 12,
            
            box(
              width = 10,
              title = "Cite metaCOVID",
              solidHeader = T,
              status = "danger",
              
              tags$div(
                "Evrenoglou, T., Boutron, I., Chaimani, A. metaCOVID: An R-Shiny application for living meta-analyses of COVID-19 trials. medRxiv (2021) ",
                tags$a(href="https://www.medrxiv.org/content/10.1101/2021.09.07.21263207v1", 
                       "doi:10.1101/2021.09.07.21263207.")
              )
            )
          )                    
  ),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),
  br(),

  column(12,
         div(style="display: inline-block; width: 24.8%;",img(src="up.png",height=60, width=180)),
         div(style="display: inline-block; width: 24.8%;",img(src="nma.png", height=120, width=220)),
         div(style="display: inline-block; width: 24.8%;",img(src="https://www.inserm.fr/wp-content/uploads/2021/01/logo-inserm.svg", height=150, width=150)),
         div(style="display: inline-block; width: 24.8%;",img(src="CC.png", height=100, width=200))
  )
  ),
  
  tabItem(tabName = "contact",
          
          h5("For comments/queries on the metaCOVID application please contact us at: tevrenoglou@gmail.com"),
          tags$div(
            "For any other question on the project please contact us here: ",
            tags$a(href="https://covid-nma.com/contact/", 
                   "https://covid-nma.com/contact/")
          ),
          
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),

          column(12,
                 div(style="display: inline-block; width: 24.8%;",img(src="up.png",height=60, width=180)),
                 div(style="display: inline-block; width: 24.8%;",img(src="nma.png", height=120, width=220)),
                 div(style="display: inline-block; width: 24.8%;",img(src="https://www.inserm.fr/wp-content/uploads/2021/01/logo-inserm.svg", height=150, width=150)),
                 div(style="display: inline-block; width: 24.8%;",img(src="CC.png", height=100, width=200))
          )          
          
  )
            
  )           
  
)

  
ui <- dashboardPage(header = dbHeader ,
                    sidebar = sidebar,
                    body = body,
                    skin = "black"
)


server <- function(input, output, session) {
  options(shiny.sanitize.errors = FALSE)
  
  observe({
    x <-  select_new(data = help_data(data=data,type=input$patients), comp.num = input$comparison,dat=comparisons)
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(
      session,
      "outcome",
      label = paste("Select an outcome"),
      choices = c("", x),
      
    )
  })
  
  
  observe({
    x1 <-  select_vaccines(data = data_vacc, type = input$type_vacc)

    # Can use character(0) to remove all choices
    if (is.null(x1))
      x1 <- character(0)

    updateSelectInput(
      session,
      "outcome_vacc",
      label = paste("Select an outcome"),
      choices = c("", x1),

    )
  })
  

  
  
  observeEvent(input$info, {
    output$distTable <- renderDT(create_table(data=help_data(data=data,type = input$patients),dat=comparisons,comp.num=input$comparison),
                                 filter = "top")
                                 
  })
  
  observeEvent(input$info_vacc, {
    output$distTable_vacc <- renderDT(create_table_vaccs(data=data_vacc,type =input$type_vacc),
                                 filter = "top")
  })
  
  
  observeEvent(input$models, {
    h5("Studies",)
    output$model_sensitivity <- renderDT(models(data=help_data(data=data,type = input$patients),dat=comparisons,comp.num=input$comparison,outcome=input$outcome),
                                 filter = "none")
  })

  
  # observeEvent(input$patients, {
  #   new_value=""
  #   updateSearchInput(session, "outcome", value = new_value)
  #   
  # })

  
  f = function(){   
    if (input$outcome == "Hospitalization or death") {
      forest_Hosp(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
      
    }
    
    if (input$outcome == "Clinical improvement D28") {
      forest_Clinical_Improvement_D14_D28(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
        
      )
      
    }
    
    
    else if (input$outcome == "Clinical improvement D60") {
      forest_Clinical_Improvement_D60(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
      
    }
    
    
    
    else if (input$outcome == "WHO Score 7 D28") {
      forest_Score7_D14_D28(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
      
    }
    
    else if (input$outcome == "WHO Score 7 D60") {
      forest_Score7_D60(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
        
      )
      
    }
    
    
    else if (input$outcome == "Mortality D28") {
      forest_Mortality_D14_D28(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
        
      )
      
    }
    else if (input$outcome == "Mortality D60") {
      forest_Mortality_D60(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
    }
    
    
    
    else if (input$outcome == "Negative conversion D7") {
      forest_NEG_D7(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
      
    }
    
    
    else if (input$outcome == "Adverse events") {
      forest_Adverse_D14_D28(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
      
    }
    else if (input$outcome == "Serious adverse events") {
      forest_SAE_D14_D28(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
        
      )
      
      
    }
    
    else if (input$outcome == "Time to clinical improvement") {
      forest_Time_to_clinical_improvement(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
      
    }
    
    
    else if (input$outcome == "Time to WHO Score 7") {
      forest_Time_to_Score7(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
      
    }
    
    else if (input$outcome == "Time to death") {
      forest_Time_to_death(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
      
    }
    
    else if (input$outcome == "Time to Negative Conversion") {
      forest_Time_to_neg_conv(
        data = help_data(data=data,type=input$patients),
        dat = comparisons,
        comp.num = input$comparison,
        sliderRob = input$rob,
        sliderDose = input$dose,
        high_RoB = input$checkRoB,
        keep_only = input$sev,
        model = input$model,
        sub = input$subgroup,
        den = input$denominator,
        hide_severity = input$hide_severity,
        hide_dose = input$hide_dose,
        Publication = input$pub,
        het_estimate=input$het
      )
    }
    
    
  }

  # f_treat <- eventReactive(input$outcome, {
  #   
  #   f()
  #   
  # })
  

  output$forest <- renderImage({
    validate(
      need(input$comparison, 'Please select a comparison'),
      need(input$outcome!= '', 'Please select an outcome')
    )
    outfile <- tempfile(fileext = '.png')
    png(outfile,
        width = 2431,
        #2431,
        height = 1346,
        res = 200)
    
    f()
    
    dev.off()
    
    
    list(
      src = outfile,
      contentType = 'image/png',
      width = 1050,
      height = 700,
      res = 250,
      alt = "Please first refresh your page by pressing the Reset button and then make a choice."
    )
  }, deleteFile = FALSE)
  
  
  observeEvent(input$reset, {
    updateSliderInput(session, "dose", value = 0)
    updateRadioButtons(session, "het", selected = "REML")
    updateSelectInput(session, "outcome", selected = "")
    updateRadioButtons(session, "model", selected = "Random-effects")
    updateRadioButtons(session, "hide_dose", selected = "TRUE")
    updateRadioButtons(session, "hide_severity", selected = "TRUE")
    updateRadioButtons(session, "denominator", selected = "Randomized")
    updateRadioButtons(session, "subgroup", selected = "Severity")
    updateRadioButtons(session, "pub", selected = "FALSE")
    updateRadioButtons(session, "denominator", selected = "Randomized")
    updateRadioButtons(session, "checkRoB", selected = "FALSE")
    updateRadioButtons(session, "sev", selected = "All population")
    updateRadioButtons(session, "checkRoB", selected = "No exclusion")
    
    
  })
  
  observeEvent(input$comparison, {
    updateRadioButtons(session,"patients",selected = "Hospitalized patients")
    
    
  })
  
  observeEvent(input$reset_vaccs, {

    updateRadioButtons(session, "het_vacc", selected = "REML")
    updateSelectInput(session, "outcome_vacc", selected = "")
    updateRadioButtons(session, "model_vacc", selected = "Random-effects")
    updateRadioButtons(session, "pub_vaccs", selected = "FALSE")
    updateRadioButtons(session, "diamond_vacc", selected = "TRUE")
    updateRadioButtons(session, "checkRoB_vaccs", selected = "No exclusion")
    
    
  })
  #

  
  
  
  
  output$downloadImage <- downloadHandler(
    #Specify The File Name
    filename = function() {
      ifelse(
        as.numeric(str_split(
          name_outcome(input$comparison, input$outcome,codes=codes_treats), "_"
        )[[1]][1]) < 10,
        paste("0", name_outcome(input$comparison, input$outcome,codes=codes_treats), sep =
                ""),
        name_outcome(input$comparison, input$outcome,codes=codes_treats)
      )
      
      
    },
    
    
    content = function(file) {
      jpeg(file,
           width = 2431,
           height = 1346,
           res = 200)
      
      f()
      
      
      dev.off()
    }
  )
  
  thedata=reactive({create_table(data=data,dat=comparisons,comp.num=input$comparison)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data",input$comparison, ".csv", sep = "_")
    },
    content = function(file) {
      
      write.csv(thedata()[input[["distTable_rows_all"]], ],file,row.names = F)
    }
  )
  
  
  
  f1 = function(){    
    if(input$outcome_vacc=="Any adverse events"){
      
      forest_AE_vaccs(data=data_vacc,type=input$type_vacc,subgroup = input$diamond_vacc,model=input$model_vacc,het_estimate=input$het_vacc,Publication = input$pub_vaccs,high_RoB = input$checkRoB_vaccs)
                   
    }
    
    if(input$outcome_vacc=="Systemic adverse events"){
      
      forest_systemic(data=data_vacc,type=input$type_vacc,subgroup = input$diamond_vacc,model=input$model_vacc,het_estimate=input$het_vacc,Publication = input$pub_vaccs,high_RoB = input$checkRoB_vaccs)
    }
    
    if(input$outcome_vacc== "Local adverse events"){
      
      forest_local(data=data_vacc,type=input$type_vacc,subgroup = input$diamond_vacc,model=input$model_vacc,het_estimate=input$het_vacc,Publication = input$pub_vaccs,high_RoB = input$checkRoB_vaccs)
    }
    
    if(input$outcome_vacc=="Serious adverse events"){
      forest_SAE(data=data_vacc,type=input$type_vacc,subgroup = input$diamond_vacc,model=input$model_vacc,het_estimate=input$het_vacc,Publication = input$pub_vaccs,high_RoB = input$checkRoB_vaccs)
      
    }
    
    if(input$outcome_vacc=="All-cause mortality"){
      forest_mortality(data=data_vacc,type=input$type_vacc,subgroup = input$diamond_vacc,model=input$model_vacc,het_estimate=input$het_vacc,Publication = input$pub_vaccs,high_RoB = input$checkRoB_vaccs)
      
    }
    
    if(input$outcome_vacc=="Symptomatic Covid-19"){
    forest_Conf_Sympt(data=data_vacc,type=input$type_vacc,subgroup = input$diamond_vacc,model=input$model_vacc,het_estimate=input$het_vacc,Publication = input$pub_vaccs,high_RoB = input$checkRoB_vaccs)
    }                 
                      
    if(input$outcome_vacc=="SARS-CoV-2 infection"){
      forest_confirmed(data=data_vacc,type=input$type_vacc,subgroup = input$diamond_vacc,model=input$model_vacc,het_estimate=input$het_vacc,Publication = input$pub_vaccs,high_RoB = input$checkRoB_vaccs)
    }
    
    
    if(input$outcome_vacc=="Confirmed severe or critical disease due to Covid-19"){
      forest_severe(data=data_vacc,type=input$type_vacc,subgroup = input$diamond_vacc,model=input$model_vacc,het_estimate=input$het_vacc,Publication = input$pub_vaccs,high_RoB = input$checkRoB_vaccs)
    }
      
   
  
       
    
    
  }
  
  
  
  output$forest_vacc <- renderImage({
    validate(
      need(input$type_vacc, 'Please select a type of vaccine'),
      need(input$outcome_vacc!= '', 'Please select an outcome')
    )
    
    outfile <- tempfile(fileext = '.png')
    png(outfile,
        width = 2431,
        height = 1346,
        res = 200)
    
    f1()
    
    dev.off()
    
    
    list(
      src = outfile,
      contentType = 'image/png',
      width = 1050,
      height = 700,
      res = 250,
      alt = "Please first refresh your page by pressing the Reset button and then make a choice."
    )
  }, deleteFile = FALSE)
  
  
  output$downloadImage_vaccs <- downloadHandler(
    #Specify The File Name
    filename = function() {
      ifelse(
        as.numeric(str_split(
          name_outcome_vaccs(outcome=input$outcome_vacc, type=input$type_vacc,codes=codes_vaccs), "_"
        )[[1]][1]) < 10,
        paste("0",  name_outcome_vaccs(outcome=input$outcome_vacc, type=input$type_vacc,codes=codes_vaccs), sep =
                ""),
        name_outcome_vaccs(outcome=input$outcome_vacc, type=input$type_vacc,codes=codes_vaccs)
      )
      
      
    },
    
    
    content = function(file) {
      jpeg(file,
           width = 2431,
           height = 1346,
           res = 200)
      
      f1()
      
      
      dev.off()
    }
  )
  
  
  
}

shinyApp(ui, server)
