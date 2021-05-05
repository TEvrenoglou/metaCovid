library(shiny)
library(tidyverse)
library(metafor)
library(stringr)
library(netmeta)
library(meta)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(waiter)
# library(parallel)
# options(mc.cores=ncores)



source("source/forest_Score7_D14_D28.R")
source("source/forest_mortality_D14_D28.R")
source("source/forest_mortality_D60.R")
source("source/forest_adverse_D14_D28.R")
source("source/forest_clinical_improvement_D14_D28.R")
source("source/forest_clinical_improvement_D60.R")
source("source/forest_negative_conversion_D7.R")
source("source/forest_SAE.R")
source("source/forest_Time_to_clinical_improvement.R")
source("source/forest_Time_to_death.R")
source("source/forest_Time_to_NEG_conversion.R")
source("source/forest_Time_to_score7.R")
source("source/select.R")


data=read.csv("rct__updated_database_30-04-2021.csv", na=c("*","NA"))
data=subset(data,data$Study_design=="RCT" & Trial_ID!=185 & Trial_ID!=280 & Trial_ID!=284 & Trial_ID!=315)

waiting_screen <- tagList(
  spin_google(),
  h3("Loading...")
) 



ui<- fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),
  
  tags$head(HTML(
    "<script>
      (function(i,s,o,g,r,a,m){
        i['GoogleAnalyticsObject']=r;i[r]=i[r]||
        function(){
          (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
          a=s.createElement(o), m=s.getElementsByTagName(o)[0];
          a.async=1;
          a.src=g;m.parentNode.insertBefore(a,m)
        })
      (window, document, 'script',
        '//www.google-analytics.com/analytics.js','ga');
      
        ga('create', '5173042', 'auto');
        ga('send', 'pageview');
      
      </script>"
  )),
  
  # tags$script(HTML(
  #   "$(document).one('shiny:idle', 
  #     function() {
  #       ga('set','userId', Shiny.user);
  #     }
  #    );"
  # )),
  
  
  use_waiter(),
  waiter_on_busy(html = waiting_screen, color = "white"),
  
  
  
  
  theme=shinytheme("journal"),
  
  br(),
  img(src='logometa.jpg'),
  h5("If you use the application, please cite us:\n Evrenoglou T, Chaimani A. metaCOVID - 
     An R-shiny application for automated meta-analyses of COVID-19 trials 
     (available from",tags$a(href="https://covid-nma.com/metacovid/", "https://covid-nma.com/metacovid/"),")"),
  # h4("Real time forest plots for COVID-19 trials"),
  #h4("The COVID-NMA initiative. A living mapping and living systematic review of Covid-19 trials"),
  #h5("Back to the main", a("website",href="https://covid-nma.com/")),
  br(),
  sidebarLayout( 
    sidebarPanel(
      fluidRow(
        column(6,wellPanel(
          h3("Analysis options"),
               selectInput("comparison",h4("Select a comparison"),
                           choices=list("",
                                        "Anti-virals" = list("Lopinavir-Ritonavir vs Standard Care"=1,"Umifenovir vs Standard Care"=2,"Lopinavir-Ritonavir vs Umifenovir"=4,
                                                             "Favipiravir vs Umifenovir"=5,"Favipiravir vs Chloroquine"=228,"Novaferon+Lopinavir-Ritonavir vs Lopinavir-Ritonavir"=11,"Novaferon+Lopinavir-Ritonavir vs Novaferon"=12,
                                                             "Novaferon vs Lopinavir-Ritonavir"=13,"Remdesivir vs Standard Care/Placebo"=14,"Remdesivir 5 days vs Remdesivir 10 days"=24,
                                                             "Interferon beta vs Standard Care"=26,"Favipiravir vs Standard Care"=43,"Sofosbuvir+Daclatasvir vs Standard Care"=58,
                                                             "Peginterferon Lambda vs Placebo"=160,"SNG001 vs Placebo"=163,"Ivermectin vs Lopinavir-Ritonavir"=199,
                                                             "Interferon alpha-2b vs Standard care"=297,"Sofosbuvir vs LPV/r+IFN beta-1a"=303
                                        ),
                                        
                                        "Other antimicrobials (antibiotics, antimalarials, antiparasitics)" = list(
                                          "Hydroxychloroquine vs Standard Care/Placebo"=3,"HCQ+AZM vs Hydroxychloroquine"=29,"Chloroquine vs Standard care"=19,
                                          "Chloroquine (nasal drops) vs Standard care"=261,"Azithromycin vs Standard care"=31,
                                          "HCQ+AZM vs Standard care/Placebo"=33,"Intravenous Immunoglobulin vs Standard Care"=35,"Ivermectin  vs Standard care/Placebo"=36,
                                          "Nitazoxanide vs Placebo"=118,"IVM+DX vs Standard care/Placebo"=134,"Hydroxychloroquine vs Azithromycin"=157,
                                          "Hydroxychloroquine+Zinc vs Hydroxychloroquine"=162,"IVM+DX vs Ivermectin"=168,"Itraconazole vs Standard care"=193,
                                          "Levamisole vs Placebo"=211
                                        ),
                                        
                                        "NSAIDs and Anti-inflammatories" = list("Colchicine vs Standard care/Placebo"=30,
                                                                                "Interferon kappa+TFF2 vs Standard care"=101
                                                                                
                                                                                
                                                                                
                                        ),
                                        
                                        "Kinase inhibitor" = list("CIGB-325 vs Standard care"=94,"Baricitinib+Remdesivir vs Placebo+Remdesivir"=176,
                                                                  "TD-0903 vs Placebo"=349
                                        ),
                                        
                                        "Corticosteroids" = list("Corticosteroids vs Standard Care/Placebo"=39,"Aprepitant+Dex vs Dexamethasone"=51,
                                                                 "Methylprednisolone vs Dexamethasone"=231,"Budesonide vs Standard care"=245
                                        ),
                                        
                                        "Monoclonal antibodies" = list("Tocilizumab vs Standard care/Placebo"=28, #"Tocilizumab vs Sarilumab"=178,
                                                                       #"Tocilizumab vs Favipiravir+Tocilizumab"=105,
                                                                       "Sarilumab vs Standard care"=179,
                                                                       "LY-CoV555 vs Placebo"=138,"Itolizumab vs Standard care"=167,"REGN-COV2 vs Placebo"=192,
                                                                       "Mavrilimumab vs Placebo"=299,"CT-P59 vs Placebo"=351,"Canakinumab vs Placebo"=357,
                                                                       "Otilimab vs Placebo"=359
                                        ),
                                        "Immunosupressant"=list("Leflunomide + Interferon alpha2a vs Interferon alpha2a"=93,
                                                                "Anakinra vs Standard Care"=223
                                                                
                                        ),
                                        
                                        "Convalescent plasma" = list("Convalescent plasma vs Standard care/Placebo"=22,
                                                                     "Early Convalescent plasma vs Late Convalescent plasma"=98
                                                                     #"Convalescent plasma vs Fresh frozen plasma"=133
                                        ),
                                        
                                        "Antithrombotic (antiplatelet, anticoagulant, thrombolytic drug)" = list(
                                          "Therapeutic anticoagulant vs Prophylactic anticoagulant"=97, 
                                          "Sulodexide vs Placebo"=175,"Intermediate-Dose prophylactic anticoagulation vs Standard-Dose prophylactic anticoagulation"=298
                                        ),
                                        
                                        "Other Advanced therapy medicinal products (ATMP)" = list(
                                          "hUC-MSC vs Standard Care/Placebo"=60,"INM005 vs Placebo"=252
                                        ),
                                        
                                        "Others" = list(
                                          "Vitamin C vs Placebo"=63,"Bromhexine vs Standard care"=84,"Vitamin D vs Standard care/Placebo"=164,"t-He/O2 vs Standard care"=165,
                                          "Prolectin-M vs Standard care"=169,"Fluvoxamine vs Placebo"=155,"Progesterone vs Standard care"=195,
                                          "Continue ARB/ACEI vs Discontinue ARB/ACEI"=212,"Compound 21 vs Placebo"=229,"Dutasteride vs Placebo"=159,
                                          "Losartan vs Amlodipine"=287,"Ammonium chloride vs Placebo"=361,"Atorvastatin vs Standard care"=368,
                                          "Atorvastatin vs Aspirin"=369,"Atorvastatin vs Atorvastatin+Aspirin"=370,"Aspirin vs Standard care"=371,
                                          "Aspirin vs Atorvastatin+Aspirin"=372,"Atorvastatin+Aspirin vs Standard care"=373
                                        )
                                        
                           ),
                           selected = ""
               ),
               
               
               selectInput("outcome",h4("Select an outcome"),
                           choices = c("",#"Clinical improvement D7",
                                       "Clinical improvement D28",
                                       "Clinical improvement D60",
                                       #"WHO Score 6 D7",
                                       #"WHO Score 6 D14-D28",
                                       #"WHO Score 7 D7",
                                       "WHO Score 7 D28",
                                       #"Mortality D7",
                                       "Mortality D28",
                                       "Mortality D60",
                                       #"Mortality D90",
                                       #"Negative conversion D3",
                                       "Negative conversion D7",
                                       #"Adverse events D7",
                                       "Adverse events",
                                       #"Serious adverse events D7",
                                       "Serious adverse events",
                                       "Time to clinincal improvement",
                                       #"Time to WHO score 6",
                                       "Time to WHO Score 7",
                                       "Time to death",
                                       "Time to Negative Conversion")),
               
               radioButtons("model", label = h4("Meta-analytical model"), 
                            choices = c("Random-effects", "Fixed-effects"),
                            selected = "Random-effects",
               ),
               
               
               radioButtons("sev",label = h4("Population severity"), 
                            choices = c("All populations","Mild populations","Mixed populations","Critical populations"),
                            selected = "All populations"
               ),
               
               radioButtons("subgroup",label = h4("Subgroup analysis by"), 
                            choices = c("Severity","Conflicts of interest","Funding","Location"),
                            selected = "Severity",
               ),
               
               h3("Sensitivity analyses"),
               
               radioButtons("checkRoB", label = h4("Exclude High RoB studies"), 
                            choices = c("No"="FALSE", "Yes"="TRUE"),
                            selected = "FALSE",
               ),
               
               radioButtons("denominator", label = h4("Missing outcome data"), 
                            choices = c("As non-events (randomized patients in the denominator)"="Randomized", 
                                        "Available case analysis"="Analyzed"),
                            selected = "Randomized"),
               

               
               
              
               
        )       
        ),
        
        column(6,wellPanel(
          
        h3("Presentation options"),
               sliderInput("rob",h4("Align Risk of bias"),value =0,min =-1,max=1,step = 0.01),
               sliderInput("dose",h4("Align Dose"),value =0,min =-0.5,max=0.5),
               radioButtons("hide_severity",label = h4("Hide population severity"), 
                            choices = c("No"="FALSE","Yes"="TRUE"),
                            selected = "FALSE"),
               
               radioButtons("hide_dose",label = h4("Hide treatment dose"), 
                            choices = c("No"="FALSE","Yes"="TRUE"),
                            selected = "FALSE")
               
               
            
               
        ),
        
        actionButton(inputId = "plot", label ="Create forest plot"),
        
        br(),
        hr(),
        
        downloadButton('downloadImage', 'Download Forest plot'),
        br(),
        hr(),
        actionButton(inputId = "reset", label = "Reset choices")
        
        )
      )
      #h6("Powered by:"),
      # img(src='image.png')
      
     
      
    ),        
    
   
    
    mainPanel(
      
      # strong("Please notice that the forest plot will be directly saved in your working directory."),
      imageOutput('forest'),
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
      hr(),
      h6("For comments/queries please contact us at: tevrenoglou@gmail.com"),
      br(),
      br(),
      br(),
      img(src='logo.jpg')
      
      #tags$style(type="text/css",
      #          ".shiny-output-error { visibility: hidden; }",
      #         ".shiny-output-error:before { visibility: hidden; }"
      #)
    )
    
  )
  
  
)

server<-function(input,output,session){
  
  
  options(shiny.sanitize.errors = FALSE)
  
  
  
  
  observe({
    x <- select(data=data,comp.num = input$comparison)
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session, "outcome",
                      label =paste("Select an outcome"),
                      choices = c("",x),
                      
    )
  })
  
  
  
  f=reactive({
    
    if(input$outcome=="Clinical improvement D28"){
      
      forest_Clinical_Improvement_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                                          model=input$model
                                          ,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose
      )
      
    }
    
    # 
    # else if(input$outcome=="Clinical improvement D7"){
    #   
    #   forest_Clinical_Improvement_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
    #                                  model=input$model)
    # }
    
    else if(input$outcome=="Clinical improvement D60"){
      
      forest_Clinical_Improvement_D60(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                                      model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      
    }
    
    # else if(input$outcome=="WHO Score 6 D7"){
    #   
    #   forest_Score6_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
    #                    model=input$model)
    # }
    # 
    # else if(input$outcome=="WHO Score 6 D14-D28"){
    #   
    #   forest_Score6_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
    #                         model=input$model)
    #   
    # }
    
    # else if(input$outcome=="WHO Score 7 D7"){
    #   
    #   forest_Score7_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
    #                    model=input$model)
    #   
    # }
    
    else if(input$outcome=="WHO Score 7 D28"){
      
      forest_Score7_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                            model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      
    }
    
    # else if(input$outcome=="Mortality D7"){
    #   
    #   forest_Mortality_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
    #                       model=input$model)
    #   
    # }
    
    else if(input$outcome=="Mortality D28"){
      
      forest_Mortality_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                               model=input$model,sub=input$subgroup
                               ,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose
      )
      
    }
    else if(input$outcome=="Mortality D60"){
      
      forest_Mortality_D60(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                           model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
    }
    
    # else if(input$outcome=="Mortality D90"){
    #   
    #   forest_Mortality_D90(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
    #                        model=input$model)
    #   
    # }
    
    # else if(input$outcome=="Negative conversion D3"){
    #   
    #   forest_NEG_D3(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
    #                 model=input$model)
    # }
    
    else if(input$outcome=="Negative conversion D7"){
      
      forest_NEG_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                    model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      
    }
    
    else if(input$outcome=="Adverse events"){
      
      forest_Adverse_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                             model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      
    }
    else if(input$outcome=="Serious adverse events"){
      
      
      forest_SAE_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                         model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      
      
    }
    
    else if(input$outcome=="Time to clinincal improvement"){
      
      forest_Time_to_clinical_improvement(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                                          model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      
    }
    
    # else if(input$outcome=="Time to WHO score 6"){
    #   
    #   forest_Time_to_Score6(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
    #                         model=input$model)
    # }
    
    else if(input$outcome=="Time to WHO score 7"){
      
      forest_Time_to_Score7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                            model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      
    }
    
    else if(input$outcome=="Time to death"){
      
      forest_Time_to_death(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                           model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      
    }
    
    else if(input$outcome=="Time to Negative Conversion"){
      
      forest_Time_to_neg_conv(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                              model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
    }
    
  })
  
  f1 <- eventReactive(input$plot, {
    
    f()
    
  })
  output$forest<-renderImage({
    
    validate(
      need(input$comparison, 'Please select a comparison'),
      need(input$outcome!= '', 'Please select an outcome')
    )
    
    
    
    outfile <- tempfile(fileext = '.png')
    png(outfile,width=2431,height=1346,res=200)
    
    f1()
    
    dev.off()
    
    
    list(src = outfile,
         contentType = 'image/png',
         width = 1050,
         height = 700,
         res=250,
         alt = "Please refresh your page")},deleteFile = FALSE)
  
  
  observeEvent(input$reset, {
    updateSliderInput(session, "rob", value = 0)
    updateSliderInput(session, "dose", value = 0)
    
    # updateSelectInput(session, "comparison", selected = "")
    
    updateSelectInput(session, "outcome", selected = "")
    
    updateRadioButtons(session,"model",selected = "Random-effects")
    
    updateRadioButtons(session,"hide_dose",selected = "FALSE")
    updateRadioButtons(session,"hide_severity",selected = "FALSE")
    updateRadioButtons(session,"denominator",selected = "Randomized")
    updateRadioButtons(session,"subgroup",selected = "Severity")
    
    updateRadioButtons(session,"checkRoB",selected = "FALSE")
    
    updateRadioButtons(session,"sev",selected = "All population")
    
  })
  
  
  
  
  output$downloadImage<- downloadHandler(
    #Specify The File Name 
    filename = function() {
      paste(input$outcome,input$comparison,"jpeg",sep= ".")},
    
    content = function(file){
      # open the format of file which needs to be downloaded ex: pdf, png etc. 
      # if (input$`Download Option`== "png"){
      #  png(file)
      #} else if (input$`Download Option`== "pdf"){
      # pdf(file)
      #} else {
      jpeg(file,width=2431,height=1346,res=200) 
      
      #
      #}
      if(input$outcome=="Clinical improvement D28"){
        
        forest_Clinical_Improvement_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                                            model=input$model
                                            ,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose
        )
        
      }
      
      
      ## else if(input$outcome=="Clinical improvement D7"){
      # #   
      # #   forest_Clinical_Improvement_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
      # #                                  model=input$model)
      # # }
      # 
      else if(input$outcome=="Clinical improvement D60"){
        
        forest_Clinical_Improvement_D60(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                                        model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
        
      }
      
      # # else if(input$outcome=="WHO Score 6 D7"){
      # #   
      # #   forest_Score6_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
      # #                    model=input$model)
      # # }
      # # 
      # # else if(input$outcome=="WHO Score 6 D14-D28"){
      # #   
      # #   forest_Score6_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
      # #                         model=input$model)
      # #   
      # # }
      # 
      # # else if(input$outcome=="WHO Score 7 D7"){
      # #   
      # #   forest_Score7_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
      # #                    model=input$model)
      # #   
      # # }
      # 
      else if(input$outcome=="WHO Score 7 D28"){
        
        forest_Score7_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                              model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
        
      }
      # 
      # # else if(input$outcome=="Mortality D7"){
      # #   
      # #   forest_Mortality_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
      # #                       model=input$model)
      # #   
      # # }
      # 
      else if(input$outcome=="Mortality D28"){
        #   
        forest_Mortality_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                                 model=input$model,sub=input$subgroup
                                 ,den=input$denominator,hide_dose=input$hide_dose,hide_severity=input$hide_severity
        )
        #   
      }
      else if(input$outcome=="Mortality D60"){
        
        forest_Mortality_D60(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                             model=input$model,sub=input$subgroup,den=input$denominator)
      }
      # 
      # # else if(input$outcome=="Mortality D90"){
      # #   
      # #   forest_Mortality_D90(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
      # #                        model=input$model)
      # #   
      # # }
      # # 
      # # else if(input$outcome=="Negative conversion D3"){
      # #   
      # #   forest_NEG_D3(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
      # #                 model=input$model)
      # # }
      # 
      else if(input$outcome=="Negative conversion D7"){
        #   
        forest_NEG_D7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                      model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
        #   
      }
      # 
      else if(input$outcome=="Adverse events"){
        #   
        forest_Adverse_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                               model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
        #   
      }
      
      else if(input$outcome=="Serious adverse events"){
        #   
        #   
        forest_SAE_D14_D28(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                           model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
        #   
        #   
      }
      # 
      else if(input$outcome=="Time to clinincal improvement"){
        #   
        forest_Time_to_clinical_improvement(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                                            model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
        #   
      }
      # 
      # # else if(input$outcome=="Time to WHO score 6"){
      # #   
      # #   forest_Time_to_Score6(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
      # #                         model=input$model)
      # # }
      # 
      else if(input$outcome=="Time to WHO score 7"){
        #   
        forest_Time_to_Score7(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                              model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
        #   
      }
      # 
      else if(input$outcome=="Time to death"){
        #   
        forest_Time_to_death(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                             model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
        #   
      }
      # 
      else if(input$outcome=="Time to Negative Conversion"){
        #   
        forest_Time_to_neg_conv(data = data,comp.num = input$comparison,sliderRob=input$rob,sliderDose=input$dose,high_RoB =input$checkRoB,keep_only=input$sev,
                                model=input$model,sub=input$subgroup,den=input$denominator,hide_severity=input$hide_severity,hide_dose=input$hide_dose)
      }
      
      dev.off()
    }
  )
  
  
  
}

shinyApp(ui=ui,server = server)


