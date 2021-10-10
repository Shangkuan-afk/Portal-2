library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(highcharter)
library(shinythemes)
library(SwimmeR)
library(rsconnect)
library(tidyverse)
library(plotly)
# library(imager)

# APP uploading
# rsconnect::setAccountInfo(name='liusha', token='D8E74B118C4171FC85EB2F602E004599', secret='P5Z5Bi6FQVzj+nJ/fUX5nU4a2M8xs85qVKgPg207')

# Import data
setwd("D:/study/R Learning/e-Portal/")
Df_lens_full <- read.csv("Data/Lens Data.csv")
Df_lens <- na.omit(Df_lens_full)
Df_lens_TCN <- Df_lens[,c("SPH","CYL","AX","n","m")]
Df_lens <- na.omit(Df_lens_full)
Df_lens_tab <- Df_lens_full[1:8]

MAT <- ordered(c("1.56ALD75mm", "1.56ABL", "1.60ZLM"))
FRM <- ordered(c("1", "2", "3"))

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# img_lens <- load.image("images/img_lens.jpg")

# Define UI

ui <- fluidPage(
  
  #Navbar structure for UI
  navbarPage("e-Portal", theme = shinytheme("journal"),
             tabPanel("Customer", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      
                      sidebarLayout(
                        sidebarPanel(
                          tabPanel("Insert Prescription", 
                                   
                                   #Heading
                                   h2("Right Eye"),
                                   #Right Eye Input UI 
                                   wellPanel(flowLayout(numericInput("RSphere", label = "RSph", value = "1.5",-10,10,0.25),
                                                        numericInput("RCylinder", label = "RCyl", value = "1",-10,10,0.25),
                                                        numericInput("RAxis", label = "RAxis", "90",1,180,1),
                                                        numericInput("RADD", label = "RAdd", "2",0,10,0.25)
                                   )),
                                   #Heading
                                   h2("Left Eye"),
                                   #Left Eye Input UI
                                   wellPanel(flowLayout(numericInput("LSphere", label = "LSph", value = "1.5",-10,10,0.25),
                                                        numericInput("LCylinder", label = "LCyl", value = "1",-10,10,0.25),
                                                        numericInput("LAxis", label = "LAxis", "90",1,180,1),
                                                        numericInput("LADD", label = "LAdd", "2",-10,10,0.25)
                                   )),
                                   
                                   #Additional Parameter
                                   h2("Additional Parameters"),
                                   wellPanel(flowLayout(selectInput("SMaterial",label="Material",c("Crown Glass","CR-39")),
                                                        numericInput("L", label = "Distance from Far Zone to the origin (mm)", value="10"),
                                                        numericInput("h", label = "Length of the corridor (mm)",value= "30")
                                   )),
                                   
                                   #Submit Button
                                   submitButton("Submit", icon=NULL, width = NULL)          
                          ),
                          helpText("Click the button after filled all gaps"),
                          hr(),
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv"))
                        ),
                        mainPanel(
                          fluidRow(
                            column(6,
                              wellPanel(
                                flowLayout(plotOutput("PRRU", width="200%")),
                                flowLayout(plotOutput("PRSP", width="200%")))
                            ),

                            column(6,
                              wellPanel(flowLayout(plotOutput("PLRU", width="200%")),
                                        flowLayout(plotOutput("PLSP", width="200%")
                                                   
                                        )
                              )
                            )

                          ),
                          fluidRow(
                            column(6,
                                   verbatimTextOutput("Basic_Curve"),
                                   verbatimTextOutput("Cross_Sagittal_Depth"),
                                   verbatimTextOutput("Toric_Sagittal_Depth"),
                                   verbatimTextOutput("Centre_Thickness")
                            ))
                        )
                      )),
             
             tabPanel("Customer", fluid = TRUE, icon = icon("globe-americas"),
                      tags$style(button_color_css),
                      
                      sidebarLayout(
                        sidebarPanel(
                          tabPanel("Insert Prescription", 
                                   
                                   #Heading
                                   h2("Right Eye"),
                                   #Right Eye Input UI 
                                   wellPanel(flowLayout(numericInput("RSphere", label = "RSph", value = "1.5",-10,10,0.25),
                                                        numericInput("RCylinder", label = "RCyl", value = "1",-10,10,0.25),
                                                        numericInput("RAxis", label = "RAxis", "90",1,180,1),
                                                        numericInput("RADD", label = "RAdd", "2",0,10,0.25)
                                   )),
                                   #Heading
                                   h2("Left Eye"),
                                   #Left Eye Input UI
                                   wellPanel(flowLayout(numericInput("LSphere", label = "LSph", value = "1.5",-10,10,0.25),
                                                        numericInput("LCylinder", label = "LCyl", value = "1",-10,10,0.25),
                                                        numericInput("LAxis", label = "LAxis", "90",1,180,1),
                                                        numericInput("LADD", label = "LAdd", "2",-10,10,0.25)
                                   )),
                                   
                                   #Additional Parameter
                                   h2("Additional Parameters"),
                                   wellPanel(flowLayout(selectInput("SMaterial",label="Material",c("Crown Glass","CR-39")),
                                                        numericInput("L", label = "Distance from Far Zone to the origin (mm)", value="10"),
                                                        numericInput("h", label = "Length of the corridor (mm)",value= "30")
                                   )),
                                   
                                   #Submit Button
                                   submitButton("Submit", icon=NULL, width = NULL)          
                          ),
                          helpText("Click the button after filled all gaps"),
                          hr(),
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv"))
                        ),
                        mainPanel(
                          fluidRow( column( width = 6,h4("Order Number Distribution", align = 'center'), highchartOutput('OrderNUM') ),
                                    column( width = 6,h4("Average Age Distribution", align = 'center'), highchartOutput('AgeDist') )
                          ),
                          fluidRow(
                            # Front Page - Lenses Data ID ----
                            
                            dataTableOutput("table_lens")
                            # tableOutput("contents")
                            
                          )
                        )
                      ))
             
             )
)

server <- function(input, output, session){
  
  values<-reactiveValues()  
  
  observe ({ 
    if(input$SMaterial == "Crown Glass") {n=as.numeric(1.523)}else{n=as.numeric(1.49)}
    
    L <-as.numeric(input$L)  
    
    h <-as.numeric(input$h)
    
    RSph<-as.numeric(input$RSphere)
    LSph<-as.numeric(input$LSphere)
    RAdd<-as.numeric(input$RADD)
    LAdd<-as.numeric(input$LADD)
    
    #Radius 
    RightRd <- as.numeric((((n-1)*1000)/(RSph)))
    RightRn <- as.numeric((((n-1)*1000)/(RSph+RAdd)))
    
    LeftRd <-as.numeric((((n-1)*1000)/(LSph)))
    LeftRn <-as.numeric((((n-1)*1000)/(LSph+LAdd)))
    
    #RIGHT EYE
    #calculate u
    Rcalculateu <- function(x,y){
      
      p = function(x,y) (x-(h/2)+L);
      
      g = function(x,y) ((1/2)*(p(x,y)+((y^2+((h^2)/4))/p(x,y))));
      
      u = function(x,y) (h/2)-L+g(x,y)-(sign(p(x,y)))*(sqrt(((g(x,y))^2)-((h^2)/4)));
      
      u(x,y) 
    }
    
    output$u<- renderText(calculateu(1,1))  #TEST OUTPUT
    #POWER LAW
    RPL <- function(x,y){
      Rcalculateu <- function(x,y){
        
        h<-as.numeric(input$h)
        
        p = function(x,y) (x-(h/2)+L);
        
        g = function(x,y) ((1/2)*(p(x,y)+((y^2+((h^2)/4))/p(x,y))));
        
        u = function(x,y) (h/2)-L+g(x,y)-(sign(p(x,y)))*(sqrt(((g(x,y))^2)-((h^2)/4)));
        
        u(x,y) 
      }
      
      u <- Rcalculateu(x,y)
      
      rd<-RightRd
      rn<-RightRn
      
      c1 <- 0; 
      c2 <- 0;
      c3 <- 0; 
      c4 <- 0;
      c5 <- 56/(h^5);
      c6 <- (-140)/(h^6);
      c7 <- 120/(h^7);
      c8 <- (-35)/(h^8);
      
      sum= ((c5*(u+L)^5)+(c6*(u+L)^6)+(c7*(u+L)^7)+(c8*(u+L)^8)); # negative 
      
      
      pl= ((1/rd)+(((1/rn)-(1/rd))*(sum)));
      pl
    }
    output$PL<-renderText(RPL(1,1)) #TEST OUTPUT
    
    #RADIUS OF CURVATURE
    RRU<- function(x,y){
      RPL <- function(x,y){
        Rcalculateu <- function(x,y){
          h<-as.numeric(input$h)
          
          p = function(x,y) (x-(h/2)+L);
          
          g = function(x,y) ((1/2)*(p(x,y)+((y^2+((h^2)/4))/p(x,y))));
          
          u = function(x,y) (h/2)-L+g(x,y)-(sign(p(x,y)))*(sqrt(((g(x,y))^2)-((h^2)/4)));
          
          u(x,y) 
        }
        
        u <- Rcalculateu(x,y)
        rd<-RightRd
        rn<-RightRn
        c1 <- 0; 
        c2 <- 0;
        c3 <- 0; 
        c4 <- 0;
        c5 <- 56/(h^5);
        c6 <- (-140)/(h^6);
        c7 <- 120/(h^7);
        c8 <- (-35)/(h^8);
        
        sum= ((c5*(u+L)^5)+(c6*(u+L)^6)+(c7*(u+L)^7)+(c8*(u+L)^8)); # negative 
        
        
        pl= ((1/rd)+(((1/rn)-(1/rd))*(sum)));
        pl
      }
      Ru <-RPL(x,y)^-1
      Ru
    }
    output$RU<- renderText(RU(1,1))
    
    
    
    
    #LEFT EYE
    #calculate u
    Lcalculateu <- function(x,y){
      
      p = function(x,y) (x-(h/2)+L);
      
      g = function(x,y) ((1/2)*(p(x,y)+((y^2+((h^2)/4))/p(x,y))));
      
      u = function(x,y) (h/2)-L+g(x,y)-(sign(p(x,y)))*(sqrt(((g(x,y))^2)-((h^2)/4)));
      
      u(x,y) 
    }
    
    output$u<- renderText(Lcalculateu(1,1))  #TEST OUTPUT
    
    #POWER LAW
    LPL <- function(x,y){
      Lcalculateu <- function(x,y){
        
        h<-as.numeric(input$h)
        
        p = function(x,y) (x-(h/2)+L);
        
        g = function(x,y) ((1/2)*(p(x,y)+((y^2+((h^2)/4))/p(x,y))));
        
        u = function(x,y) (h/2)-L+g(x,y)-(sign(p(x,y)))*(sqrt(((g(x,y))^2)-((h^2)/4)));
        
        u(x,y) 
      }
      
      u <- Lcalculateu(x,y)
      
      rd<-LeftRd
      rn<-LeftRn
      
      c1 <- 0; 
      c2 <- 0;
      c3 <- 0; 
      c4 <- 0;
      c5 <- 56/(h^5);
      c6 <- (-140)/(h^6);
      c7 <- 120/(h^7);
      c8 <- (-35)/(h^8);
      
      sum= ((c5*(u+L)^5)+(c6*(u+L)^6)+(c7*(u+L)^7)+(c8*(u+L)^8)); # negative 
      
      pl= ((1/rd)+(((1/rn)-(1/rd))*(sum)));
      pl
    }
    
    output$PL<-renderText(PL(1,1)) #TEST OUTPUT
    
    #RADIUS OF CURVATURE
    LRU<- function(x,y){
      LPL <- function(x,y){
        Lcalculateu <- function(x,y){
          h<-as.numeric(input$h)
          
          p = function(x,y) (x-(h/2)+L);
          
          g = function(x,y) ((1/2)*(p(x,y)+((y^2+((h^2)/4))/p(x,y))));
          
          u = function(x,y) (h/2)-L+g(x,y)-(sign(p(x,y)))*(sqrt(((g(x,y))^2)-((h^2)/4)));
          
          u(x,y) 
        }
        
        u <- Rcalculateu(x,y)
        rd<-LeftRd
        rn<-LeftRn
        c1 <- 0; 
        c2 <- 0;
        c3 <- 0; 
        c4 <- 0;
        c5 <- 56/(h^5);
        c6 <- (-140)/(h^6);
        c7 <- 120/(h^7);
        c8 <- (-35)/(h^8);
        
        sum= ((c5*(u+L)^5)+(c6*(u+L)^6)+(c7*(u+L)^7)+(c8*(u+L)^8)); # negative 
        
        pl= ((1/rd)+(((1/rn)-(1/rd))*(sum)));
        pl
      }
      Ru <-LPL(x,y)^-1
      Ru
    }    
    
    LSP<- function(x,y){
      power<-((n-1)*1000)/LRU(x,y)
      power
    }
    
    RSP<- function(x,y){
      power<-((n-1)*1000)/RRU(x,y)
      power
    }
    #PLOTS
    
    output$PRRU<-renderPlot({x <- y <- seq(-30,30)
    plot<-outer(x,y,RRU)
    contour(x,y,plot)
    title("Radius Contour Plot - Right Eye ", xlab = "X (mm)", ylab= "Y (mm)")
    lines(x=c(-L,-L+h),y=c(0,0))
    points(x=-L,y=0)
    points(x=-L+h,y=0) })
    
    output$PLSP<-renderPlot({x<-y<-seq(-30,30)
    plot<-outer(x,y,LSP)
    contour(x,y,plot)
    title("Spherical P Contour Plot (D) - Left Eye ", xlab = "X (mm)", ylab= "Y (mm)")
    lines(x=c(-L,-L+h),y=c(0,0))
    points(x=-L,y=0)
    points(x=-L+h,y=0)
    })
    
    output$PLRU<-renderPlot({x<-y<-seq(-30,30)
    plot<-outer(x,y,LRU)
    contour(x,y,plot)
    title("Radius Contour Plot (mm) - Left Eye ", xlab = "X (mm)", ylab= "Y (mm)")
    lines(x=c(-L,-L+h),y=c(0,0))
    points(x=-L,y=0)
    points(x=-L+h,y=0)
    })
    
    output$PRSP<-renderPlot({x<-y<-seq(-30,30)
    plot<-outer(x,y,RSP)
    contour(x,y,plot)
    title(main= "Spherical P Contour Plot (D) - Right Eye", xlab = "X (mm)", ylab= "Y (mm)")
    lines(x=c(-L,-L+h),y=c(0,0))
    points(x=-L,y=0)
    points(x=-L+h,y=0)
    })
  })
  
  #Lens Data Manipulating ----
  
  # Function to find Spherical Equivalent a = SPH, b = CYL
  SE_Function <- function(a,b){
    SE <- b/2 + a
  }
  # Function to calculate Vogels Rule Equivalent a = SPH, b = CYL
  Vogels_Rule <- function(a,b){
    VR <- SE_Function(a,b)/2 +6
  }
  
  # Function to find Toric a = SPH, b = CYL
  Toric_Function <- function(a,b){
    Toric <- a - round(Vogels_Rule(a, b))
  }
  
  # Function to find Cross a = SPH, b = CYL
  Cross_Function <- function(a,b){
    Cross <- a + b - round(Vogels_Rule(a, b))
  }
  
  # Function to Calculate Basic Curvature a = SPH, b = CYL, c = n
  Basic_Curvature <- function(a,b,c){
    BC <- (c - 1)/Vogels_Rule(a, b)
  }
  
  # Radius Function a = SPH, b = CYL
  Radius_Function <- function(a,b){
    RF <- (1 - a)/b
  }
  
  # Radius of Cross function a = SPH, b = CYL, c = n
  Cross_Radius_Function <- function(a,b,c){
    CRF <- Radius_Function(c, Cross_Function(a, b))
  }
  
  # Radius of Toric function a = SPH, b = CYL, c = n
  Toric_Radius_Function <- function(a,b,c){
    TRF <- Radius_Function(c, Toric_Function(a, b))
  }
  
  # Sagittal Depth function a = SPH, b = CYL
  Sagittal_Depth_Function <- function(a,b){
    SD <- a - sqrt(a^2 - (b/2)^2)
  }
  
  # Basic curve sagittal Depth function a = SPH, b = CYL, c = n, d = m
  Basic_Curve_Sagittal_Depth_Function <- function(a,b,c,d){
    BCSD <- Sagittal_Depth_Function(Basic_Curvature(a,b,c), d)
  }
  
  # Function to find Cross Sagittal Depth (Cross2)
  Cross_Sagittal_Depth_Function <- function(a,b,c,d){
    CSD <- Sagittal_Depth_Function(Cross_Radius_Function(a,b,c), d)
  }
  
  # Function to find Toric Sagittal Depth (Toric2)
  Toric_Sagittal_Depth_Function <- function(a,b,c,d){
    TSD <- Sagittal_Depth_Function(Toric_Radius_Function(a,b,c), d)
  }
  
  # Function to calculate Centre Thickness, a = SPH, b = CYL, c = n, d = ET (Edge Thickness)
  Centre_Thickness_Function <- function(a,b,c,d){
    Centre_Thickness <- Basic_Curvature(a,b,c) - Toric_Radius_Function(a,b,c) + d
  }
  
  # Reactive function ----
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  
  # LensPrescription <- eventReactive(
  #   input$EnterPrescri,{
  #   req(input$PrescriptionSPH)
  #   req(input$PrescriptionCYL)
  #   req(input$PrescriptionAX)
  #   req(input$PrescriptionM)
  #   req(input$PrescriptionN)
  #   
  #   Spherical_Equivalent <- SE_Function(input$PrescriptionSPH, input$PrescriptionCYL)
  #   print(Spherical_Equivalent)
  #   
  #   # LensPrescription <- data.frame(
  #   #   SPH = input$PrescriptionSPH,
  #   #   CYL = input$PrescriptionCYL,
  #   #   AX = input$PrescriptionAX,
  #   #   m = input$PrescriptionM,
  #   #   n = input$PrescriptionN,
  #   #   ET = c(0.004))
  #   # 
  #   # LensPrescription <- LensPrescription %>%
  #   #   # Find Spherical Equivalent
  #   #   mutate(SE = (LensPrescription$CYL/2) + LensPrescription$SPH)
  #   # LensPrescription <- LensPrescription %>%
  #   #   # Use Vogels Rule
  #   #   mutate(VRR = (LensPrescription$SE/2) + 6)
  #   # LensPrescription <- LensPrescription %>%
  #   #   # Find Toric and Cross
  #   #   mutate(Toric1 = LensPrescription$SPH - round(LensPrescription$VRR)) %>%
  #   #   mutate(Cross1 = LensPrescription$SPH + LensPrescription$CYL - round(LensPrescription$VRR)) %>%
  #   #   # Find Base Curve (BC)
  #   #   mutate(BC1 = (LensPrescription$n - 1)/ LensPrescription$VRR)
  #   # LensPrescription <- LensPrescription %>%
  #   #   # Use Lensmakers formula to find rad. Of curvature
  #   #   mutate(Cross2 = (1 - LensPrescription$n)/ LensPrescription$Cross1)%>%
  #   #   mutate(Toric2 = (1 - LensPrescription$n)/ LensPrescription$Toric1)%>%
  #   #   # Calculate Sagittal Depth
  #   #   mutate(BC2 = LensPrescription$BC1 - sqrt((LensPrescription$BC1)^2 - (LensPrescription$m/2)^2))
  #   # LensPrescription <- LensPrescription %>%
  #   #   mutate(Cross3 = LensPrescription$Cross2 - sqrt((LensPrescription$Cross2)^2 - (LensPrescription$m/2)^2)) %>%
  #   #   mutate(Toric3 = LensPrescription$Toric2 - sqrt((LensPrescription$Toric2)^2 - (LensPrescription$m/2)^2)) %>%
  #   #   mutate(Centre_Thickness = LensPrescription$BC1 - LensPrescription$Toric2 + LensPrescription$ET)
  #   # 
    # table render
    output$table_lens <- renderDataTable(
      datatable(Df_lens,
                rownames = FALSE,
                extensions = "Buttons",
                options = list(dom = "fBtp",
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               scrollX = TRUE),
                caption = "This table display the full data of current order"
    )
    )

  # })
  
  

  #FrontPage - Plot 01 - Lens Order Plot Data Input
  plot_lensOrder <- tapply(Df_lens_full$Year, list(Df_lens_full$Month, Df_lens_full$Year), length)
  plot_lensOrder
  plot_lensOrder <- as.data.frame(plot_lensOrder)
  names(plot_lensOrder)[1] <- 'First_Year'
  names(plot_lensOrder)[2] <- 'Second_Year'
  names(plot_lensOrder)[3] <- 'Third_Year'   # Should be modified to be more reactive, to encounter different type of data source
  plot_lensOrder[is.na(plot_lensOrder)] = 0
  plot_lensOrder



  # FrontPage - Plot 01 - Order Number Chart
  output$OrderNUM <- renderHighchart({
    highchart() %>%
      hc_exporting(enabled = TRUE) %>%
      hc_chart(type = 'line') %>%
      hc_series(list(name = '2019', data = plot_lensOrder$First_Year, color = "black") ,
                list(name = '2020', data = plot_lensOrder$Second_Year, color = "blue"),
                list(name = '2021', data = plot_lensOrder$Third_Year, color = "green")
      )%>%
      hc_xAxis(title = list(text = "Month"), categories = unique(plot_lensOrder$Month))%>%
      hc_yAxis(title = list(text = "Order Number"))%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMouseTracking = T
      ))%>%
      hc_tooltip(table = T,
                 sort = T,
                 pointFormat = paste0('<br><span style = "color: {point.color}">\u25CF</span>',
                                      '{series.name}: {point.y}'),
                 headerFormat = '<span style = "font-size: 13px">Month {point.key}</span>'
      )%>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )

  })

  # FrontPage - Plot 02 - Data Input
  plot_ageDist <- tapply(Df_lens_full$Age, list(Df_lens_full$Month, Df_lens_full$Year), mean)%>%
    as.data.frame()
  names(plot_ageDist) <- c('First_Year', 'Second_Year', 'Third_Year')
  plot_ageDist[is.na(plot_ageDist)] = 0
  plot_ageDist

  # FrontPage - Plot 02 - Age Distribution Chart
  output$AgeDist <- renderHighchart({
    highchart() %>%
      hc_exporting(enabled = TRUE) %>%
      hc_chart(type = 'line') %>%
      hc_series(list(name = '2019', data = plot_ageDist$First_Year, color = "black") ,
                list(name = '2020', data = plot_ageDist$Second_Year, color = "blue"),
                list(name = '2021', data = plot_ageDist$Third_Yea, color = "green")
      )%>%
      hc_xAxis(title = list(text = "Month"), categories = unique(plot_ageDist$Month))%>%
      hc_yAxis(title = list(text = "Average Age"),
               plotLines = list(
                 list(
                   color = "#ff0000",
                   width = 2,
                   value = 40
                 )
               ))%>%
      hc_tooltip(table = T,
                 sort = T,
                 pointFormat = paste0('<br><span style = "color: {point.color}">\u25CF</span>',
                                      '{series.name}: {point.y} years old'),
                 headerFormat = '<span style = "font-size: 13px">Month {point.key}</span>'
      )%>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = F),
        enableMouseTracking = T
      ))%>%
      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )

  })
  
}

shinyApp(ui = ui, server = server)
# # Data manipulating
# setwd("D:/study/R Learning/Test_App")
# Df_lens_full <- read.csv("Data/Lens Data.csv")
# Df_lens <- na.omit(Df_lens_full)
# Df_lens_full
# Df_lens_TCN <- Df_lens[,c("SPH","CYL","AX","n","m")]
# Df_lens_TCN
# 
# SE <- Df_lens_TCN$CYL/2 + Df_lens_TCN$SPH
# SE
