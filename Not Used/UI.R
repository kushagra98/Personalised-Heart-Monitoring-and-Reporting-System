library(shiny)

ui <- (fluidPage("Deepshi Amma ki Jai",
                  tabPanel("Classifier",
                           
                           # Application title
                           titlePanel(h1("Personal Heart Monitoring System")),
                           sidebarLayout(position="left",
                                         
                                         
                                         # Entry variables
                                         sidebarPanel(h3("Entry variables"),
                                                      numericInput("age","Enter the patient's age","",min = 0,max = 100),
                                                      
                                                      radioButtons("sex","Select gender",choices = c("male"= 1,"female"=0),""),
                                                      radioButtons("cp","Select the chest pain type",choices = c("typical angina Value"=1,"  atypical angina"=2," non-anginal pain"=3, "asymptomatic"=4
                                                      )),
                                                      numericInput("trestbps","Resting blood pressure(mmHg)"," "),
                                                      numericInput("chol","Serum cholestoral in mg/dl"," "),
                                                      radioButtons("fbs","fasting blood sugar > 120 mg/dl",choices = c("Yes"= 1,"No"=0),""),
                                                      radioButtons("restecg", "Resting electrocardiographic results",choices = c( "normal"="1",
                                                                                                                                  " ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV"="2", 
                                                                                                                                  "showing probable or definite left ventricular hypertrophy by Estes' criteria"="3"),""),
                                                      
                                                      
                                                      numericInput("thalach","maximum heart rate achieved",""),
                                                      radioButtons("exang" ,"exercise induced angina" ,choices = c("Yes"=1,"No" = 0),""),
                                                      numericInput("oldpeak"," ST depression induced by exercise relative to rest"," "),
                                                      radioButtons("slope" ,"the slope of the peak exercise ST segment " ,choices = c("1"=1,"2" = 2,"3"=3,"4"=4),""),
                                                      
                                                      
                                                      
                                                      actionButton("Submit","submit")
                                                      
                                                      
                                         ),
                                         #Display results
                                         
                                         mainPanel(
                                           tabsetPanel(
                                             
                                             tabPanel("Text", h3(tableOutput("text"))),
                                             tabPanel("Plot", plotOutput("plot")), 
                                             tabPanel("Table", tableOutput("input_result"))
                                           )
                                         ))))
)

