library(shiny)




server <-  function(input,output){
    #pred_system<-predict(model_rf,newdata=data.frame(input$oldpeak,input$slope),method="class",type="prob") 
    
    result<-eventReactive(input$Submit,
                          {
                            
                            ha<-matrix(ncol = 11,c(as.numeric(input$age),as.numeric(input$sex),as.numeric(input$cp),as.numeric(input$trestbps),as.numeric(input$chol),as.numeric(input$fbs),as.numeric(input$restecg),as.numeric(input$thalach),as.numeric(input$exang),as.numeric(input$oldpeak),as.numeric(input$slope)))
                            print(ha)
                            colnames(ha)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope")
                            k<-as.data.frame(ha)
                            pred_system<-predict(model_rf,k,method="class",type="prob")
                            pred_result<-as.data.frame(pred_system)
                            
                            #output$plot<-hist(pred_result)
                            #output$input_result<-renderTable({paste(pred_result)})
                            pred_result$positive
                          })
    output$text<-renderPrint({paste("The probrability of heart disease :",result())})
    #output$input_result<- renderDataTable({
    #col.names <- c("negative", "positive")
    #cbind(Outcome = result, as.data.frame(, ncol = 2))})
    
    
  }