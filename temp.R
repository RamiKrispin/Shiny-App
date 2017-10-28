#------------------------------ Loading the Packages -------------------------------------
set.seed(1234)
# Setting the required packages
pkgs <- c("shiny", "shinydashboard", "shinyWidgets",
          "ggplot2", "plotly", "rbokeh",
          "caret", "randomForest",
          "dplyr", "data.table",
          "DT", "knitr", "kableExtra"
)

for(pkg in pkgs){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
  lapply(pkg, FUN = function(X) {
    do.call("require", list(X)) 
  })
}
#------------------------------ Source the UI Function -------------------------------------
source("Shiny Modeling App UI.R")
#------------------------------ Loading Functions -------------------------------------
col_num <- function(df){
  if(ncol(df)%%3 !=0){
    x <- ncol(df)%/%3 +1
  } else {x <- ncol(df)%/%3}
  return (x)
}
#------------------------------ Confusion Matrix Function -------------------------------------

cm_fun <- function(train, test){
  cm_train <- cm_test <- data.frame(matrix(NA, ncol = dim(train)[1], nrow = dim(train)[2]))
  for(i in 1:(dim(train)[1])){
    cm_train[i,] <- train[i,]
    cm_test[i,] <- test[i,]
  }
  colnames(cm_train) <- colnames(train)
  colnames(cm_test) <- colnames(test)
  
  cm_train <- data.frame(rownames(train), cm_train)
  names(cm_train) <- c("Prediction", colnames(train))
  cm_test <- data.frame(rownames(test), cm_test)
  names(cm_test) <- c("Prediction", colnames(test))
  cm_df <- rbind(cm_train, cm_test)
  colnames(cm_df) <- c("Prediction", colnames(train))
  
  
  options(knitr.table.format = "html") 
  table_out <- knitr::kable(cm_df) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE) %>%
    group_rows("Training Set", 1, dim(train)[1]) %>%
    group_rows("Testing Set", (dim(train)[1] + 1), dim(train)[1] * 2) %>%
    add_header_above(c("", "Reference" = dim(train)[1] ))
  return(table_out)
}

#------------------------------ Accuracy Matrix Function -------------------------------------

accuracy_fun <- function(train, test){
  a_df <- data.frame(matrix(0, ncol = 2, nrow = 4))
  names(a_df) <- c("Training", "Testing")
  a_df[1,] <- c(train$overall[1], test$overall[1])
  a_df[2,] <- c(paste("[", round(train$overall[3], 2), ", ", round(train$overall[4], 2), "]", sep = "") , 
                paste("[", round(test$overall[3], 2),", ", round(test$overall[4], 2), "]", sep = ""))
  a_df[3,] <- c(train$overall[2], test$overall[2])
  a_df[4,] <- c(train$overall[2], test$overall[2])
} 
#------------------------------ Creating list of the installed packages datasets -------------------------------------
r_dataset <- NULL
pack_list <- installed.packages()
pack_list[,"Package"]

d <- data(package = pack_list[,"Package"])
installed_datasets <- as.list(paste(d$results[,"Package"],"-" ,d$results[,"Item"], sep = " "))
#------------------------------ Creating list of the avilable data frames/matrices -------------------------------------
df_list = c(names(which(sapply(.GlobalEnv, is.data.frame))),
            names(which(sapply(.GlobalEnv, is.matrix))),
            names(which(sapply(.GlobalEnv, is.data.table))))
#------------------------------ Server Function -------------------------------------
server <- function(input, output,session) {
#------------------------------ Set initial parameters -------------------------------------
 
  input_df <- reactiveValues(counter = 0,
                             data_name = NULL,
                             df_list = NULL)
  
#------------------------------ Data tab summary boxes -------------------------------------
  
  output$installed_datasets <- renderValueBox({
    valueBox(
      length(prev_table$r_datasets), "Installed R Datasets Available", icon = icon("folder-open"),
      color = "green"
    )
  })
  
  output$in_memory_df <- renderValueBox({
    valueBox(
      length(prev_table$data_frame_list), "In Memory Data Frame", icon = icon("superscript"),
      color = "light-blue"
    )
  })
  
  output$load_datasets <- renderValueBox({
    valueBox(
      ifelse(is.null(input_df$df_list), 0, length(input_df$df_list)), "Loaded Datasets", icon = icon("list"),
      color = "maroon"
    )
  })
  
  #------------------------------ Selecting the Data Input ------------------------------------- 
  prev_table <- reactiveValues(inputs_list = NULL, # Get the list of avilable dataset to load
                               data_frame_list = df_list, # List of avilable dataframes in memory
                               r_datasets = installed_datasets,
                               file_name = NULL, # If loading csv file, the name of the file
                               file_path = NULL, # If loading csv file, the path of the file
                               df_class = NULL, # Identify the class of the selected dataset 
                               df_name = NULL  # The name of the selected dataset
                               )  
  
  
  observeEvent(input$data_source,{
#------------------------------ Loading from data frame or package ------------------------------------- 
    prev_table$inputs_list <- switch(input$data_source,
                              "data_frame" = {# Case I - load in memory data frames
                                # If threre is no any data frame available in memory
                                if(length(prev_table$data_frame_list) == 0){
                                  showModal(modalDialog(
                                    title = "Warning - No Data Frame",
                                    HTML(paste("There is no any data frame avialable",
                                               "to load in the R Global Environment", 
                                               sep = "<br/>")
                                    ), size = "s"
                                  ))
                                  
                                  df_return_list <- NA
                                  # Set the condition for the load button
                                  output$load_flag <- reactive('0')
                                  outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                } else { # Otherwise return the list of available data frames in memory
                                  df_return_list <- prev_table$data_frame_list
                                  # Set the condition for the load button
                                  output$load_flag <- reactive('1')
                                  outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                }
                                df_return_list
                              },
                              "inst_pack" = {# Case II - load datasets from installed packages
                                # If threre is no any dataset available in the installed packages
                                if(length(prev_table$r_datasets) == 0){
                                  showModal(modalDialog(
                                    title = "Warning - No Datasets",
                                    HTML(paste("There is no any dataset avialable",
                                               "to load in the installed R packages", 
                                               sep = "<br/>")
                                    ), size = "s"
                                  ))
                                  dataset_list <- NA
                                  output$load_flag <- reactive('0')
                                  outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                } else {
                                  dataset_list <- prev_table$r_datasets
                                  output$load_flag <- reactive('1')
                                  outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                }
                                dataset_list
                                
                              }
                              
    )
  })  
  
  #------------------------------ Setting the csv file path------------------------------------- 
  observeEvent(input$file1,{
    output$load_flag <- reactive('0')
    inFile <- input$file1
    if(!is.null(inFile$datapath)){
      prev_table$file_name <- inFile$name
      prev_table$file_path <- inFile$datapath
      output$load_flag <- reactive('2')
      outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
    } else{
      output$load_flag <- reactive('0')
      outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
    } 
    
  })
  
  #------------------------------ Loading from data frame or package ------------------------------------- 
  # Feed the list of data frames and 
  #avialable datasets to the menue selection
  output$df_list <- renderUI({
    if(input$data_source == "data_frame" ) {
      selectInput("df_to_load", "Select Data Frame",
                  choices = prev_table$inputs_list )
    } else if(input$data_source == "inst_pack" ){
      selectInput("df_to_load", "Select Dataset",
                  choices = prev_table$inputs_list )
    }
  })
  
  # Load the data according to the user selection  
  df_view <- reactive({
    prev_table$df_class <- NULL
    if(input$data_source == "data_frame" & length(prev_table$data_frame_list) != 0){
      df_view <- NULL
      prev_table$df_name <- input$df_to_load
      df_view <- get(input$df_to_load)
      if(length(class(df_view)) > 1 & "data.frame" %in% class(df_view)){
        prev_table$df_class <- "data.frame"
      } else if(length(class(df_view)) > 1){
        prev_table$df_class <- class(df_view)[1]
      } else{
      prev_table$df_class <- class(df_view)
      }
    } else if(input$data_source == "inst_pack" & length(prev_table$r_datasets) != 0){
      df_view <- NULL
      
      dataset_name <- substr(input$df_to_load, 
                             regexpr("-", input$df_to_load) + 2,
                             nchar(trimws(input$df_to_load)))
      
      prev_table$df_name <- dataset_name
      df_view <- get(dataset_name)
     
      if(length(class(df_view)) > 1 & "data.frame" %in% class(df_view)){
        prev_table$df_class <- "data.frame"
      } else if(length(class(df_view)) > 1){
        prev_table$df_class <- class(df_view)[1]
      } else{
        prev_table$df_class <- class(df_view)
      }
      if(!is.data.frame(df_view) & !is.data.table(df_view) & !is.matrix(df_view)){
        if(is.ts(df_view)){
          df_view <- data.frame(Y=as.matrix(df_view), date=time(df_view))
        }
      }
      
    } else if(input$data_source == "import" & !is.null(prev_table$file_path)){
      df_view <- NULL
      
      prev_table$df_name <- substr(prev_table$file_name,1,regexpr(".", prev_table$file_name, fixed = T)-1)
      df_view <- read.csv(prev_table$file_path)
      prev_table$df_class <- class(df_view)
      
    } else {
      df_view <- NULL
    }
    
    return(df_view)
  })
  
  # View of the data
  output$view_table <- DT::renderDataTable(
    df_view(), server = FALSE, options = list(pageLength = 10,
                                              lengthMenu = c(10, 25, 50))
  )
  
  #------------------------------ Loading a selected dataset  ------------------------------------- 
  observeEvent(input$load, {
    name <- prev_table$df_name
    
    if(!name %in% input_df$data_name){
      input_df$data_name <- c(input_df$data_name, name)
      if(is.null(input_df$loaded_table)){
        input_df$loaded_table <- data.frame(name = name,
                                            class = prev_table$df_class,
                                            stringsAsFactors = FALSE)
      } else {
        temp <- data.frame(name = name,
                           class = prev_table$df_class,
                           stringsAsFactors = FALSE)
        input_df$loaded_table <- rbind(input_df$loaded_table,temp)
        temp <- NULL    
      }
      if(is.null(input_df$df_list)){
        input_df$df_list <- list(df_view())
        
      } else {
        input_df$df_list[[length(input_df$df_list) + 1]] <- df_view()
      }
      names(input_df$df_list)[length(input_df$df_list)] <- name
    } else{
      input_df$df_list[[which(names(input_df$df_list) == name)]] <- df_view()
    }
  })
  #------------------------------ Setting the condition for the "Remove" button  ------------------------------------- 
  observeEvent(input_df$loaded_table,{
    if(is.null(input_df$loaded_table)){
      output$loaded_table_flag <- reactive("0")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    } else {
      output$loaded_table_flag <- reactive("1")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    }
  })
  #------------------------------ Activate the "Remove" button ------------------------------------- 
  observeEvent(input$remove,{
   
    if(length(input_df$df_list)>1){
    input_df$df_list[[input$list_loaded_df_rows_selected]] <- NULL
    input_df$loaded_table <- input_df$loaded_table[-input$list_loaded_df_rows_selected,]
    input_df$data_name <- names(input_df$df_list)
    } else {
      input_df$df_list <- NULL
      input_df$loaded_table <- NULL
      input_df$data_name <- NULL
      output$loaded_table_flag <- reactive("0")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    }
    print(names(input_df$df_list))
    print(prev_table$df_name)
    
    
    
  })
  #------------------------------ Loaded dataset table ------------------------------------- 
  output$list_loaded_df <- DT::renderDataTable(
    data.frame(input_df$loaded_table), 
    selection = list(selected = 1, mode = 'single'), 
    options = list(pageLength = 10,
                   lengthMenu = c(10, 25, 50))
  )
}
#------------------------------ Call the App -------------------------------------
shinyApp(ui = ui, server = server)