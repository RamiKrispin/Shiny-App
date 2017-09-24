#------------------------------ Loading the Packages -------------------------------------
set.seed(1234)
#Setting the required packages
pkgs <- c("shiny", "shinydashboard", "shinyWidgets",
          "plotly",
          "dplyr", "data.table", "lubridate", "reshape2",
          "DT", "knitr", "kableExtra",
          "datasets"
)

for(pkg in pkgs){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg, dependencies = TRUE)
  }
  lapply(pkg, FUN = function(X) {
    do.call("require", list(X))
  })
}
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

packages.list <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)

d <- data(package = packages.list$Package)

#d <- data(package = pack_list[,"Package"])
dataset.df <- data.frame(package = d$results[,"Package"], dataset =  d$results[,"Item"] ,
                         space = regexpr(" ",d$results[,"Item"]), 
                         stringsAsFactors = FALSE )

dataset.df$dataset.fixed <- ifelse(dataset.df$space != -1, 
                                   substr(dataset.df$dataset, 1, (dataset.df$space - 1)), 
                                   dataset.df$dataset)


installed_datasets <- as.list(paste(dataset.df$package,"-" ,dataset.df$dataset.fixed, sep = " "))
#------------------------------ Creating list of the avilable data frames/matrices/ time series -------------------------------------
df_list <- c(names(which(sapply(.GlobalEnv, is.data.frame))),
             names(which(sapply(.GlobalEnv, is.matrix))),
             names(which(sapply(.GlobalEnv, is.data.table)))
)
ts_list <- c(names(which(sapply(.GlobalEnv, is.ts))))
#------------------------------ Server Function -------------------------------------
server <- function(input, output,session) {
  #------------------------------ input_df - set initial parameters -------------------------------------
  
  input_df <- reactiveValues(counter = 0,
                             data_name = NULL,
                             ts_obj = NULL,
                             mts_obj = NULL,
                             df_list = NULL,
                             df_class = NULL,
                             names_list = NULL,
                             df = NULL,
                             class = NULL,
                             var_summary = NULL)
  
  #------------------------------ Data tab 1 summary boxes -------------------------------------
  
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
  
  #------------------------------ Data tab 2 summary boxes -------------------------------------
  output$data_name <- renderValueBox({
    valueBox(
      input$select_df, input_df$class, icon = icon("folder-open"),
      color = "green"
    )
  })
  
  output$num_var <- renderValueBox({
    valueBox(
      ifelse(is.ts(input_df$df),frequency(input_df$df), ncol(input_df$df)),
      ifelse(is.ts(input_df$df),"Frequency", "Variables"), 
      icon = icon("superscript"),
      color = "light-blue"
    )
  })
  
  output$num_obs <- renderValueBox({
    valueBox(
      ifelse(is.ts(input_df$df),length(input_df$df),nrow(input_df$df)), "Observations", icon = icon("list"),
      color = "maroon"
    )
  })
  
  #------------------------------ Selecting the Data Input ------------------------------------- 
  prev_table <- reactiveValues(inputs_list = NULL, # Get the list of avilable dataset to load
                               data_frame_list = df_list, # List of avilable dataframes in memory
                               time_series_list = ts_list, # List of avilable time series in memory
                               r_datasets = installed_datasets, # List of avilable datasets within the installed packages
                               file_name = NULL, # If loading csv file, the name of the file
                               file_path = NULL, # If loading csv file, the path of the file
                               class = NULL, # Identify the class of the selected dataset 
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
                                     "time_series" = {# Case II - load in memory time series
                                       # If threre is no any data frame available in memory
                                       if(length(prev_table$time_series_list) == 0){
                                         showModal(modalDialog(
                                           title = "Warning - No Time Series Data",
                                           HTML(paste("There is no any time series data avialable",
                                                      "to load in the R Global Environment", 
                                                      sep = "<br/>")
                                           ), size = "s"
                                         ))
                                         
                                         df_return_list <- NA
                                         # Set the condition for the load button
                                         output$load_flag <- reactive('0')
                                         outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                       } else { # Otherwise return the list of available time series in memory
                                         df_return_list <- prev_table$time_series_list
                                         # Set the condition for the load button
                                         output$load_flag <- reactive('1')
                                         outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
                                       }
                                       df_return_list
                                     },
                                     "inst_pack" = {# Case III - load datasets from installed packages
                                       # If threre is no any dataset available in the installed packages
                                       if(length(prev_table$r_datasets) == 0){
                                         showModal(modalDialog(
                                           title = "Warning - No Datasets",
                                           HTML(paste("There is no any dataset avialable",
                                                      "to load from the installed R packages", 
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
    } else if(input$data_source == "time_series" ) {
      selectInput("df_to_load", "Select Data Frame",
                  choices = prev_table$inputs_list )
    } else if(input$data_source == "inst_pack" ){
      selectInput("df_to_load", "Select Dataset",
                  choices = prev_table$inputs_list )
    }
  })
  
  # Load the data according to the user selection  
  df_tbl_view <- reactive({
    prev_table$class <- NULL
    if(input$data_source == "data_frame" & length(prev_table$data_frame_list) != 0){
      df_view <- NULL
      prev_table$df_name <- input$df_to_load
      df_view <- get(input$df_to_load)
      if(length(class(df_view)) > 1 & "data.frame" %in% class(df_view)){
        prev_table$class <- "data.frame"
        df_view <- as.data.frame(df_view)
      } else if(length(class(df_view)) > 1){
        prev_table$class <- class(df_view)[1]
        df_view <- as.data.frame(df_view)
      } else{
        prev_table$class <- class(df_view)
        df_view <- as.data.frame(df_view)
      }
    } else if(input$data_source == "time_series" & length(prev_table$time_series_list) != 0){
      df_view <- NULL
      prev_table$df_name <- input$df_to_load
      input_df$ts_obj <- get(input$df_to_load)
      df_view <- get(input$df_to_load)
      if(is.mts(df_view)){
        df_view <- data.frame(date=time(df_view), as.matrix(df_view))
      } else if(is.ts(df_view)){
        df_view <- data.frame(date=time(df_view), as.matrix(df_view))
        names(df_view) <- c("date", prev_table$df_name)
      }
      if(length(class(input_df$ts_obj)) > 1 & "ts" %in% class(input_df$ts_obj)){
        prev_table$class <- "ts"
      } else if(length(class(input_df$ts_obj)) > 1){
        prev_table$class <- class(input_df$ts_obj)[1]
      } else{
        prev_table$class <- class(input_df$ts_obj)
      }
      # Loading from installed package
    } else if(input$data_source == "inst_pack" & length(prev_table$r_datasets) != 0){
      df_view <- NULL
      dataset_name <- NULL
      dataset_name <- substr(input$df_to_load, 
                             regexpr("-", input$df_to_load) + 2,
                             nchar(trimws(input$df_to_load)))
      package_name <- substr(input$df_to_load, 
                             1, (regexpr("-", input$df_to_load) - 2)
      )
      if(!paste("package:", package_name, sep = "") %in% search()){
        p <- NULL
        p <- as.list(package_name)
        do.call("require", p)
      }
      # Loading the selected dataset
      prev_table$df_name <- dataset_name
      if(!is.na(dataset_name)){
        if(dataset_name != "NA"){
          df_view <- try(get(dataset_name), silent = TRUE)
          if(class(df_view) == "try-error" & !is.na(dataset_name)){
            
            showModal(modalDialog(
              title = "Warning - Cannot Load the Dataset",
              HTML(paste("Cannot Load the Dataset:",
                         "- The package index name is not match the package name",
                         "- or the dataset format cannot be loaded",
                         sep = "<br/>")
              ), size = "s"
            ))
            output$load_flag <- reactive('0')
            outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
          }
        }
      }
      if(class(df_view) != "try-error"){
        output$load_flag <- reactive('2')
        outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
        
        if(is.mts(df_view)){
          input_df$mts_obj <- df_view
          df_view <- data.frame(date=time(df_view), as.matrix(df_view))
          prev_table$class <- "mts"
        } else if(is.ts(df_view)){
          input_df$ts_obj <- df_view
          df_view <- data.frame(date=time(df_view), as.matrix(df_view))
          names(df_view) <- c("date", prev_table$df_name)
          prev_table$class <- "ts"
        } else if(any(class(df_view) %in% c("data.frame","matrix", "data.table", "table"))){
          if(length(class(df_view)) > 1 & "data.frame" %in% class(df_view)){
            prev_table$class <- "data.frame"
            df_view <- as.data.frame(df_view)
          } else if(length(class(df_view)) > 1){
            prev_table$class <- class(df_view)[1]
            df_view <- as.data.frame(df_view)
          } else{
            prev_table$class <- class(df_view)
            df_view <- as.data.frame(df_view)
          }
          
        } 
        #   else {
        #   df_view <- NULL
        #   
        #   output$package_flag <- reactive('0')
        #   outputOptions(output, "package_flag", suspendWhenHidden = FALSE)
        #   showModal(modalDialog(
        #     title = "Warning - Invalid Format",
        #     HTML(paste("The dataset format is invalid",
        #                "the current available formats:",
        #                "- Data frame",
        #                "- Data table",
        #                "- Matrix",
        #                "- Time series",
        #                sep = "<br/>")
        #     ), size = "s"
        #   ))
        #   
        # }
      } 
    } else if(input$data_source == "import" & !is.null(prev_table$file_path)){
      df_view <- NULL
      prev_table$class <- NULL
      prev_table$df_name <- substr(prev_table$file_name,1,regexpr(".", prev_table$file_name, fixed = T)-1)
      df_view <- read.csv(prev_table$file_path, stringsAsFactors = FALSE)
      prev_table$class <- class(df_view)
      
    } else {
      df_view <- NULL
    }
    return(df_view)
  })
  
  # View of the data
  output$view_table <- DT::renderDataTable(
    df_tbl_view(), 
    server = FALSE, 
    rownames = FALSE,
    options = list(pageLength = 10,
                   lengthMenu = c(10, 25, 50))
  )
  
  #------------------------------ Loading a selected dataset  ------------------------------------- 
  observeEvent(input$load, {
    name <- prev_table$df_name
    type <- NULL
    type <- ifelse(prev_table$class == "data.frame", "Data Frame",
                   ifelse(prev_table$class == "ts", "Time Series",
                          ifelse(prev_table$class == "mts", "Multiple Time Series",
                                 ifelse(prev_table$class == "matrix", "Matrix", 
                                        prev_table$class ))))
    
    
    
    if(!name %in% input_df$data_name){
      input_df$data_name <- c(input_df$data_name, name)
      if(is.null(input_df$loaded_table)){
        input_df$loaded_table <- data.frame(name = name,
                                            var = ncol(df_tbl_view()),
                                            row = nrow(df_tbl_view()),
                                            class = type,
                                            stringsAsFactors = FALSE)
        
      } else {
        temp <- data.frame(name = name,
                           var = ncol(df_tbl_view()),
                           row = nrow(df_tbl_view()),
                           class = type,
                           stringsAsFactors = FALSE)
        input_df$loaded_table <- rbind(input_df$loaded_table,temp)
        
        temp <- NULL    
      }
      if(is.null(input_df$df_list)){
        if(prev_table$class != "ts"){
          input_df$df_list <- list(df_tbl_view())
        } else {
          input_df$df_list <- list(input_df$ts_obj)
        }
        input_df$df_class <- list(type)
        
      } else {
        if(prev_table$class != "ts"){
          input_df$df_list[[length(input_df$df_list) + 1]] <- df_tbl_view()
        } else {
          input_df$df_list[[length(input_df$df_list) + 1]] <- input_df$ts_obj
        }
        input_df$df_class[[length(input_df$df_list)]] <- type
      }
      names(input_df$df_list)[length(input_df$df_list)] <- name
      input_df$names_list <- names(input_df$df_list)
    } else{
      if(prev_table$class != "ts"){
        input_df$df_list[[which(names(input_df$df_list) == name)]] <- df_tbl_view()
      } else {
        input_df$df_list[[which(names(input_df$df_list) == name)]] <- input_df$ts_obj
      }
      input_df$df_class[[which(names(input_df$df_list) == name)]] <- type
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
      input_df$df_class[[input$list_loaded_df_rows_selected]] <- NULL
      input_df$loaded_table <- input_df$loaded_table[-input$list_loaded_df_rows_selected,]
      input_df$data_name <- names(input_df$df_list)
      input_df$names_list  <- input_df$data_name
    } else {
      input_df$df_list <- NULL
      input_df$loaded_table <- NULL
      input_df$data_name <- NULL
      input_df$names_list <- NULL
      input_df$df_class <- NULL
      input_df$names_list <- "NA"
      output$loaded_table_flag <- reactive("0")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    }
  })
  #------------------------------ Loaded dataset table ------------------------------------- 
  output$list_loaded_df <- DT::renderDataTable(
    data.frame(input_df$loaded_table), 
    colnames = c("Dataset Name", "Num. of Variables", "Num. of Obs", "Data Type"),
    selection = list(selected = 1, mode = 'single'), 
    options = list(pageLength = 10,
                   lengthMenu = c(10, 25, 50))
  )
  #------------------------------ DATA TAB 2 -------------------------------------   
  
  observeEvent({
    input_df$names_list
  },{
    output$loaded_ds_list  <- renderUI({
      selectInput("select_df", "Select Dataset",
                  choices = input_df$names_list
      )
    })
    
  })
  
  observeEvent(input$select_df, {
    if(!is.null(input$select_df)){
      input_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df)]]
      )
      input_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df)]]
      output$data_tab2_table <- DT::renderDataTable(
        data.frame(input_df$df),selection = list(selected = 1, mode = 'single'), 
        options = list(pageLength = 10,
                       lengthMenu = c(10, 25, 50))
      )
      
    } else{
      input_df$df <- NULL
      input_df$class <- NULL
      output$data_tab2_table <- NULL
    }
  })
  #------------------------------ Data tab 2 - Data Prep -------------------------------------    
  #------------------------------ Data tab 2 - Creating Variables Table ------------------------------------- 
  observeEvent({input$data_option
    input_df$df
    input$select_df
  }, {
    if(!is.ts(input_df$df)){            
      if(input$data_option == "var_attr" &
         !is.null(input_df$df) &
         !is.null(input_df$loaded_table)
      ){
        var.names <- names(input_df$df)
        var.class <- NULL
        for(i in 1:ncol(input_df$df)){
          if(length(class(input_df$df[,i])) > 1){
            if("factor" %in% class(input_df$df[,i])){
              var.class <- c(var.class, "factor")
            } else {
              var.class <- c(var.class, "NA")
            }
          } else {
            var.class <- c(var.class, class(input_df$df[,i])[1])
          }
        }
        input_df$var_summary <- data.frame(var.names, var.class, stringsAsFactors = FALSE)
        names(input_df$var_summary) <- c("Name", "Class")
        
        output$data_tab2_var <- DT::renderDataTable(
          input_df$var_summary,
          server = FALSE, rownames = FALSE,
          selection = list(selected = 1, mode = 'single'), 
          options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 10, dom = 'p')
        )
        
      } 
    } else {
      output$data_tab2_ts <- renderPlotly({
        
        
        if(!input$ts_plot_log){
          plot_ly( x = time(input_df$df), y = input_df$df, type  = "scatter", mode = input$ts_prep_mode)
        } else if(input$ts_plot_log){
          plot_ly( x = time(input_df$df), 
                   y = log(input_df$df, base = exp(1)), type  = "scatter", mode = input$ts_prep_mode) %>%
            layout(title = "Log Transformation")
        }
        
      })
    }
  })
  
  output$class_df_flag <- reactive({
    ifelse(is.ts(input_df$df), TRUE, FALSE)
  })
  outputOptions(output, "class_df_flag", suspendWhenHidden = FALSE)    
  #------------------------------ Data tab 2 - Creating Variable Summary -------------------------------------  
  observeEvent({input$data_tab2_var_rows_selected
    input$select_df
    input_df$df},{
      if(!is.ts(input_df$df)){
        r1 <- input$data_tab2_var_rows_selected
        if(is.numeric(input_df$df[, r1]) | is.integer(input_df$df[, r1])){
          var.mean <- mean(input_df$df[, r1], na.rm = TRUE)
          var.min  <- min(input_df$df[, r1], na.rm = TRUE)
          var.max  <- max(input_df$df[, r1], na.rm = TRUE)
          var.median <- median(input_df$df[, r1], na.rm = TRUE)
          var.sd <- sd(input_df$df[, r1])
          summary.vec <- c(var.mean, var.min, var.max, var.median, var.sd)
          var_s <- data.frame(summary.vec)
          names(var_s) <- names(input_df$df)[r1]
          row.names(var_s) <- c("Mean", "Min", "Max", "Median", "Standard Deviation")
          p <- plot_ly(y = ~ input_df$df[, r1], type = "box", name = names(input_df$df)[r1],
                       boxpoints = "all", jitter = 0.3,
                       pointpos = -1.8)%>%
            layout(yaxis = list(title = "Range"))
        } else if(is.factor(input_df$df[, r1])){
          var.n.levels <- length(levels(input_df$df[, r1]))
          var.levels <- NULL
          for(i in 1:var.n.levels){var.levels <- c(var.levels,levels(input_df$df[, r1])[i])}
          var_s <- c(var.n.levels)
          var_s <- data.frame(var_s)
          row.names(var_s) <- c("Number of Levels")
          names(var_s) <- names(input_df$df)[r1]
          factor.df <- group_by(input_df$df, get(names(input_df$df)[r1])) %>%
            summarise(count = n())
          names(factor.df) <- c(names(names(input_df$df)[r1]), "Count")
          p <- plot_ly(data = factor.df, name = "Levels",
                       x =  ~ get(names(factor.df)[1]),
                       y =  ~ get(names(factor.df)[2]), 
                       type = "bar") %>%
            layout(yaxis = list(title = "Count"),
                   xaxis = list(title = "Levels"))
        } else if(is.Date(input_df$df[, r1])){
          var_s <- NULL
          var_s <- data.frame(c(as.character(min(input_df$df[, r1])), 
                                as.character(max(input_df$df[, r1]))), row.names = c("Start/Min Date", "End/Max Date"))
          names(var_s) <- names(input_df$df)[r1]
          p <- NULL
        }
        
        # render the data summary into table
        output$data_tab2_var_summary <- renderTable(var_s, rownames = TRUE)
        output$data_tab2_summary_plot <- renderPlotly(p)
      } else {
        ts_table <- data.frame(c(paste(start(input_df$df), collapse = "-"),
                                 paste(end(input_df$df), collapse = "-"),
                                 min(input_df$df, na.rm = TRUE),
                                 max(input_df$df, na.rm = TRUE),
                                 round(sd(input_df$df, na.rm = TRUE),2)),
                               row.names = c("Start Date",
                                             "End Date", "Min Value",
                                             "Max Value","Standard Deviation"))
        names(ts_table) <- input$select_df
        output$ts_table <- renderTable(ts_table, rownames = TRUE)
        
      }
      
    })
  #------------------------------ Data tab 2 - Midifing Variables Attributes -------------------------------------  
  observeEvent(input$var_modify,{
    if(!is.ts(input_df$df)){
      r2 <- input$data_tab2_var_rows_selected
      input_df$df[,r2] <- switch(input$class_selection,
                                 "numeric" = as.numeric(input_df$df[,r2]),
                                 "factor" = as.factor(input_df$df[,r2]),
                                 "character" = as.character(input_df$df[,r2]),
                                 "date" = {eval(parse(text = 
                                                        paste("lubridate::",
                                                              input$date_format,
                                                              "('", 
                                                              as.character(input_df$df[,input$data_tab2_var_rows_selected]),
                                                              "')",
                                                              sep = "")))
                                 }
      )
      input_df$df_list[[which(names(input_df$df_list) == input$select_df)]] <- input_df$df
    }
  })
  
  observeEvent({input$date_format
    input$data_tab2_var_rows_selected
    input$class_selection
    input$select_df
  },{
    if(!is.ts(input_df$df)){
      new.date <- input_df$df[1,input$data_tab2_var_rows_selected]
      new.date <- as.character(new.date)
      output$date_prev <-  renderPrint(eval(parse(text = 
                                                    paste("lubridate::",
                                                          input$date_format,
                                                          "('", 
                                                          new.date[1],
                                                          "')",
                                                          sep = "")))
      )
    }
  })
  
  
  
  
  observeEvent(input$tabs,{
    if(input$tabs != "data1" & is.null(input_df$df_list)){
      showModal(modalDialog(
        title = "Warning - No Loaded Dataset",
        HTML(paste("There is no any loaded dataset ",
                   "Please select input and load it", 
                   sep = "<br/>")
        ), size = "s"
      ))
    }
  })
  #------------------------------ Data tab 2 - End ------------------------------------- 
  #------------------------------ Visualization Tab Start -------------------------------------  
  # Selecting the Dataset
  # Setting reactive values
  vis_df <- reactiveValues(df = NULL,
                           class = NULL,
                           var_factor = NULL,
                           var_numeric = NULL,
                           var_date = NULL)
  
  # Setting the data selection
  observeEvent({
    input_df$names_list
  },{
    
    output$loaded_ds_list_vis  <- renderUI({
      selectInput("select_df_vis", "Select Dataset",
                  choices = input_df$names_list
      )
    })
    
  })
  
  
  observeEvent({
    input$var_modify
    input$select_df_vis
  }, {
    if(!is.null(input$select_df_vis)){
      vis_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df_vis)]]
      )
      vis_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df_vis)]]
      
      vis_df$var_numeric <- vis_df$var_factor <- NULL
      if(!is.ts(vis_df$df)){
        for(i in 1:ncol(vis_df$df)){
          if(is.factor(vis_df$df[,i])){
            vis_df$var_factor <- c(vis_df$var_factor, names(vis_df$df)[i])
          } else if(is.numeric(vis_df$df[,i]) | is.integer(vis_df$df[,i])){
            vis_df$var_numeric <- c(vis_df$var_numeric,names(vis_df$df)[i])
          }
        }
      }
    } else{
      
      vis_df$df <- NULL
      vis_df$class <- NULL
      vis_df$var_factor <- NULL
      vis_df$var_numeric <- NULL
      
      
      
    }
  })
  
  
  observeEvent({input$var_modify
    input$select_df_vis},{
      if(!is.null(vis_df$var_numeric) & !is.ts(vis_df$df)){
        ###################### NEED TO ADD CASE FOR ONLY ONE VARIABE !!!!!!
        if(length(vis_df$var_numeric) == 1 ){
          output$vis_plot_type <- renderUI({
            selectInput("plot_type", "Select the Plot Type",
                        choices = list("Boxplot" = "box",
                                       "Histogram" = "hist",
                                       "Density" = "density"))
          })
          output$vis_one_var <- renderUI({
            selectInput("plot_var", "Select a Variable",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })
          
          output$vis_factor <- renderUI({
            if(!is.null(vis_df$var_factor)){
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = c("None", as.character(vis_df$var_factor))
              )
            } else {
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = "NA"
              )
            }
          })
          
        } else if(length(vis_df$var_numeric) > 1 ){
          output$vis_plot_type <- renderUI({
            selectInput("plot_type", "Select the Plot Type",
                        choices = list("Scatter" = "scatter",
                                       "Line" = "line",
                                       "Boxplot" = "box",
                                       "Histogram" = "hist",
                                       "Density" = "density",
                                       "Correlation" = "cor"))
          })
          
          output$vis_one_var <- renderUI({
            selectInput("plot_var", "Select a Variable",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })
          
          output$vis_x <- renderUI({
            selectInput("plot_x", "Select the X Axis",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })
          
          output$vis_y <- renderUI({
            selectInput(
              "plot_y", "Select the Y Axis",
              choices = vis_df$var_numeric,
              selected = vis_df$var_numeric[2]
            )
          })
          
          output$vis_factor <- renderUI({
            if(!is.null(vis_df$var_factor)){
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = c("None", as.character(vis_df$var_factor))
              )
            } else {
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = "NA"
              )
            }
          })
        }
        
      } else if(is.null(vis_df$var_numeric) & !is.ts(vis_df$df)){
        output$vis_x  <- renderUI({
          selectInput("plot_x", "Select Variables",
                      choices = "No Available Numeric Variables"
          )
        })
      } else if(is.ts(vis_df$df)){
        output$vis_plot_type <- renderUI({
          selectInput("plot_type", "Select the Plot Type",
                      choices = list("Scatter" = "scatter",
                                     "Line" = "line",
                                     "Boxplot" = "box",
                                     "Seasonal Plot" = "seasonal_plot",
                                     "Lags Plot" = "lags_plot"))
        })
      }
      
    })
  
  observeEvent({input$var_modify
    input$plot_factor
    input$plot_var
    input$plot_x
    input$plot_y
    input$plot_type
    vis_df$df
    input$select_df_vis
    
  },{
    
    output$main_plot <- renderPlotly({
      
      if(!is.ts(vis_df$df)){
        p <- x <- y <- color <-   NULL
        
        if(length(vis_df$var_numeric) > 1){
          y <- vis_df$df[,input$plot_y]
        } else if(length(vis_df$var_numeric) == 1){
          y <- NA
        }
        
        if(input$plot_type == "box" | input$plot_type == "density"){
          x <- vis_df$df[, input$plot_var]
        } else {
          x <- vis_df$df[,input$plot_x]
        }
        
        if(input$plot_factor != "None" & input$plot_factor != "NA" & !is.null(input$plot_factor)){
          color <- vis_df$df[,input$plot_factor]
          type <- vis_df$df[,input$plot_factor]
        } else {
          color <-  NULL
          type <- input$plot_var
        }
        
        p <- switch(input$plot_type,
                    "scatter" = {          
                      plot_ly(x = x, y = y, color = color) %>%
                        layout(xaxis = list(title = input$plot_x),
                               yaxis = list(title = input$plot_y))
                    },
                    "line" = {
                      plot_ly(x = x, y = y, mode = "lines", color = NULL)%>%
                        layout(xaxis = list(title = input$plot_x),
                               yaxis = list(title = input$plot_y))
                    },
                    "box" = {
                      plot_ly(y = x, type = "box", color = color, 
                              name = names(vis_df$df)[which(names(vis_df$df) == input$plot_factor)],
                              boxpoints = "all", jitter = 0.3,
                              pointpos = -1.8)%>%
                        layout(yaxis = list(title = names(vis_df$df)[which(names(vis_df$df) == input$plot_x)]),
                               xaxis = list(title = "")
                        )
                    },
                    "hist" = {
                      p_hist <- NULL
                      if(input$plot_factor == "None" | input$plot_factor == "NA"){
                        p_hist <- plot_ly(x = vis_df$df[,input$plot_var], type = "histogram") 
                      } else if(input$plot_factor != "None" & 
                                input$plot_factor != "NA" & 
                                !is.null(input$plot_factor)){
                        
                        plot_list <- l <- NULL
                        for(l1 in levels(vis_df$df[,input$plot_factor])){
                          hist.sub.df <- subset(vis_df$df, vis_df$df[,input$plot_factor] == l1)
                          l <- length(plot_list)
                          plot_list[[l + 1]] <- plot_ly(hist.sub.df, 
                                                        x = hist.sub.df[,input$plot_var], 
                                                        name = l1) %>%
                            layout(xaxis = list(title = l1),
                                   title = input$plot_var)
                          
                        }
                        p_hist <- subplot(plot_list, titleX = TRUE, shareX = TRUE) %>% 
                          hide_legend()
                      }  
                      p_hist
                    },
                    "density" = {
                      plot_den <- NULL
                      if(input$plot_factor == "None" | input$plot_factor == "NA"){
                        dens <- density(x)
                        dens.df <- data.frame(x = dens$x, y = dens$y)
                        min_y <- 0 
                        max_y <- max(dens.df$y)
                        plot_den <- plot_ly(data = dens.df, x  = ~x, 
                                            y = ~y)
                      } else if(input$plot_factor != "None" & 
                                input$plot_factor != "NA" & 
                                !is.null(input$plot_factor)){ 
                        
                        plot_list_den <- l <-  NULL 
                        for(l2 in levels(vis_df$df[, input$plot_factor])){
                          df.den <- subset(vis_df$df, 
                                           vis_df$df[, input$plot_factor] == l2)
                          l <- length(plot_list_den)
                          dens <- density(df.den[,input$plot_var])
                          dens.df <- data.frame(x = dens$x, y = dens$y)
                          plot_list_den[[l + 1]] <- plot_ly(data = dens.df, 
                                                            x = ~x, 
                                                            y = ~y)%>%
                            layout(xaxis = list(title = l2),
                                   title = input$plot_var)
                        }
                        
                        plot_den <- subplot(plot_list_den, titleX = TRUE, shareX = TRUE)%>% 
                          hide_legend()
                      }
                      plot_den
                      
                      
                    },
                    "cor" = {
                      c <- NULL
                      c <- round(cor(vis_df$df[, which(colnames(vis_df$df) %in% vis_df$var_numeric)]), 3)
                      plot_ly(x = vis_df$var_numeric, y = vis_df$var_numeric, z = c, 
                              key = c, type = "heatmap", source = "heatplot")
                    }
        )
      } else if(is.ts(vis_df$df)){
        ts.df <- data.frame(dec_left = floor(time(vis_df$df)),
                            dec_right = round((time(vis_df$df) - floor(time(vis_df$df))) * 
                                                frequency(vis_df$df) + 1), 
                            value = as.numeric(vis_df$df))
        p <- switch(input$plot_type,
                    "line" = {
                      plot_ly( x = time(vis_df$df), y = vis_df$df, type  = "scatter", mode = "line")
                    },
                    "scatter" = {
                      plot_ly( x = time(vis_df$df), y = vis_df$df, type  = "scatter")
                    },
                    "box" = {
                      plot_ly(data = ts.df, y = ~ value ,
                              color = ~ as.factor(dec_right), 
                              type = "box", 
                              boxpoints = "all", jitter = 0,
                              pointpos = -1.8)
                      
                    },
                    
                    "seasonal_plot" = {
                      if(frequency(vis_df$df) == 1){
                        p <- plot_ly()
                        showModal(modalDialog(
                          title = "Warning - Seasonal Plot is Not Available",
                          HTML(paste("Seasonal plot is not available",
                                     "for time series object with yearly frequancy", 
                                     sep = "<br/>")
                          ), size = "s"
                        ))
                        p
                      } else {
                        ts.df_wide <- reshape2::dcast(ts.df, dec_right ~ dec_left )
                        p <- plot_ly()
                        
                        for(f in 2:ncol(ts.df_wide)){
                          p <- p %>% add_trace(x = ts.df_wide[,1], y = ts.df_wide[,f],
                                               name = paste("time", names(ts.df_wide)[f], sep = " " ),
                                               mode = "line")
                        }
                        p
                      }
                    },
                    "lags_plot" = {
                      lag <- NULL
                      lag_plots <- NULL 
                      max.lags <- 12
                      for(g in 1:max.lags){
                        if(g == 1){
                          lag <- c(NA, ts.df$value[- nrow(ts.df)]) 
                        } else {
                          lag <- c(NA,lag[-nrow(ts.df)])
                        }
                        lag_plots[[g]] <- plot_ly(x = lag, y = ts.df$value, 
                                                  name = paste("Lag", g, sep = " ")) %>%
                          layout(xaxis = list(title = paste("Lag", g, sep = " "),
                                              range = c( min(na.omit(as.numeric(lag))),  
                                                         max(na.omit(as.numeric(lag))))),
                                 yaxis = list(title = paste("Series", sep = ""),
                                              range = c( min(na.omit(as.numeric(ts.df$value))),  
                                                         max(na.omit(as.numeric(ts.df$value))))),
                                 title = paste(input$select_df_vis,"Series vs Lags", sep = " "),
                                 annotations = list(
                                   # x = median(na.omit(as.numeric(lag))),
                                   # y = median(na.omit(as.numeric(ts.df$value))),
                                   showarrow = FALSE,
                                   # arrowhead = 4,
                                   # arrowsize = 0.5,
                                   # ax = 20,
                                   # ay = -20,
                                   xref = paste("x", g, sep = ""),
                                   yref = paste("y", g, sep = ""),
                                   text = paste("Lag", g, sep = " "))
                          )
                      }
                      
                      subplot(lag_plots, 
                              titleX = FALSE, titleY = TRUE,
                              shareX = FALSE, shareY = FALSE, 
                              margin = 0.05,
                              nrows = ceiling(length(lag_plots) / 3))%>% 
                        hide_legend()
                    }
        )
        
      }
      
      
      
      
    }) 
    return(p)
  })
  
  output$class_df_flag_vis <- reactive({
    ifelse(is.ts(vis_df$df), TRUE, FALSE)
  })
  outputOptions(output, "class_df_flag_vis", suspendWhenHidden = FALSE)  
  #------------------------------ Server Function - End -------------------------------------  
}


#------------------------------ UI Function -------------------------------------
ui <- dashboardPage(
  dashboardHeader(),
  #------------------------------ Side Bar Function -------------------------------------
  dashboardSidebar(
    sidebarMenu(id = "tabs", 
                menuItem("Data", tabName = "data", icon = icon("dashboard"), startExpanded = TRUE,
                         menuSubItem("Data", tabName = "data1"),
                         menuSubItem("Data Prep", tabName = "data2")
                ),
                menuItem("Visualization", icon = icon("bar-chart-o"), tabName = "vis")
    )
  ),
  #------------------------------ Dashboard Body -------------------------------------
  dashboardBody(
    #------------------------------ Tags Style -------------------------------------    
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    #------------------------------ Tabs Start -------------------------------------     
    tabItems(
      #------------------------------ Tabs Data Start-------------------------------------
      tabItem(tabName = "data1",
              #------------------------------ Tabs Data - fluid page start -------------------------------------
              fluidPage(
                #------------------------------ Tabs Data - fluid row 1 -------------------------------------
                fluidRow(
                  infoBoxOutput("installed_datasets"),
                  infoBoxOutput("in_memory_df"),
                  infoBoxOutput("load_datasets")
                ),
                #------------------------------ Tabs Data - fluid row 2 -------------------------------------
                fluidRow(
                  box(
                    width = 4, height = 100,
                    selectInput('data_source', 'Select Data Source', 
                                list(
                                  "R Data Frame" = "data_frame",
                                  "R Time Series" = "time_series",
                                  "Installed Package Dataset" = "inst_pack",
                                  "Import CSV File" = "import"
                                ))
                  ),
                  box(width =  4, height = 100,
                      conditionalPanel(condition = "input.data_source.includes('data_frame') || input.data_source.includes('inst_pack') || input.data_source.includes('time_series')",
                                       uiOutput("df_list")
                      ),
                      conditionalPanel(condition = "input.data_source == 'import'",
                                       
                                       
                                       dropdownButton(
                                         fileInput('file1', 'Choose CSV File',
                                                   accept=c('text/csv', 
                                                            'text/comma-separated-values,text/plain', 
                                                            '.csv')),
                                         awesomeCheckbox(inputId = "csv_header", 
                                                         label = "Header", 
                                                         value = TRUE),
                                         radioButtons('sep', 'Separator',
                                                      c(Comma=',',
                                                        Semicolon=';',
                                                        Tab='\t'),
                                                      ','),
                                         radioButtons('quote', 'Quote',
                                                      c(None='',
                                                        'Double Quote'='"',
                                                        'Single Quote'="'"),
                                                      '"'),
                                         circle = TRUE, status = "danger", 
                                         icon = icon("file-text", lib = "font-awesome"), width = "300px",
                                         tooltip = tooltipOptions(title = "Click to set csv file parameters !")
                                       )
                      )
                      
                  ),
                  box(width =  4, height = 100,
                      conditionalPanel(condition = "(output.load_flag == '2' && input.data_source == 'inst_pack') ||  (output.load_flag == '2' && input.data_source == 'import' ) || (output.load_flag == '1' && input.data_source == 'data_frame' ) || ( output.load_flag == '1' && input.data_source == 'inst_pack') || (output.load_flag == '1' && input.data_source == 'time_series')",
                                       actionButton("load", "Load")
                      ),
                      conditionalPanel(condition =  "output.loaded_table_flag == '1'",
                                       actionButton("remove", "Remove")
                      )
                  )
                ),
                fluidRow(
                  box(width = 7, title = "Preview Table", 
                      div(style = 'overflow-x: scroll',
                          DT::dataTableOutput('view_table'))
                  ),
                  
                  box(width = 5, title = "Loaded Datasets",
                      div(style = 'overflow-x: scroll',
                          DT::dataTableOutput('list_loaded_df'))
                  )
                )
                
                
                
                
                #------------------------------ Tabs Data - fluid row 2 -------------------------------------
              )
              #------------------------------ Tabs Data - fluid page end -------------------------------------
              
      ),
      #------------------------------ Tabs Data2 Start-------------------------------------
      tabItem(tabName = "data2",
              fluidPage(
                fluidRow(
                  conditionalPanel(condition =  "output.loaded_table_flag == '1'",
                                   infoBoxOutput("data_name"),
                                   infoBoxOutput("num_var"),
                                   infoBoxOutput("num_obs")
                  )
                ),
                fluidRow(
                  conditionalPanel(condition =  "output.loaded_table_flag == '1'",
                                   box(width = 4, title = "Select Dataset",
                                       uiOutput("loaded_ds_list"),
                                       conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == false ",
                                                        selectInput('data_option', 'Select Option', 
                                                                    list(
                                                                      "Variables Attributes" = "var_attr",
                                                                      "Data Summarise" = "data_summary",
                                                                      "Reshape Options" = "data_reshape"
                                                                    ))
                                       ),
                                       conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == false && input.data_option == 'var_attr'",
                                                        radioButtons("class_selection", label = "Variables Modification", 
                                                                     choices = list(Numeric = "numeric", Factor = "factor", 
                                                                                    Character = "character",
                                                                                    Date = "date"),
                                                                     selected = "numeric"),
                                                        conditionalPanel(condition =  "input.class_selection == 'date' && output.loaded_table_flag == '1' && output.class_df_flag == false && input.data_option == 'var_attr'",
                                                                         selectInput('date_format', "Select the Date Format",
                                                                                     list(
                                                                                       YMD = "ymd",
                                                                                       YDM = "ydm",
                                                                                       MYD = "myd",
                                                                                       MDY = "mdy",
                                                                                       DMY = "dmy",
                                                                                       DYM = "dym"
                                                                                     )),
                                                                         #titlePanel(h5("Date Preview")),
                                                                         tags$h5("Date Preview"),
                                                                         verbatimTextOutput("date_prev")
                                                        ),
                                                        actionButton("var_modify", "Modify")
                                       ),
                                       conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == true ",
                                                        tableOutput("ts_table")             
                                       )
                                       
                                   )),
                  conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == false ",
                                   box(width = 4, title = "List of Variables",
                                       DT::dataTableOutput("data_tab2_var")
                                       
                                   ),
                                   box(width = 4, title = "Variable Summary",
                                       plotlyOutput("data_tab2_summary_plot",height = 200),
                                       tableOutput("data_tab2_var_summary")
                                   )
                  ),
                  conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == true ",
                                   box(width = 8, title = "Time Series Plot",
                                       dropdownButton(
                                         tags$h3("List of Input"),
                                         materialSwitch(inputId = "ts_plot_log", label = "Log Transformation", 
                                                        status = "primary", right = FALSE),
                                         awesomeRadio(inputId = "ts_prep_mode", 
                                                      label = "Radio buttons", 
                                                      choices = c("lines","lines+markers", "markers")
                                                      , selected = "lines"),
                                         
                                         
                                         circle = TRUE, status = "danger", icon = icon("gear"), width = "200px",
                                         tooltip = tooltipOptions(title = "Click to see inputs !")
                                       ),
                                       plotlyOutput("data_tab2_ts")
                                       
                                   )
                  )
                  
                ),
                
                fluidRow(
                  conditionalPanel(condition = "output.loaded_table_flag == '1'",
                                   div(style = 'overflow-x: scroll',
                                       DT::dataTableOutput("data_tab2_table")) 
                  )
                )
              )
              
      ),
      #------------------------------ Tabs Data End-------------------------------------
      #------------------------------ Tabs Visualization Start-------------------------------------
      
      tabItem(tabName = "vis",
              conditionalPanel(condition =  "output.loaded_table_flag == '1'",
                               fluidPage(
                                 fluidRow(
                                   box(width = 2,
                                       uiOutput("loaded_ds_list_vis"),
                                       uiOutput("vis_plot_type")
                                       
                                   ),
                                   conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag_vis == false && input.plot_type != 'cor' ",
                                                    box(width = 2,
                                                        conditionalPanel(condition = "input.plot_type != 'density' && input.plot_type != 'box' && input.plot_type != 'hist'",
                                                                         uiOutput("vis_x"),
                                                                         uiOutput("vis_y")
                                                        ),
                                                        conditionalPanel(condition = "input.plot_type == 'density' || input.plot_type == 'hist' || input.plot_type == 'box'",
                                                                         uiOutput("vis_one_var")
                                                        )
                                                    ),
                                                    conditionalPanel(condition = "input.plot_type == 'scatter' || input.plot_type == 'box'|| input.plot_type == 'hist' || input.plot_type == 'density'",
                                                                     box(width = 2,
                                                                         uiOutput("vis_factor"))
                                                    )
                                   )
                                 ),
                                 fluidRow(
                                   box(width = 12, title = "plot",
                                       plotlyOutput("main_plot")
                                       
                                   )
                                 )
                               )
              )
      )
      #------------------------------ Tabs Visualization End-------------------------------------
      
      
    )
  )
)

#------------------------------ Call the App -------------------------------------
runApp(list(ui = ui, server = server), launch.browser = TRUE)
