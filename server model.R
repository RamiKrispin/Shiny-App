models_df <- reactiveValues(df = NULL, # Load the selected data frame
                            var_list = NULL, # Create a variable list
                            independent_var = NULL, # Create the independent variables list
                            var_dep_class = NULL # The class of the dependent variable
)

# Select the dataset
observeEvent({
  input$var_modify
  input_df$names_list
},{
  if(length(input_df$names_list[which(input_df$df_class == "Data Frame")]) == 0){
    output$models1_df_list  <- renderUI({
      output$model_tab_input <- reactive("0")
      outputOptions(output, "model_tab_input", suspendWhenHidden = FALSE)
      models_df$var_list <-  models_df$df <- NULL
      showModal(modalDialog(
        title = "Warning - No Available Data Frame",
        HTML(paste("No available data frame in the platform",
                   "Use the Data tab to load data", 
                   sep = "<br/>")
        ), size = "s"
      ))
      output$models1_df_list  <- renderUI({
        selectInput("models1_select_df", "Select Dataset",
                    choices = "NA"
        )
      })
    })
  } else if(length(input_df$names_list[which(input_df$df_class == "Data Frame")]) > 0){
    output$model_tab_input <- reactive("1")
    outputOptions(output, "model_tab_input", suspendWhenHidden = FALSE)
    output$models1_df_list  <- renderUI({ 
      selectInput("models1_select_df", "Select Dataset",
                  choices = input_df$names_list[which(input_df$df_class == "Data Frame")]
      )
    })
  }
})

# Update the dataset selection
observeEvent({
  input$var_modify
  input$models1_select_df
  
},{
  if(length(input_df$names_list[which(input_df$df_class == "Data Frame")]) > 0){
    output$model_tab_input <- reactive("1")
    outputOptions(output, "model_tab_input", suspendWhenHidden = FALSE)
    models_df$df <- input_df$df_list[[which(input_df$names_list == input$models1_select_df)]]
  } else {
    output$model_tab_input <- reactive("0")
    outputOptions(output, "model_tab_input", suspendWhenHidden = FALSE)
    models_df$df <- NULL
  }
})

# Dependent variable
observeEvent({
  input$var_modify
  input$models1_select_df
  
}, {
  if(!is.null(models_df$df)){
    models_df$var_list <- names(models_df$df)
    output$model_tab_ind <- reactive("1")
    outputOptions(output, "model_tab_ind", suspendWhenHidden = FALSE)
    output$models1_var_list  <- renderUI({
      selectInput("models1_select_var", "Select the Dependent Variable",
                  choices = c("Select Variable",models_df$var_list)
      )
    })
  } else if(is.null(models_df$df)){
    models_df$var_list <- NULL
    output$model_tab_ind <- reactive("0")
    outputOptions(output, "model_tab_ind", suspendWhenHidden = FALSE)
  }
  
})

# Independent variable
observeEvent(input$models1_select_var, {
  
  if(input$models1_select_var != "Select Variable"){
    
    models_df$var_dep_class <- class(models_df$df[,which(names(models_df$df) == input$models1_select_var)])
    models_df$independent_var <- setdiff(names(models_df$df), c(input$models1_select_var, "name"))
    output$model_tab_ind <- reactive("1")
    outputOptions(output, "model_tab_ind", suspendWhenHidden = FALSE)
    output$models1_independent_list  <- renderUI({
      
      pickerInput(inputId = "models1_independent", 
                  label = "Select the Independent Variable", 
                  choices = models_df$independent_var, options = list(`actions-box` = TRUE), 
                  multiple = TRUE,
                  selected = models_df$independent_var)
      
    })
  } else if(input$models1_select_var == "Select Variable"){
    models_df$independent_var <- NULL      
    models_df$var_dep_class <- NULL
    output$model_tab_ind <- reactive("0")
    outputOptions(output, "model_tab_ind", suspendWhenHidden = FALSE)
  }
})


observeEvent(models_df$var_dep_class,{
  if(models_df$var_dep_class == "factor"){
    if(levels(models_df$df[,which(names(models_df$df) == input$models1_select_var)]) == 2){
      output$model_binomial <- reactive("1") # set condition for binomial model
      outputOptions(output, "model_binomial", suspendWhenHidden = FALSE)
    } else if(levels(models_df$df[,which(names(models_df$df) == input$models1_select_var)]) > 2){
      output$model_binomial <- reactive("2") # set condition for multinomial model
      outputOptions(output, "model_binomial", suspendWhenHidden = FALSE)
    } else {
      output$model_binomial <- reactive("0") # not engough levels for binomial/multinomial
      outputOptions(output, "model_binomial", suspendWhenHidden = FALSE)
    }
    output$dep_var_class <- reactive("1") # flag for factor variable
    outputOptions(output, "dep_var_class", suspendWhenHidden = FALSE)
  } else if (models_df$var_dep_class == "numeric" |
             models_df$var_dep_class == "integer") {
    output$dep_var_class <- reactive("2") # flag for numeric/integer variable
    outputOptions(output, "dep_var_class", suspendWhenHidden = FALSE)
    # 
    output$model_binomial <- reactive("0") # reseting the binomial flag
    outputOptions(output, "model_binomial", suspendWhenHidden = FALSE)
  }
})

#------------------------------ Setting the H2o Package ------------------------------------- 
h2o_df <- reactiveValues(status = FALSE,
                         free_mem = NULL,
                         num_cpus = NULL,
                         df = NULL,
                         class = NULL,
                         var_factor = NULL,
                         var_numeric = NULL,
                         df.h2o = NULL,
                         train = NULL,
                         test = NULL,
                         valud = NULL,
                         x = NULL,
                         y = NULL)


observeEvent({
  input$model_package
  
},{
  if("H2O" %in% input$model_package){
    if(!"h2o" %in% installed.packages()){
      
      showModal(modalDialog(
        title = "Warning - H2O is not Available",
        HTML(paste("The H2O package is not installed.", 
                   "Please install the package to continue.", 
                   sep = "<br/>")
        ), size = "s"
      ))
      output$h2o_flag <- reactive("0")
      outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
    } else if("h2o" %in% installed.packages() & !"package:h2o" %in% search()){
      output$h2o_flag <- reactive("1")
      print("Need to Load")
      outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
    } else if("h2o" %in% installed.packages() & "package:h2o" %in% search()){
      output$h2o_flag <- reactive("2")
      outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
      print("No Need to Load")
    }
  } else {
    print("YYY")
  }
  
  
})

# Load H2O
observeEvent(input$load_h2o,{
  require(h2o)
  if ("h2o" %in% installed.packages() & "package:h2o" %in% search()){
    output$h2o_flag <- reactive("2")
    outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
    
  } else if("h2o" %in% installed.packages() & !"package:h2o" %in% search()){
    showModal(modalDialog(
      title = "Warning - Failed to Load H2O",
      HTML(paste("Could not load H2O, please check if the package was installed currectly", 
                 "For further information, please check H2O User Documentation:",
                 "https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/3/index.html",
                 sep = "<br/>")
      ), size = "s"
    ))
  }
})

# Install H2O
observeEvent(input$install_h2o,{
  # The following two commands remove any previously installed H2O packages for R.
  if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
  if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
  
  # Next, we download packages that H2O depends on.
  pkgs <- c("statmod","RCurl","jsonlite")
  for (pkg in pkgs) {
    if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
  }
  
  # Now we download, install and initialize the H2O package for R.
  install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/3/R")
  if ("h2o" %in% installed.packages() & !"package:h2o" %in% search()){
    output$h2o_flag <- reactive("1")
    outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
  } else if(!"h2o" %in% installed.packages()){
    install.packages("h2o")
    if ("h2o" %in% installed.packages() & !"package:h2o" %in% search()){
      output$h2o_flag <- reactive("1")
      outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
    } 
  }else if(!"h2o" %in% installed.packages()){
    showModal(modalDialog(
      title = "Warning - Installation Failed",
      HTML(paste("There was a problem to installed H2O.", 
                 "For further information, please check H2O User Documentation:",
                 "https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/3/index.html",
                 sep = "<br/>")
      ), size = "s"
    ))
  }
})

output$h2o_into_ram <- renderUI({
  sliderInput("max_mem", "Set the Max Memory Size:",
              min = 1, max = ceiling(get_free_ram() / 1024 ^ 2),
              value = ceiling(get_free_ram() / 1024 ^ 2))
})


output$available_memory <- renderValueBox({
  valueBox(
    paste(round(get_free_ram() / 1024 ^ 2,2), "GB", sep = ""), "Free Physical Memory", 
    icon = icon("microchip"),
    color = "purple"
  )
})


observeEvent(input$h2o_start, {
  if(input$h2o_start){
    mem <- NULL
    print(input$max_mem)
    
    if(is.null(mem)){mem <- 1}
    h2o.init(nthreads=-1 , max_mem_size = paste(mem, "g", sep = ""))
    h2o.removeAll()
    output$h2o_flag <- reactive("3")
    outputOptions(output, "h2o_flag", suspendWhenHidden = FALSE)
    h2o_df$status <- h2o.clusterIsUp()
    cluster_status <- h2o.clusterStatus()
    h2o_df$free_mem <- as.numeric(cluster_status$free_mem)
    h2o_df$num_cpus <- as.numeric(cluster_status$num_cpus)
  }
})
output$h2o_status_box <- renderValueBox({
  valueBox(
    ifelse(h2o_df$status, "Connected","Disconnected" ), "H2O Status", icon = icon("signal"),
    color = ifelse(h2o_df$status, "green","red" )
  )
})


output$h2o_cluster_mem <- renderValueBox({
  valueBox(
    paste(round((h2o_df$free_mem / 1024^3), 2), "GB", sep = ""), 
    "H2O Cluster Total Memory", icon = icon("microchip"),
    color = "maroon"
  )
})

output$h2o_cpu <- renderValueBox({
  valueBox(
    h2o_df$num_cpus,  
    "Number of CPUs in Use", icon = icon("signal"),
    color = "light-blue"
  )
})

output$h2o_load_data <- renderUI({
  if(is.null(h2o_df$df.h2o)){
    actionButton("h2o_df_load", "Load to H2O")
  } else{
    actionButton("h2o_df_load", "Reload to H2O")
  }
})


observeEvent(input$h2o_df_load,{
  h2o.removeAll()
  h2o_df$df.h2o <- as.h2o(models_df$df)
  
  h2o_df$y <- h2o_df$x <- h2o_df$train <- h2o_df$test <- h2o_df$valid <- NULL
  
  h2o_df$y <- match(input$models1_select_var, names(h2o_df$df.h2o))
  h2o_df$x <- match(input$models1_independent, names(h2o_df$df.h2o))
  
  print(h2o_df$y)
  print(h2o_df$x)
  if(input$h2o_validation){
    
    splits <- h2o.splitFrame(
      data = h2o_df$df.h2o, 
      ratios = c(input$h2o_split_v[1],(input$h2o_split_v[2] - input$h2o_split_v[1])),   
      destination_frames = c("train", "valid", "test"), seed = 1234
    )
    h2o_df$train <- splits[[1]]
    h2o_df$valid <- splits[[2]]
    h2o_df$test  <- splits[[3]]
  } else {
    splits <- h2o.splitFrame(
      data = h2o_df$df.h2o, 
      ratios = c(input$h2o_split),   
      destination_frames = c("train", "test"), seed = 1234
    )
    h2o_df$train <- splits[[1]]
    h2o_df$test  <- splits[[2]]
  }
})

observeEvent({
  h2o_df$df.h2o
},{
  if(!is.null(h2o_df$df.h2o)){
    output$h2o_df_loaded <- reactive("1")
    outputOptions(output, "h2o_df_loaded", suspendWhenHidden = FALSE) 
  } else{
    output$h2o_df_loaded <- reactive("0")
    outputOptions(output, "h2o_df_loaded", suspendWhenHidden = FALSE) 
  }
})

#------------------------------ Setting the H2o Package ------------------------------------- 
h2o_rf <- reactiveValues(model = NULL)

observeEvent(input$rf_h2o_run,{
  h2o_rf$model <- NULL
  if(input$h2o_validation){
    h2o_rf$model <- h2o.randomForest(       
      training_frame = h2o_df$train,        
      validation_frame = h2o_df$valid,      
      x = h2o_df$x,                        
      y = h2o_df$y,                          
      ntrees = input$h2o_rf_ntree, 
      max_depth = input$h2o_rf_max_depth
    )
  } else if(!input$h2o_validation){
    h2o_rf$model <- h2o.randomForest(       
      training_frame = h2o_df$train,        
      x = h2o_df$x,                        
      y = h2o_df$y,                          
      ntrees = input$h2o_rf_ntree, 
      max_depth = input$h2o_rf_max_depth
    )   
  }
  
  
  sh <- h2o.scoreHistory(h2o_rf$model)
  
  output$error_plot <- renderPlotly({
    plot_ly(data = sh, x = ~number_of_trees, y =  ~ training_rmse, 
            type = "scatter", mode = "lines+markers", name = "Training", visible = TRUE) %>%
      add_trace(x = ~number_of_trees, y =  ~ validation_rmse, 
                type = "scatter", mode = "lines+markers", name = "Validation", visible = TRUE) %>%
      add_trace(x = ~number_of_trees, ~ training_classification_error, 
                type = "scatter", mode = "lines+markers", 
                name = "Training", visible = FALSE) %>%
      add_trace(x = ~number_of_trees, y =  ~ validation_classification_error, 
                type = "scatter", mode = "lines+markers", 
                name = "Validation", visible = FALSE) %>%
      layout(
        title = "Random Forest - Error Score History",
        yaxis = list(title = "Error"),
        xaxis = list(title = "Number of Trees"),
        updatemenus = list(
          list(
            y = 0.95, x= - 0.1,
            buttons = list(
              # RMSE
              list(method = "restyle",
                   args = list("visible", list(TRUE, TRUE,FALSE, FALSE)                        
                   ),
                   label = "RMSE"),
              list(method = "restyle",
                   args = list("visible", list(FALSE, FALSE, TRUE, TRUE)
                               
                   ),
                   label = "Classification")
            ))))
  })
  
  
})
