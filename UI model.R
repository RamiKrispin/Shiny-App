tabItem(tabName = "models1",
        fluidRow(conditionalPanel(condition = "input.model_package == 'H2O'",
                                  infoBoxOutput("available_memory"),
                                  infoBoxOutput("h2o_status_box"),
                                  conditionalPanel(condition = "output.h2o_flag == '3'",
                                                   infoBoxOutput("h2o_cluster_mem")  
                                  )
        )
        ),
        
        
        fluidRow(
          # Select the data frame
          box(width = 3, title = "Model Inputs",
              conditionalPanel(condition = "output.h2o_df_loaded == '1' && output.model_binomial == '2' && output.dep_var_class == '1'",
                               selectInput("binomial_models", "Select Classification Model",
                                           choices = c("GLMNET" = "glmnet",
                                                       "GBM" = "gbm",
                                                       "Random Forest" = "rf")
                               ),
                               conditionalPanel( condition = "input.binomial_models == 'rf'",
                                                 sliderInput("h2o_rf_ntree", "Number of Trees",
                                                             min = 25, max = 1000,
                                                             value = 50),
                                                 sliderInput("h2o_rf_max_depth", "Maximum Tree Depth",
                                                             min = 1, max = 30,
                                                             value = 20
                                                 ),
                                                 actionButton("rf_h2o_run", "Run Model")
                                                 
                               )
                               
              ),
              uiOutput("models1_df_list"),
              conditionalPanel(condition = "output.model_tab_input == '1' || output.model_tab_input == '2'",
                               uiOutput("models1_var_list"),
                               uiOutput("models1_independent_list")
              ),
              conditionalPanel(condition = "output.model_tab_ind == '1'",
                               awesomeCheckboxGroup(inputId = "model_package", 
                                                    label = "Set Packages", 
                                                    choices = c("H2O"), selected = NULL, 
                                                    inline = TRUE),
                               conditionalPanel(condition = "output.h2o_flag == '0'",
                                                actionButton("install_h2o", "Install H2O")),
                               conditionalPanel(condition = "output.h2o_flag == '1' && input.model_package == 'H2O'",
                                                actionButton("load_h2o", "Load H2O")),
                               conditionalPanel(condition = "output.h2o_flag == '2' && input.model_package == 'H2O'",
                                                uiOutput("h2o_into_ram"),
                                                actionButton("h2o_start", "Start Connection")
                               ),
                               conditionalPanel(condition = "output.h2o_flag == '3' && input.model_package == 'H2O'",
                                                materialSwitch(inputId = "h2o_validation", 
                                                               label = "Add Validation Partition", 
                                                               status = "primary", right = FALSE),
                                                conditionalPanel(condition = "input.h2o_validation == true",
                                                                 sliderInput("h2o_split_v", "Set the Training/Testing/Validation Partitions:",
                                                                             min = 0.05, max = 1,
                                                                             value = c(0.6,0.8))),
                                                conditionalPanel(condition = "input.h2o_validation == false",
                                                                 sliderInput("h2o_split", "Set the Training/Testing Partitions:",
                                                                             min = 0.05, max = 1,
                                                                             value = 0.7)),
                                                uiOutput("h2o_load_data")
                               )
                               
              )
          ),
          
          box(width = 6, title = "Score History",
              plotlyOutput("rf_error_plot")
          ),
          box(width = 3, title = "Summary"
              
          )
        )
        
)