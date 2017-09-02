library(shinydashboard)
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
