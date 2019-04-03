library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse) 
library(lubridate)
library(stringr)
library(viridis)
library(ggthemr)
library(ggrepel)
library(rdrop2)

source("scripts/make-transfer-in-session.R")

# change the paths below to match where your data is stored. for loading from dropbox, the paths should be relative to the dropbox root
slopes <- drop_read_csv("Skeletonema-marinoi-salinity-reaction-norms-data/measurements/slopes.csv", stringsAsFactors=FALSE)
dd <- drop_read_csv("Skeletonema-marinoi-salinity-reaction-norms-data/measurements/all_data.csv", stringsAsFactors=FALSE)

# if running locally (without dropbox), comment the two lines above and uncomment the four lines below. best to use absolute paths
# source("full/path/to/prepare-data.R")
# source("full/path/to/calculate-slopes.R")
# slopes <- read_csv("full/path/to/slopes.csv")
# dd <- read_csv("full/path/to/all_data.csv")

# set the levels according to your treatments and desired order
# currently assumes the treatment variable is called `salinity``, 
# if the treatment column is called something else, modify these two lines appropriately
# in this case additional changes will be needed in app.R
slopes <- slopes %>% drop_na %>% mutate(treatment=factor(paste(treatment, "ppt"), levels=paste(c(8,12,16,20,24,28), "ppt")))
dd <- mutate(dd, treatment=factor(paste(treatment, "ppt"), levels=paste(c(8,12,16,20,24,28), "ppt")))

# to make sure these variables are in the appropriate format
dd <- mutate(dd, rep=factor(replicate))
dd <- mutate(dd, dayn=as.numeric(day))

ui <- fluidPage(theme = "flatly",
   
   titlePanel("Skeletonema marinoi salinity reaction norms"),
   
   sidebarLayout(fluid = TRUE,
      sidebarPanel(width = 2,
        checkboxInput('subset_zones', 'Subset by Baltic Sea zone', FALSE),
        conditionalPanel(condition = 'input.subset_zones',
                         wellPanel(
                           checkboxGroupButtons(
                             inputId = "choose_zones",
                             label = "Zones:",
                             choices = dd$zone %>% unique,
                             selected = dd$zone %>% unique %>% .[1],
                             status="primary", 
                             size="xs",
                             individual = TRUE)
                         )
        ),
        checkboxInput('subset_treatments', 'Subset by salinity treatment', FALSE),
        conditionalPanel(condition = 'input.subset_treatments',
                         wellPanel(
                           checkboxGroupButtons(
                             inputId = "choose_treatments",
                             label = "Treatments:",
                             choices = as.character(dd$treatment) %>% unique,
                             selected = as.character(dd$treatment) %>% unique,
                             status="primary", 
                             size="xs",
                             individual = TRUE
                           )
                         )
        ),
        checkboxInput('subset_replicates', 'Subset by replicate', FALSE),
        conditionalPanel(condition = 'input.subset_replicates',
                         wellPanel(
                           checkboxGroupButtons(
                             inputId = "choose_replicates",
                             label = "Replicates:",
                             choices = 1:3,
                             selected = 1:3,
                             status="primary", 
                             size="xs",
                             individual = TRUE)
                         )
        ),
        checkboxInput('subset_transfer', 'Subset by transfer', FALSE),
        conditionalPanel(condition = 'input.subset_transfer',
                         wellPanel(
                           checkboxGroupButtons(
                             inputId = "choose_transfer",
                             label = "Transfers:",
                             choices = paste("t", 1:length(unique(dd$transfer)), sep=""),
                             selected =paste("t", 1:length(unique(dd$transfer)), sep=""),
                             status="primary",
                             size="xs",
                             individual = TRUE
                           )
                         )
        ),  
        checkboxInput('facetting', 'Faceting', FALSE),
        conditionalPanel(condition = 'input.facetting',
                         wellPanel(
                           checkboxGroupButtons(
                             inputId = "choose_facets",
                             label = "Facet by:",
                             choices = c("Strain","Treatment", "Zone", "Transfer", "Replicate"),
                             selected = c("Strain","Replicate"),
                             status="primary", 
                             size="xs",
                             individual = TRUE
                           ),
                           numericInput("n_facet_cols", "Number of faceting columns", value = 8)
                         )  
        ),
        
        checkboxInput('y_axis', 'Y axis settings', FALSE),
        conditionalPanel(condition = 'input.y_axis',  
                         wellPanel(
                           radioGroupButtons(
                             inputId = "choose_y_value",
                             label = "Y axis is:",
                             choices = c("Sum", "Mean", "Median"),
                             selected = "Sum"),
                           checkboxInput("toggle_log_scale", "Toggle log y axis", value = FALSE),
                           checkboxInput("toggle_free_y_scale", "Toggle separate y axes", value = FALSE)
                         )
        ),
        tags$head(tags$style(HTML("#plotGrowth {background-color: #043969; color: white;}"))),
        actionButton(inputId = "plotGrowth", label = "Plot")
      ),
      
      mainPanel(width = 10,
                tabsetPanel(
                  tabPanel("About",
                           includeMarkdown("www/About.md")
                  ),
                  tabPanel("Data",
                           dataTableOutput("growth_data") 
                  ),
                  tabPanel("Growth curves",
                           plotOutput("growth_plot",height = 800)
                           ),
                  tabPanel("Reaction norms",
                           plotOutput("slope_plot",height = 800) 
                           ),
                  tabPanel("Summarised reaction norms",
                           plotOutput("slope_mean_sd",height = 800) 
                  ),
                  tabPanel("Transfer",
                           column(width = 4,
                                  tags$br(),
                           wellPanel(
                             selectizeInput(
                               inputId = "which_plt",
                               label = "Select plate to transfer:",
                               choices = unique(dd$plate_name),
                               selected = NULL,
                               multiple = FALSE,
                               options = list(
                                 placeholder = 'Choose plate(s) (name is Zone + replicate, e.g., a1)',
                                 onInitialize = I('function() { this.setValue(""); }')
                               )
                             ),
                             helpText("In the background the data are filtered by today's date, so if the particular plate name + transfer number combination was not measured today, the rendered table will be empty.")
                           ),
                           wellPanel(
                             prettyRadioButtons(
                               inputId = "which_trn",
                               label = "Select the transfer number:",
                               choices = unique(dd$transfer),
                               selected = unique(dd$transfer)[4],
                               inline = TRUE,
                               status = "primary"
                               )
                           ),
                           wellPanel(
                             prettyRadioButtons(
                               inputId = "dilute_by",
                               label = "Calculate dilution using:",
                               inline = TRUE,
                               choices = c("Sum", "Mean", "Median"),
                               selected = "Sum")
                           ),
                           wellPanel(
                             helpText("Adjust the starting relative fluorescence (RFU) value based on the chosen statistic for calculating dilution."),
                             numericInput(inputId = "start_rfu", label = "Set the starting RFU:", value = 1000),
                             numericInput(inputId = "max_volum", label = "Set the maximum volume per well:", value = 1000)
                           ),
                           actionButton(inputId = "make_transfer", label = "Make files for transfer with Mari"),
                           downloadButton(outputId = "download_mari_fill", label = "Fill csv"),
                           downloadButton(outputId = "download_mari_transfer", label = "Transfer csv")
                           ),
                           column(width=8,
                                  tabsetPanel(
                                    tabPanel(title = "Mari fill csv",
                                      tableOutput("mari_fill")
                                    ),
                                    tabPanel(title = "Mari transfer csv",
                                      tableOutput("mari_transfer")
                                    )
                                    
                                  )
                                  )
                           ),
                  tabPanel("Help",
                           includeMarkdown("www/Usage.md")
                           )
                  
                )
          )
        )
      )

server <- function(input, output) {
  ggthemr("grape")
  
  ##### --- data tab ------------------------------------------------- #####
  output$growth_data <- renderDataTable(
    dd %>% select(-experiment, -plate_number, -rep, -dayn) %>% 
      select(plate=plate_name, well, row, column, zone, strain, treatment, replicate, transfer, date, day, mean=Mean, sd=SD, median=Median, sum=Sum),
    options = list(pageLength = 10)
  )

  ##### --- growth curves tab ---------------------------------------- #####
  # one event reactive takes care of both growth curves and reaction norms tab  
  
  make_plot <- eventReactive( {
    input$plotGrowth
  } , {
    
    validate(
      need(length(input$choose_zones) > 0 && length(input$choose_treatments) > 0  , "Eh'nt. Please select a zone and treatment.")
    )
    
    n_col <- input$n_facet_cols
    wanted_zones <- input$choose_zones
    wanted_treatments <- input$choose_treatments
    wanted_replicates <- input$choose_replicates
    wanted_transfers <- input$choose_transfer
    wanted_facets <- tolower(input$choose_facets)
    wanted_y <- as.name(input$choose_y_value)  
    
    dd <- dd %>% 
      filter(zone %in% wanted_zones ) %>% 
      filter(treatment %in% wanted_treatments) %>% 
      filter(replicate %in% wanted_replicates) %>% 
      filter(transfer %in% wanted_transfers) %>% 
      group_by(zone, strain, replicate, transfer) %>% 
      mutate(transfer_start=ifelse(dayn==min(dayn), paste("r", replicate, "-", transfer,  sep=""), "")) %>% 
      ungroup %>%
      mutate(replicate=paste("replicate", replicate)) %>% 
      mutate(transfer=paste("transfer", str_extract(transfer, regex("\\d")))) %>% 
      group_by(zone, strain, replicate, transfer)
    
    pp <- ggplot(data=dd, aes(x=dayn, y=!!wanted_y, colour=treatment)) +
      scale_color_viridis(end = 0.9, discrete = TRUE, option= "B") +
      geom_point(size=2.5, pch=21) + 
      # geom_text(aes(label=Fold_change), size=3, nudge_x = 0.5, color="black") +
      geom_line(size=0.6, aes(group=interaction(rep, transfer, treatment, strain))) +
      labs(y="Relative fluorescence", x="Time (days)")
    
    if (length(wanted_facets) == 1) {
      # facets <- as.formula(paste(".~", wanted_facets))
      facets <- wanted_facets
    }
    
    if (length(wanted_facets) == 2) {
      facets <- as.formula(paste(wanted_facets[1], "~", wanted_facets[2]))
    }
    
    pl <- pp + facet_wrap(facets, ncol=n_col) 
    
    if (input$toggle_log_scale) {  
      pl <- pl + scale_y_continuous(trans="log") 
    }
    
    if (input$toggle_free_y_scale) {
      pl <- pl + facet_wrap(facets, ncol=n_col, scales="free_y")
    }
    
    if (input$toggle_log_scale && input$toggle_free_y_scale) {
      pl <- pl + 
        scale_y_continuous(trans="log") +
        facet_wrap(facets, ncol=n_col, scales="free_y") 
    }
    pl <- pl + geom_label_repel(aes(label=transfer_start), color="black", data=filter(dd, treatment=="8 ppt", dayn==min(dayn)))
    # pl <- ggplotly(pl)
    return(pl)
  })
  
  output$growth_plot <- renderPlot({make_plot()})
  
  ##### --- slopes tab ----------------------------------------------- #####
  slope_plot <- eventReactive({
    input$plotGrowth }, {
      
      n_col <- input$n_facet_cols
      wanted_zones <- input$choose_zones
      wanted_treatments <- input$choose_treatments
      wanted_replicates <- input$choose_replicates
      wanted_transfers <- input$choose_transfer
      wanted_facets <- tolower(input$choose_facets)
      
      # filter
      slopes_filtered <- slopes %>% 
        filter(zone %in% wanted_zones) %>% 
        filter(treatment %in% wanted_treatments) %>% 
        filter(replicate %in% wanted_replicates) %>% 
        filter(transfer %in% wanted_transfers) %>% 
        mutate(replicate=paste("replicate", replicate)) %>% 
        mutate(transfer=paste("transfer", str_extract(transfer, regex("\\d"))))
      
      ss <- ggplot(slopes_filtered, aes(x=treatment, y=estimate, color=treatment, shape=replicate)) +
        geom_hline(aes(yintercept=0), color="red", linetype=2) +
        geom_point(alpha=.8, size=2, stroke=1.5) +
        scale_shape_manual(values=21:23) +
        scale_fill_viridis(end = 0.9, discrete = TRUE, option= "B") +
        scale_color_viridis(end = 0.9, discrete = TRUE, option= "B") +
        labs(y="Slope of log(RFU) by day", x="Salinity (ppt)")
      
      
      if (length(wanted_facets) == 1) { 
        # facets <- as.formula(paste(".~", wanted_facets))
        facets <- wanted_facets
      } 
      
      if (length(wanted_facets) == 2) {
        facets <- as.formula(paste(wanted_facets[1], "~", wanted_facets[2]))
      }
      
      sl <- ss + facet_wrap(facets, ncol=n_col)
      
      if (input$toggle_free_y_scale) {
        sl <- sl + facet_wrap(facets, ncol=n_col, scales="free_y")
      }
      # sl <- ggplotly(sl)
      return(sl)
    })
  
  output$slope_plot <- renderPlot({slope_plot()})

  ##### --- slopes mean sd tab --------------------------------------- #####
  mean_sd_plot <- eventReactive({
    input$plotGrowth }, {
    
      n_col <- input$n_facet_cols
      wanted_zones <- input$choose_zones
      wanted_treatments <- input$choose_treatments
      wanted_replicates <- input$choose_replicates
      wanted_transfers <- input$choose_transfer
      wanted_facets <- tolower(input$choose_facets)
      
    slopes_filtered <- slopes %>%
      filter(zone %in% wanted_zones) %>%
      filter(treatment %in% wanted_treatments) %>%
      filter(replicate %in% wanted_replicates) %>%
      filter(transfer %in% wanted_transfers) %>%
      ungroup %>%
      mutate(replicate=paste("replicate", replicate)) %>%
      mutate(transfer=paste("transfer", str_extract(transfer, regex("\\d"))))
    
    if (length(wanted_facets) == 1) {
      slopes_sum <- slopes_filtered %>% 
        group_by(zone, strain, treatment) %>% 
        summarise_at("estimate", funs(mean_slope=mean, sd_slope=sd)) 
    } else {
      if (wanted_facets[2] == "replicate") {
        slopes_sum <- slopes_filtered %>% 
          group_by(zone, strain, treatment, replicate) %>% 
          summarise_at("estimate", funs(mean_slope=mean, sd_slope=sd))
      } else if (wanted_facets[2] == "transfer") {
        slopes_sum <- slopes_filtered %>% 
          group_by(zone, strain, treatment, transfer) %>% 
          summarise_at("estimate", funs(mean_slope=mean, sd_slope=sd))
      } else {
        slopes_sum <- slopes_filtered %>% 
          group_by(zone, strain, treatment) %>% 
          summarise_at("estimate", funs(mean_slope=mean, sd_slope=sd))
      }
    }
    
    ms <- ggplot(slopes_sum, aes(x=treatment, y=mean_slope, color=treatment)) +
      geom_hline(aes(yintercept=0), color="red", linetype=2) +
      geom_point(alpha=.8, size=2, stroke=1.5) +
      # geom_crossbar(data=slopes_sum_filtered, aes(x=treatment, y=mean_slope, ymin=mean_slope-sd_slope, ymax=mean_slope+sd_slope, color=treatment), width=.4, alpha=.4) +
      geom_errorbar(data=slopes_sum, aes(x=treatment, y=mean_slope, ymin=mean_slope-sd_slope, ymax=mean_slope+sd_slope, color=treatment), width=.1) +
      # scale_fill_viridis(end = 0.9, discrete = TRUE, option= "B") +
      scale_color_viridis(end = 0.9, discrete = TRUE, option= "B") +
      labs(y="Slope of log(RFU) by day", x="Salinity (ppt)")
    
    if (length(wanted_facets) == 1) { 
      # facets <- as.formula(paste(".~", wanted_facets))
      facets <- wanted_facets
    } 
    
    if (length(wanted_facets) == 2) {
      facets <- as.formula(paste(wanted_facets[1], "~", wanted_facets[2]))
    }
    
    ml <- ms + facet_wrap(facets, ncol=n_col)
    
    if (input$toggle_free_y_scale) {
      ml <- ml + facet_wrap(facets, ncol=n_col, scales="free_y")
    }
    # ml <- ggplotly(ml)
    return(ml)
  })
  
  output$slope_mean_sd <- renderPlot({mean_sd_plot()})

##### --- transfer tab --------------------------------------------- #####
  
  transfer_sheets <- eventReactive(
    input$make_transfer,
    {
      start_rfu <- input$start_rfu %>% as.numeric()
      trans_plt <- input$which_plt %>% as.character()
      trans_trn <- input$which_trn %>% as.character()
      trans_val <- input$dilute_by %>% as.character()
      max_volum <- input$max_volum %>% as.numeric()
                                  
      ts <- make_transfer(
        experiment_data = dd,
        dilute_by = trans_val,
        which_plate = trans_plt,
        which_transfer = trans_trn,
        starting_rfu = start_rfu,
        max_volume = max_volum)
      return(ts)
      })
  
  # render a table tab for all selected plates
  output$plate_tabs <- renderUI({
    plate_transfer_combinations <- paste("plate:", unlist(input$which_plt), ", transfer:" , input$which_trn)
    ptc <- paste0(input$which_plt, input$which_trn, collapse = "_")
    tabs <- map2(plate_transfer_combinations, ptc, function(x, y) tabPanel(x, tags$br(), downloadButton(paste(y, "_d")), tableOutput(y) ))
    do.call(tabsetPanel, tabs)
    })
  
  output$mari_fill <- renderTable(transfer_sheets()[[1]], rownames = TRUE, striped = TRUE, spacing = "xs", colnames = FALSE)
  output$mari_transfer <- renderTable(transfer_sheets()[[2]], rownames = TRUE, striped = TRUE, spacing = "xs", colnames = FALSE)
  
  output$download_mari_fill <- downloadHandler(
    filename = function() {
      paste(input$which_plt, "-", input$which_trn, "-FILL-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(transfer_sheets()[[1]], file, col_names = FALSE)
    }
  )
  
  output$download_mari_transfer <- downloadHandler(
    filename = function() {
      paste(input$which_plt, "-", input$which_trn, "-TRANSFER-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(transfer_sheets()[[2]], file, col_names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

