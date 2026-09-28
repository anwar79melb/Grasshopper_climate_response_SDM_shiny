# load libraries
library(shiny)
library(ggplot2)


# read model file names
# model_dir <- "D:/PhD related/2nd chapter/final analysis 21Sep2026/ShinyGithub/maps" # for local run
model_dir <- "maps"

addResourcePath(
  "maps",
  normalizePath("maps")
)

model_files <- list.files(
  model_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)

print(model_files)

species_names <- gsub(
  "\\.rds$",
  "",
  basename(model_files)
)

print(species_names)

# User Interface (UI)
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    img {
      max-width: 100%;
      height: auto;
    }
  "))
  ),
  
  titlePanel(
    "Grasshopper Species Distribution Models"
  ),
  
  sidebarLayout(
    
    # sidebarPanel(
    #   
    #   selectInput(
    #     "species",
    #     "Select species:",
    #     choices = species_names
    #   )
    #   
    # ),
    sidebarPanel(
      
      selectInput(
        "species",
        "Select species:",
        choices = species_names
      ),
      
      uiOutput("stats"),
      
      hr(),
      
      h4("About this App"),
      
      p("This interactive application presents species distribution models",
        "for grasshoppers of Western Australia developed as part of a PhD research project."
      ),
      
      p(
        "Explore species-specific distribution maps based on long-term climate, short-term climate and static predictors."
      ),
      
      p(
        "Further details can be found in the associated ",
        tags$a(
          "PhD thesis",
          href = "https://minerva-access.unimelb.edu.au/items/d6602761-b9e9-4dc1-b52d-e0ceb9bae69a",
          target = "_blank"
        )
      ),
      
      p(
        tags$a(
          "Hosted on GitHub Repository",
          href = "https://github.com/anwar79melb",
          target = "_blank"
        )
      ),
      
      hr(),
      
      strong("Author:"),
      p("Authors: Md Anwar Hossain, MR Kearney, JJ Lahoz-Monfort"),
      
      strong("Institution:"),
      p("The University of Melbourne"),
      
      p(
        a(
          "Contact",
          href = "mailto:anwar.wildlife.du3@gmail.com"
        )
      )
      
    ),
    
    mainPanel(
      
      fluidRow(
        
        column(
          6,
          
          div(
            style = "margin-bottom:20px;",
            
            uiOutput(
              "sdm_LTC_plot",
              height = "450px"
            ),
            
            p(
              "Click image to open full-size map in a new tab",
              style = "font-size:12px; color:grey;"
            ),
            
            downloadButton(
              "download_sdm_LTC",
              "Download SDM Map (Long-term Climate and Static predictors)",
              width = "100%"
            )
          )
        ),
        
        column(
          6,
          
          div(
            style = "margin-bottom:20px;",
            
            uiOutput(
              "sdm_LTC_only_plot",
              height = "450px"
            ),
            
            p(
              "Click image to open full-size map in a new tab",
              style = "font-size:12px; color:grey;"
            ),
            
            downloadButton(
              "download_SDM_LTC_only",
              "Download SDM Map (Long-term Climate only (no Static predictors)",
              width = "100%"
            )
          )
        )
        
      ),
      
      fluidRow(
        
        column(
          6,
          
          div(
            style = "margin-bottom:20px;",
            
            uiOutput(
              "SDM_STC_1982_plot",
              height = "450px"
            ),
            
            p(
              "Click image to open full-size map in a new tab",
              style = "font-size:12px; color:grey;"
            ),
            
            downloadButton(
              "download_SDM_STC_1982",
              "Download SDM Map: Short-term Climate (dry year 1982) and Static predictors",
              width = "100%"
            )
          )
        ),
        
        column(
          6,
          
          div(
            style = "margin-bottom:20px;",
            
            uiOutput(
              "SDM_STC_1965_plot",
              height = "450px"
            ),
            
            p(
              "Click image to open full-size map in a new tab",
              style = "font-size:12px; color:grey;"
            ),
            
            downloadButton(
              "download_SDM_STC_1965",
              "Download SDM Map: Short-term climate (wet year 1965) and Static predictors",
              width = "100%"
            )
          )
        )
        
      ),
      
    )
    
  )
  
)


# server
server <- function(input, output, session){
  
  current_species <- reactive({
    
    readRDS(
      file.path(
        model_dir,
        paste0(input$species, ".rds")
      )
    )
    
  })
  
  # LTC map
  output$sdm_LTC_plot <- renderUI({
    
    tags$a(
      href = file.path("maps", current_species()$LTC_map),
      target = "_blank",
      
      tags$img(
        src = file.path("maps", current_species()$LTC_map),
        style = "width:100%; height:auto; border:1px solid #ddd;"
      )
    )
    
  })
  
  # LTC only map
  output$sdm_LTC_only_plot <- renderUI({
    
    tags$a(
      href = file.path("maps", current_species()$LTC_only_map),
      target = "_blank",
      
      tags$img(
        src = file.path("maps", current_species()$LTC_only_map),
        style = "width:100%; height:auto; border:1px solid #ddd;"
      )
    )
    
  })
  
  # Dry year 1982 map
  output$SDM_STC_1982_plot <- renderUI({
    
    tags$a(
      href = file.path("maps", current_species()$Dry1982_map),
      target = "_blank",
      
      tags$img(
        src = file.path("maps", current_species()$Dry1982_map),
        style = "width:100%; height:auto; border:1px solid #ddd;"
      )
    )
    
  })
  
  # Wet year 1965 map
  output$SDM_STC_1965_plot <- renderUI({
    
    tags$a(
      href = file.path("maps", current_species()$Wet1965_map),
      target = "_blank",
      
      tags$img(
        src = file.path("maps", current_species()$Wet1965_map),
        style = "width:100%; height:auto; border:1px solid #ddd;"
      )
    )
    
  })
  
  # Download LTC
  output$download_sdm_LTC <- downloadHandler(
    
    filename = function() {
      paste0(
        current_species()$species,
        "_SDM_Map_LTC_Static.png"
      )
    },
    
    content = function(file) {
      
      file.copy(
        file.path(
          "maps",
          current_species()$LTC_map
        ),
        file
      )
      
    }
    
  )
  
  # Download LTC only
  output$download_SDM_LTC_only <- downloadHandler(
    
    filename = function() {
      paste0(
        current_species()$species,
        "_SDM_LTC_only.png"
      )
    },
    
    content = function(file) {
      
      file.copy(
        file.path(
          "maps",
          current_species()$LTC_only_map
        ),
        file
      )
      
    }
    
  )
  
  # Download 1982
  output$download_SDM_STC_1982 <- downloadHandler(
    
    filename = function() {
      paste0(
        current_species()$species,
        "_SDM_STC_1982_dry_year.png"
      )
    },
    
    content = function(file) {
      
      file.copy(
        file.path(
          "maps",
          current_species()$Dry1982_map
        ),
        file
      )
      
    }
    
  )
  
  # Download 1965
  output$download_SDM_STC_1965 <- downloadHandler(
    
    filename = function() {
      paste0(
        current_species()$species,
        "_SDM_STC_1965_wet_year.png"
      )
    },
    
    content = function(file) {
      
      file.copy(
        file.path(
          "maps",
          current_species()$Wet1965_map
        ),
        file
      )
      
    }
    
  )
  
  # Species summary
  output$stats <- renderUI({
    
    x <- current_species()
    
    div(
      
      style = "
      background:#eef5ff;
      border:1px solid #d6e4ff;
      border-radius:8px;
      padding:8px;
      margin-bottom:5px;
      font-size:14px;
      line-height:1.3;
      ",
      
      h4(
        "Species Summary (Long-term Climate and Static predictors)",
        style = "margin-top:0px; margin-bottom:8px;"
      ),
      
      HTML(
        paste0(
          "<b>Species:</b> ", x$species, "<br>",
          "<b>Occurrences:</b> ", x$occurrences, "<br>",
          "<b>ROC-AUC:</b> ", round(x$auc_roc, 3), "<br>",
          "<b>PR-AUC:</b> ", round(x$auc_pr, 3)
        )
      )
      
    )
    
  })
  
}

# run app
shinyApp(ui, server)
