# Pakkar
library(shiny)
library(tidyverse)
library(lubridate)
library(reactable)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Viðmót sett upp
ui <- fluidPage(
  
  titlePanel("Skráarvinnsla orkusvið"),
  
  br(),
  
  fileInput("upload", "Hlaða upp skrá"),
  
  reactableOutput("preview"),
  
  br(),
  
  downloadButton("download", "Sækja .csv skrá")
)

# Bakendi
server <- function(input, output) {
  gogn <- reactive({
    req(input$upload)
    zip_skjal <- input$upload$datapath
    
    skjol <- unzip(zip_skjal, list = TRUE)[[1]]
    
    tafla <- map(
      skjol,
      \(x) read_delim(unz(zip_skjal, x), delim = ";", col_types = "cn") |> 
        mutate(Dagsetning = dmy_hm(Dagsetning) - hours(1), maelir = str_split_i(x, "-", 4)),
      .progress = "vinnur gögn"
    ) |> bind_rows() |> rename("dags" = 1, "notkun" = 2)
    
    tafla
  })
  
  output$preview <- renderReactable({
    reactable(
      head(gogn(), n = 10L),
      columns = list(
        dags = colDef(format = colFormat(datetime = TRUE, hour12 = FALSE))
      )
    )
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(str_split_i(input$upload, "\\.", 1), ".csv")
    },
    content = function(file) {
      write_csv(gogn(), file)
    }
  )
}

# Keyrsla á appi
shinyApp(ui = ui, server = server)