# Pakkar
library(shiny)
library(tidyverse)
library(lubridate)
library(reactable)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(qs)

birgdir <- qread("birgdir.qs")

# Viðmót sett upp
ui <- fluidPage(
  
  useShinyjs(),
  # useWaiter(),
  # useShinyFeedback(),
  
  titlePanel("Birgðabókhald"),
  
  br(),
  
  div(
    id = "bokhaldsLinur",
    fluidRow(
      id = "start_lina",
      column(
        3,
        selectizeInput(
          "tegund_1",
          label = "Tegund",
          choices = unique(birgdir$tegund),
          selected = NULL,
          width = "100%",
          options = list(
            create = TRUE,
            placeholder = "Tegund",
            items = NULL,
            createOnBlur = TRUE
          )
        )
      ),
      column(
        3,
        selectizeInput(
          "vara_1",
          label = "Vara",
          choices = unique(birgdir$vara),
          width = "100%",
          options = list(
            create = TRUE,
            placeholder = "Vara",
            items = NULL,
            createOnBlur = TRUE
          )
        )
      ),
      column(
        2,
        numericInput(
          "magn_1",
          label = "Magn",
          value = 0,
          width = "100%"
        )
      ),
      column(
        2,
        selectizeInput(
          "eining_1",
          label = "Eining",
          choices = unique(birgdir$eining),
          width = "100%",
          options = list(
            create = TRUE,
            placeholder = "Eining",
            items = NULL,
            createOnBlur = TRUE
          )
        )
      ),
      column(
        2,
        selectizeInput(
          "stadsetning_1",
          label = "Staðsetning",
          choices = unique(birgdir$stadsetning),
          width = "100%",
          options = list(
            create = TRUE,
            placeholder = "Staðsetning",
            items = NULL,
            createOnBlur = TRUE
          )
        )
      )
    )
  ),
  
  fluidRow(
    column(
      3
    ),
    column(
      3,
      actionButton(
        "nyLina",
        "Bæta við línu",
        width = "100%",
        class = "btn btn-sm"
      )
    ),
    column(
      3,
      actionButton(
        "boka",
        "Bóka",
        width = "100%",
        class = "btn btn-sm"
      )
    ),
    column(
      3
    )
  ),
  
  br(),
  
  reactableOutput("hreyfingar"),
  
  br(),
  
  reactableOutput("stada")
  
)

# Bakendi
server <- function(input, output) {
  
  birgdir_df <- reactiveValues(birgdir = birgdir)
  
  fjoldiLina <- reactiveVal(1)
  
  observeEvent(input$nyLina, {
    if (input[[paste0("magn_", fjoldiLina())]] == 0) {
    
    } else {
    nyttId <- fjoldiLina() + 1
    fjoldiLina(nyttId)
    
    insertUI(
      selector = "#bokhaldsLinur",
      where = "beforeEnd",
      ui = fluidRow(
        id = paste0("bokhaldsLina_", nyttId),
        column(
          3,
          selectizeInput(
            paste0("tegund_", nyttId),
            label = "",
            choices = unique(birgdir_df$birgdir$tegund),
            selected = NULL,
            width = "100%",
            options = list(
              create = TRUE,
              placeholder = "Tegund",
              items = NULL,
              createOnBlur = TRUE
            )
          )
        ),
        column(
          3,
          selectizeInput(
            paste0("vara_", nyttId),
            label = "",
            choices = unique(birgdir_df$birgdir$vara),
            width = "100%",
            options = list(
              create = TRUE,
              placeholder = "Vara",
              items = NULL,
              createOnBlur = TRUE
            )
          )
        ),
        column(
          2,
          numericInput(
            paste0("magn_", nyttId),
            label = "",
            value = 0,
            width = "100%"
          )
        ),
        column(
          2,
          selectizeInput(
            paste0("eining_", nyttId),
            label = "",
            choices = unique(birgdir_df$birgdir$eining),
            width = "100%",
            options = list(
              create = TRUE,
              placeholder = "Eining",
              items = NULL,
              createOnBlur = TRUE
            )
          )
        ),
        column(
          2,
          selectizeInput(
            paste0("stadsetning_", nyttId),
            label = "",
            choices = unique(birgdir_df$birgdir$stadsetning),
            width = "100%",
            options = list(
              create = TRUE,
              placeholder = "Staðsetning",
              items = NULL,
              createOnBlur = TRUE
            )
          ),
        )
      )
    )
  }})
  
  observeEvent(input$boka, {
    birgdir_df$birgdir <- map(1:fjoldiLina(), function(x){
      tibble(
        tegund = input[[paste0("tegund_", x)]],
        vara = input[[paste0("vara_", x)]],
        magn = input[[paste0("magn_", x)]],
        eining = input[[paste0("eining_", x)]],
        dags = today(),
        stadsetning = input[[paste0("stadsetning_", x)]]
      )
    }) |> bind_rows(a = birgdir_df$birgdir, b = _)
    
    map(1:fjoldiLina(), function(y){
      updateSelectizeInput(inputId = paste0("tegund_", y), selected = NULL, choices = unique(birgdir_df$birgdir$tegund))
      updateSelectizeInput(inputId = paste0("vara_", y), selected = NULL, choices = unique(birgdir_df$birgdir$vara))
      updateNumericInput(inputId = paste0("magn_", y), value = 0)
      updateSelectizeInput(inputId = paste0("eining_", y), selected = NULL, choices = unique(birgdir_df$birgdir$eining))
      updateSelectizeInput(inputId = paste0("stadsetning_", y), selected = NULL, choices = unique(birgdir_df$birgdir$stadsetning))
    })
    
    removeUI(selector = "[id^=bokhaldsLina]", multiple = TRUE)
    fjoldiLina(1)
    
    qsave(birgdir_df$birgdir, "birgdir.qs")
  })
  
  output$hreyfingar <- renderReactable(
    reactable(
      birgdir_df$birgdir,
      columns = list(
        dags = colDef(format = colFormat(date = TRUE))
      )
    )
  )
  
  output$stada <- renderReactable(
    reactable(
      birgdir_df$birgdir |> 
        mutate(hreyf = if_else(magn < 0, 0, 1)) |> 
        arrange(hreyf) |> 
        group_by(tegund, vara) |> 
        mutate(uppsafnad = cumsum(magn)) |> 
        filter(uppsafnad > 0) |> 
        summarise(dags = min(dags), magn = last(uppsafnad)),
      groupBy = "tegund",
      columns = list(
        dags = colDef(aggregate = JS(
          "function(values) {
              const dates = values.map(v => new Date(v));
              const minDate = new Date(Math.min(...dates));
              const maxDate = new Date(Math.max(...dates));
              const formatDate = date => date.toLocaleDateString('en-GB');
              return `${formatDate(minDate)} - ${formatDate(maxDate)}`;
            }")
          # format = colFormat(date = T)
        ),
        vara = colDef(aggregate = "count"),
        magn = colDef(aggregate = "sum")
      )
    )
  )
  
  # observe(
  #   map(c("tegund", "vara", "eining", "stadsetning"), function(x) {
  #     updateSelectizeInput(inputId = paste0(c, "_1"), choices = eval(parse(text = paste0("birgdir_df$birgdir$", x))), server = T)
  #   })
  # )
  # map(c("tegund", "vara", "eining", "stadsetning"), function(x) {
  #   updateSelectizeInput(inputId = paste0(c, "_1"), choices = eval(parse(text = paste0("birgdir_df$birgdir$", x))), server = T)
  # })
  # 
  # updateSelectizeInput(inputId = "tegund_1", )
  
  # gogn <- reactive({
  #   req(input$upload)
  #   zip_skjal <- input$upload$datapath
  #   
  #   skjol <- unzip(zip_skjal, list = TRUE)[[1]]
  #   
  #   tafla <- map(
  #     skjol,
  #     \(x) read_delim(unz(zip_skjal, x), delim = ";", col_types = "cn") |> 
  #       mutate(Dagsetning = dmy_hm(Dagsetning) - hours(1), maelir = str_split_i(x, "-", 4)),
  #     .progress = "vinnur gögn"
  #   ) |> bind_rows() |> rename("dags" = 1, "notkun" = 2)
  #   
  #   tafla
  # })
  # 
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste0(str_split_i(input$upload, "\\.", 1), ".csv")
  #   },
  #   content = function(file) {
  #     write_csv(gogn(), file)
  #   }
  # )
}

# Keyrsla á appi
shinyApp(ui = ui, server = server)