
#' UI to set WBM gemeente
#' @export
set_gemeente_menu <- function(){

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Stel de huidige gemeente in",
                           left = miniUI::miniTitleBarCancelButton(label = "Annuleren", primary = TRUE),
                           right = NULL),
    miniUI::miniContentPanel(

      shiny::uiOutput("ui_current_klant"),

      shiny::selectInput("sel_klant",  "Maak een keuze", choices = NULL),

      tags$p("Pas op: huidige this_version.yml wordt overschreven (zonder comments)"),
      shiny::actionButton("btn_set_klant", "Klant instellen",
                          icon = shiny::icon("check"),
                          class = "btn-success btn-lg")

    )
  )

  server <- function(input, output, session){

    shiny::observeEvent(input$cancel, {
      shiny::stopApp("Geannulleerd.")
    })

    output$ui_current_klant <- renderUI({
      tags$p(HTML(glue::glue("De huidige gemeente is <b>{get_gemeente()}</b>")),
             style = "font-size: 1.1em;")
    })

    observe({
      print(get_gemeente_choices())
      updateSelectInput(session, "sel_klant",
                         choices = sort(get_gemeente_choices()),
                         selected = get_gemeente())
    })


    observeEvent(input$btn_set_klant, {

      set_gemeente(input$sel_klant)
      shiny::stopApp(glue::glue("Nieuwe gemeente is {input$sel_klant}"))
    })

  }

  shiny::runGadget(ui, server,
                   viewer = shiny::dialogViewer(dialogName = "WBM - gemeente kiezen"),
                   stopOnCancel = FALSE)

}
