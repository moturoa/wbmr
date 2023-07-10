
#' UI to set WBM tenant
#' @export
set_tenant_menu <- function(){

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Juno : switch de tenant",
                           left = miniUI::miniTitleBarCancelButton(label = "Annuleren", primary = TRUE),
                           right = NULL),
    miniUI::miniContentPanel(

      shiny::uiOutput("ui_current_klant"),

      shiny::selectInput("sel_klant",  "Maak een keuze", choices = NULL),

      shiny::tags$p("Pas op: huidige this_version.yml wordt overschreven (zonder comments)"),
      shiny::actionButton("btn_set_klant", "Tenant instellen",
                          icon = shiny::icon("check"),
                          class = "btn-success btn-lg")

    )
  )

  server <- function(input, output, session){

    shiny::observeEvent(input$cancel, {
      shiny::stopApp("Geannulleerd.")
    })

    output$ui_current_klant <- renderUI({
      shiny::tags$p(HTML(glue::glue("De huidige tenant is <b>{get_tenant()}</b>")),
             style = "font-size: 1.1em;")
    })

    observe({
      chc <- get_tenant_choices()
      
      updateSelectInput(session, "sel_klant",
                         choices = sort(setNames(chc, paste0(chc, " (", label_tenant(chc), ")"))),
                         selected = get_tenant())
    })


    observeEvent(input$btn_set_klant, {

      set_tenant(input$sel_klant)
      shiny::stopApp()
    })

  }

  shiny::runGadget(ui, server,
                   viewer = shiny::dialogViewer(dialogName = "WBM - tenant kiezen"),
                   stopOnCancel = FALSE)

}
