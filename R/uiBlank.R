uiBlank <- function(id){
        ns <- NS(id)
        tagList(
                # Code for initial "Wait"-Animation
                # tags$head(tags$script(src='http://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.2/modernizr.js')),
                #tags$head(tags$link(rel='stylesheet', type='text/css', href='init_anim.css')),
                #tags$div(class='init-animation'),
                #uiInit(ns),
                rintrojs::introjsUI(),
                shinyjs::useShinyjs(),
                shinyStore::initStore(ns("store"), "oydStore"),
                #uiConfigDialog(ns),
                fluidPage(
                        id=ns('mainPage'),
                        title=paste0(appTitle, ' | OwnYourData'),
                        shinyBS::bsAlert('piaStatus'),
                        uiBody(ns)
                )
        )
}
