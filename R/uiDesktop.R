# top-level framework for desktop version
# last update:2016-10-29

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

uiDesktop <- function(id){
        ns <- NS(id)
        tagList(
                # Code for initial "Wait"-Animation
                # tags$head(tags$script(src='http://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.2/modernizr.js')),
                tags$head(tags$link(rel='stylesheet', type='text/css', href='init_anim.css')),
                tags$div(class='init-animation'),
                uiInit(ns),
                uiConfigDialog(ns),
                navbarPage(
                        uiOutput(ns('hdrMyAppsLink')),
                        id=ns('mainPage'),
                        selected = appName,
                        position = 'fixed-top',
                        collapsible=TRUE,
                        inverse=FALSE,
                        fluid=FALSE,
                        windowTitle=paste0(appTitle,
                                           ' | OwnYourData'),
                        tabPanel(HTML(paste0('hidden',
                                             '</a></li>',
                                             '<li><a id="oyd-help" href="#" class="action-button shiny-bound-input"><i class="fa fa-question-circle" aria-hidden="true"></i> Hilfe'))),
                        tabPanel(appTitle,
                                 value = appName,
                                 shinyBS::bsAlert('piaStatus'),
                                 uiApp(ns)
                        ),
                        navbarMenu(icon('cog'),
                                   uiMenu(ns)),
                        footer=uiFooter(ns)
                )
        )
}
