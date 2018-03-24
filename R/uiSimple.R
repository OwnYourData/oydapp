uiSimple <- function(id){
        ns <- NS(id)
        shiny::addResourcePath(
                prefix = 'img',
                directoryPath = system.file('img',
                                            package='oydapp'))
        tagList(
                # Code for initial "Wait"-Animation
                # tags$head(tags$script(src='http://cdnjs.cloudflare.com/ajax/libs/modernizr/2.8.2/modernizr.js')),
                # tags$head(tags$link(rel='stylesheet', type='text/css', href='init_anim.css')),
                # tags$div(class='init-animation'),
                # uiInit(ns),
                rintrojs::introjsUI(),
                shinyjs::useShinyjs(),
                shinyStore::initStore(ns("store"), "oydStore"),
                tags$script('setInterval(avoidIdle, 5000);
                            function avoidIdle(){
                            Shiny.onInputChange("myData", 0) }'
                ),
                tags$script(HTML('var userLang = navigator.languages && navigator.languages[0] ||
                                 navigator.language ||
                                 navigator.userLanguage;
                                 $(document).on("shiny:connected", function() {
                                 Shiny.onInputChange("oyd-userLang", userLang.substr(0,2));
                                 });')),
                tags$script(paste0(
                        # JavaScript to hide initial "Wait"-Animation
                        'Shiny.addCustomMessageHandler("finishInit", function(x) {
                        $(".init-animation").fadeOut("slow");
                        }); ',
                        "$(window).load(function(){
                        $('button:contains(\"Close\")').attr(\"id\", \"configDialogCloseBtn\");
                        });")),
                tags$script(
                        'Shiny.addCustomMessageHandler("setTranslation", function(x) {
                        myArray = x.split(",");
                        $("#" + myArray[0]).html(myArray[1]);
                        })'
                ),
                tags$script(
                        '$(window).on("shown.bs.modal", function() {
                        $("#shiny-modal").keyup(function(event) {
                        if (event.keyCode == 13) {
                        $("#oyd-decryptBtn").click();
                        $("#oyd-decryptConfigBtn").click();
                        }
                        });
                        });'
                ),
                uiConfigDialog(ns),
                fluidPage(
                        id=ns('mainPage'),
                        title=paste0(appTitle, ' | OwnYourData'),
                        titlePanel(span(tags$img(src='img/oydapp.png', style='width: 40px;'),
                                   span(
                                           actionLink(ns('uiSimpleShowConfig'),
                                                      '', icon = icon("cog")),
                                           style="float:right;"))),
                        shinyBS::bsAlert('piaStatus'),
                        uiBody(ns),
                        hr(),
                        span(span(tagList(span(uiOutput(ns('ctrlTrnsl_createdBy'), inline = TRUE),
                                               class = 'hidden-xs'),
                                  a("OwnYourData.eu", href='https://www.ownyourdata.eu')),
                                  style="float:right;"),
                             div(style='display:flex; width:200px; margin-top:-8px;',
                                 div(style='margin:8px;', uiOutput(ns('ctrlTrnsl_langLabel'))),
                                 selectInput(ns('lang'), label = NULL, width = '160px',
                                             choices = list("Deutsch" = 'de', "English" = 'en'),
                                             selected = 'de')))
                )
        )
}
