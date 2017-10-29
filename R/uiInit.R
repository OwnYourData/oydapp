uiInit <- function(ns){
        tagList(
                rintrojs::introjsUI(),
                shinyjs::useShinyjs(),
                shinyStore::initStore(ns("store"), "oydStore"),
                tags$style(busyIndicatorCSS),
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
                        var url = window.location.href;
                        if(url.indexOf('PIA_URL=') == -1){
                        if(localStorage['oydStore\\\\pia_url'] === undefined) {
                        $('#oyd-startConfig').modal('show');
                        } else {
                        if(JSON.parse(localStorage['oydStore\\\\pia_url']).data === null) {
                        $('#oyd-startConfig').modal('show');
                        }
                        }
                        }
                        $('button:contains(\"Close\")').attr(\"id\", \"configDialogCloseBtn\");
                        $('.dropdown-menu').attr('class', 'dropdown-menu pull-right');
                        $('a').filter(function(index) { return $(this).text() === \"", appTitle, "\"; }).css('display', 'none');
                        $('a').filter(function(index) { return $(this).text() === \"hidden\"; }).css('display', 'none');
                        });")),
                tags$script(
                        'Shiny.addCustomMessageHandler("setPiaUrl", function(x) {
                                $("#returnPIAlink").attr("href", x);
                        })'
                ),
                tags$script(
                        'Shiny.addCustomMessageHandler("setTranslation", function(x) {
                                myArray = x.split(",");
                                $("#" + myArray[0]).html(myArray[1]);
                        })'
                ),
                tags$script(
                        'Shiny.addCustomMessageHandler("openUrlInNewTab", function(x) {
                                var win = window.open(x, "_blank");
                                win.focus();
                        })'
                ),
                tags$script(
                        'Shiny.addCustomMessageHandler("setDisplayButton", function(x) {
                                var id = "#oyd-" + x;
                                $("#oyd-buttonAnalysis").css("background-color", "#f5f5f5");
                                $("#oyd-buttonAnalysis").css("color", "black");
                                $("#oyd-buttonSource").css("background-color", "#f5f5f5");
                                $("#oyd-buttonSource").css("color", "black");
                                $("#oyd-buttonReport").css("background-color", "#f5f5f5");
                                $("#oyd-buttonReport").css("color", "black");
                                $(id).css("background-color", "#45b79e");
                                $(id).css("color", "white");
                        });'
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
                tags$head(
                        tags$style(HTML(".navbar .navbar-nav {float: right}"))
                ),
                tags$style(type="text/css", "body {padding-top: 70px;}")
        )
}
