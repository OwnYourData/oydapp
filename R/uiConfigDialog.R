# dialog with assisstant to configure connection to PIA and other items
# last update:2016-10-29

uiConfigDialog <- function(ns){
        shinyBS::bsModal(ns('startConfig'),
                         uiOutput(ns('ctrlTrnsl_configDialogTitle')),
                         'launchDialog', size='large',
                shinyBS::bsCollapse(id = ns('collapse'), open = "Willkommen",
                        shinyBS::bsCollapsePanel(
                                uiOutput(ns('ctrlTrnsl_configDialogStep1')),
                                value="Willkommen",
                                uiOutput(ns('ctrlTrnsl_configDialogStep1Text')),
                                br(), br(),
                                actionButton(ns('p1next'),
                                             uiOutput(ns('ctrlTrnsl_configDialogStep1BtnNext'))),
                                style = "primary"),
                        shinyBS::bsCollapsePanel(
                                uiOutput(ns('ctrlTrnsl_configDialogStep2')),
                                value="PIA",
                                uiOutput(ns('ctrlTrnsl_configDialogStep2Text')),
                                br(),
                                fluidRow(column(6,
                                        textInput(ns('modalPiaUrl'),
                                                  uiOutput(ns('ctrlTrnsl_configDialogStep2UrlInput'))),
                                        textInput(ns('modalPiaId'),
                                                  uiOutput(ns('ctrlTrnsl_configDialogStep2IdInput'))),
                                        textInput(ns('modalPiaSecret'),
                                                  uiOutput(ns('ctrlTrnsl_configDialogStep2SecretInput')))),
                                     column(6,
                                        uiOutput(ns('currentToken')),
                                        conditionalPanel(
                                                condition = "output['oyd-currentToken'] != ''",
                                                actionButton(ns('disconnectPIA'),
                                                             uiOutput(ns('ctrlTrnsl_configDialogStep2disconnectPiaBtn'),
                                                                      style='display: inherit;'),
                                                             icon('chain-broken'))),
                                        br(),
                                        uiOutput(ns('connectError')))),
                                hr(),
                                tags$div(
                                        actionButton(ns('p2prev'),
                                                     uiOutput(ns('ctrlTrnsl_configDialogStep2BtnBack')),
                                                     style = 'display: inline;'),
                                        withBusyIndicatorUI(
                                                actionButton(ns('p2next'),
                                                             uiOutput(ns('ctrlTrnsl_configDialogStep2BtnNext')),
                                                             style = 'display: inline;')),
                                        actionButton(ns('p2skip'),
                                                     uiOutput(ns('ctrlTrnsl_configDialogStep2BtnSkip')),
                                                     style = 'display: inline;')),
                                style = "info"),
                        shinyBS::bsCollapsePanel(
                                uiOutput(ns('ctrlTrnsl_configDialogStep3')),
                                value = "Keys",
                                uiOutput(ns('ctrlTrnsl_configDialogStep3Text')),
                                br(),
                                fluidRow(column(5,
                                                selectInput(
                                                        ns('keyList'),
                                                        uiOutput(ns('ctrlTrnsl_configDialogStep3KeyList')),
                                                        list(),
                                                        multiple=TRUE,
                                                        selectize=FALSE,
                                                        size=14),
                                                actionButton(ns('delKeyList'),
                                                             # uiOutput(ns('ctrlTrnsl_configDialogStep3DeleteKey')),
                                                             icon('trash')),
                                                actionButton(ns('importKeyList'),
                                                             uiOutput(ns('ctrlTrnsl_configDialogStep3ImportBtn'),
                                                                      style='display: inherit;'),
                                                             icon('download')),
                                                actionButton(ns('exportKeyList'),
                                                             uiOutput(ns('ctrlTrnsl_configDialogStep3ExportBtn'),
                                                                      style='display: inherit;'),
                                                             icon('upload'))),
                                         column(7,
                                                textInput(
                                                        ns('keyTitle'),
                                                        uiOutput(ns('ctrlTrnsl_configDialogStep3KeyTitle')),
                                                        value = ''),
                                                textInput(
                                                        ns('keyRepo'),
                                                        uiOutput(ns('ctrlTrnsl_configDialogStep3KeyRepo')),
                                                        value = ''),
                                                div(textInput(
                                                        ns('keyKeystr'),
                                                        uiOutput(ns('ctrlTrnsl_configDialogStep3KeyKeystr')),
                                                        value = ''),
                                                    actionButton(
                                                        ns('keyGenerate'),
                                                        icon('edit'),
                                                        style="height: fit-content; align-self: center; margin-bottom: -9px; margin-left: 10px;"),
                                                    actionButton(
                                                            ns('keyRandom'),
                                                            icon('random'),
                                                            style="height: fit-content; align-self: center; margin-bottom: -9px; margin-left: 10px;"),
                                                    style='display:flex;'),
                                                selectInput(
                                                        ns('keyType'),
                                                        uiOutput(ns('ctrlTrnsl_configDialogStep3KeyType')),
                                                        choices = NULL,
                                                        selected = 1),
                                                br(),
                                                actionButton(
                                                        ns('addKeyItem'),
                                                        uiOutput(ns('ctrlTrnsl_configDialogStep3KeyAdd'),
                                                                 style='display: inherit;'),
                                                        icon('plus')),
                                                actionButton(
                                                        ns('updateKeyItem'),
                                                        uiOutput(ns('ctrlTrnsl_configDialogStep3KeyUpdate'),
                                                                 style='display: inherit;'),
                                                        icon('edit')),
                                                actionButton(
                                                        ns('resetKeyItemForm'),
                                                        uiOutput(ns('ctrlTrnsl_configDialogStep3KeyResetForm'),
                                                                 style='display: inherit;'),
                                                        icon('undo')))
                                ),
                                hr(),
                                tags$div(
                                        actionButton(ns('p3prev'),
                                                     uiOutput(ns('ctrlTrnsl_configDialogStep3BtnBack'),
                                                              style='display: inherit;')),
                                        withBusyIndicatorUI(
                                                actionButton(ns('p3next'),
                                                             uiOutput(ns('ctrlTrnsl_configDialogStep3BtnNext'),
                                                                      style='display: inherit;')))),
                                style = "info"),
                        shinyBS::bsCollapsePanel(
                                uiOutput(ns('ctrlTrnsl_configDialogStep4')),
                                value="Fertig",
                                uiOutput(ns('ctrlTrnsl_configDialogStep4Text')),
                                br(), br(),
                                actionButton(ns('p4prev'),
                                             uiOutput(ns('ctrlTrnsl_configDialogStep4BtnBack')),
                                             style = 'display: inline;'),
                                actionButton(ns('p4close'),
                                             uiOutput(ns('ctrlTrnsl_configDialogStep4BtnFinish')),
                                             style = 'display: inline;'),
                                style = "info")
                )
        )
}
