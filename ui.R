library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("Hello SHEM !"),
  navbarPage(title = "Onglets",
             tabPanel("Import des donnees",
                      sidebarLayout(
                        sidebarPanel(title = "Titre du panel",
                                     fileInput(inputId = "fichier_data",
                                               label = "Chercher le fichier en local",
                                               multiple = FALSE,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv"),
                                     )
                                     # ,selectInput(inputId = "fichier_github",
                                     #             label = "Chercher le fichier sur github",
                                     #             choices = list("prod_shem_2017_2018.csv" = 1),
                                     #             selected = 1
                                     # ),
                                     # actionButton(inputId = "action_github",
                                     #              label = "Download")
                        ),
                        mainPanel(
                          strong("Se rendre dans Tableaux de prod et valo pour la visualisation complete"),
                          br(),
                          br(),
                          textOutput("dim_table"),
                          br(),
                          textOutput("apercu_debut"),
                          br(),
                          DT::dataTableOutput("table_output_head"),
                          br(),
                          br(),
                          textOutput("apercu_fin"),
                          br(),
                          DT::dataTableOutput("table_output_tail")
                        )
                      )
             ),
             tabPanel("Tableaux de prod et valo",
                      sidebarLayout(
                        sidebarPanel(title = "Options de filtrage",
                                     dateRangeInput(inputId = "debut_fin",
                                                    label = "Periode",
                                                    start = "2017-01-01",
                                                    end = "2018-12-31",
                                                    min = "2012-01-01",
                                                    max = "2019-03-22",
                                                    format = "dd-mm-yyyy"),
                                     checkboxGroupInput(inputId = "selec_prix",
                                                        label = "Afficher les prix",
                                                        choiceNames = c("Prix spot",
                                                                        "Prix OA"),
                                                        choiceValues = c("prix_spot",
                                                                         "prix_OA"),
                                                        selected = "prix_spot"
                                     ),
                                     radioButtons(inputId = "granu",
                                                  label = "Granularite",
                                                  choices = c("Heure",
                                                              "Jour",
                                                              "Semaine",
                                                              "Mois",
                                                              "Annee"),
                                                  selected = "Heure"
                                     ),
                                     conditionalPanel(condition = "input.granu == 'Annee'",
                                                      checkboxInput(inputId = "tranche", 
                                                                    label = "Tranche tarifaire (uncheck after use)")
                                     ),
                                     radioButtons(inputId = "agreg",
                                                  label = "Agregation temporelle",
                                                  choices = c("Annee",
                                                              "Historique"),
                                                  selected = "Annee"
                                     ),
                                     radioButtons(inputId = "puissance_ou_energie",
                                                  label = "Mode de calcul prod et valo",
                                                  choices = c("Puissance (MW) - Valo horaire moyenne (€) - Prix moyens (€/MWh)" = "puissance",
                                                              "Energie (MWh) - Valo cumulee (€)" = "energie"),
                                                  selected = "puissance"
                                     ),
                                     radioButtons(inputId = "usine_ou_gpmt",
                                                  label = "Donnees par usine ou par groupement",
                                                  choices = c("Usine" = "usine",
                                                              "Groupement" = "gpmt"),
                                                  selected = "usine"
                                     ),
                                     checkboxGroupInput(inputId = "selec_gpmt",
                                                        label = "Selection par groupement",
                                                        choiceNames = c("Artouste",
                                                                        "Hourat",
                                                                        "Valentin",
                                                                        "Mareges",
                                                                        "Louron",
                                                                        "Eget"),
                                                        choiceValues = c("Artouste",
                                                                         "Hourat",
                                                                         "Valentin",
                                                                         "Mareges",
                                                                         "Louron",
                                                                         "Eget"),
                                                        selected = list("Artouste", "Hourat", "Valentin", "Mareges", "Louron", "Eget")
                                     ),
                                     checkboxGroupInput(inputId = "selec_tarif",
                                                        label = "Selection par tarification",
                                                        choiceNames = c("Spot",
                                                                        "OA 1C",
                                                                        "OA 2C",
                                                                        "OA 4C",
                                                                        "OA 5C"),
                                                        choiceValues = c("Prix_spot",
                                                                         "Prix_OA1C",
                                                                         "Prix_OA2C",
                                                                         "Prix_OA4C",
                                                                         "Prix_OA5C"),
                                                        selected = list("Prix_spot", "Prix_OA1C", "Prix_OA2C", "Prix_OA4C", "Prix_OA5C")
                                     )),
                        mainPanel(
                          navbarPage(
                            tabPanel("Juste pour faire un decalage, on dirait un bug..."
                            ),
                            tabPanel("Production",
                                     DT::dataTableOutput("datatable_output_prod")
                            ),
                            tabPanel("Valorisation",
                                     DT::dataTableOutput("datatable_output_valo")
                            )
                          )
                        )
                      )
             ),
             # tabPanel("Donnees de valorisation",
             #          sidebarLayout(
             #            sidebarPanel(
             #              radioButtons(inputId = "granu_valo",
             #                           label = "Granularite",
             #                           choices = c("Heure" = "granu_h",
             #                                       "Jour" = "granu_d",
             #                                       "Semaine" = "granu_w",
             #                                       "Mois" = "granu_m",
             #                                       "Annee" = "granu_y"),
             #                           selected = "granu_h"
             #              )
             #            ),
             #            mainPanel("Tableau des valorisations",
             #                      DT::dataTableOutput("datatable_output_valo")
             #            )
             #          )
             # ),
             tabPanel("Graphiques")
  )
)