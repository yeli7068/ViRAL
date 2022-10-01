library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
#library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(shinythemes)
library(slickR)
library(shinyalert)
library(shinydisconnect)
library(bslib)

china.red <- '#d7000f'
hermes.orange <- '#fabd03'
marrs.green <- '#34a853'
klein.blue <- '#4286f5'

#======
gene.blood.default <- "OASL"

gene.blood.id <- selectizeInput(inputId = "gene.blood.id", label=h4(strong(icon("dna"), "Search a Gene of Interest"), align='left', style='font-family:Georgia;color:#2C3E50'),
                                choices = NULL, selected = gene.blood.default, #OASL,
                                multiple = FALSE, width = 300,
                                options = list(
                                    placeholder = "e.g. OASL",
                                    server = TRUE, selectOnTab=TRUE
                                ))


single.infection.default <- 'IFV'
single.infection.list <- c("IFV", "RSV", "HRV", "SARS-CoV-2", "Seasonal CoVs")
single.infection.id <- selectizeInput(inputId = "single.infection.id", label=h5(strong('Step1: Select a Virus')),# h4(strong('miRNA'))
                                         choices = single.infection.list, selected = single.infection.default,
                                         multiple = FALSE, width = 300,
                                         options = list(placeholder = 'Select a virus of interest',
                                                        server = TRUE, selectOnTab=TRUE
                                         ))

single.infection.symptom.default <- "Symptomatic"
single.infection.symptom.list <- c("Symptomatic", "Asymptomatic")
single.infection.symptom.id <- selectizeInput(inputId = "single.infection.symptom.id", label=h5(strong('Step2: Symptomatic?')),# h4(strong('miRNA'))
                                       choices = single.infection.symptom.list, selected = single.infection.symptom.default,
                                       multiple = FALSE, width = 300,
                                       options = list(placeholder = 'Symptomatic or Asymptomatic',
                                                      server = TRUE, selectOnTab=TRUE
                                       ))

#co.infection.default <- "HRV.and.Seasonal.CoVs"
#co.infection.list <- c("HRV.and.Seasonal.CoVs", "IFV.and.Seasonal.CoVs")

# co.infection.default <- "HRV and Seasonal CoVs"
# 
# co.infection.list <- c("HRV and Seasonal CoVs", "IFV and Seasonal CoVs", "IFV and HRV")
# co.infection.id <- selectizeInput(inputId = "co.infection.id", label=h5(strong('Select the viral co-infection')),# h4(strong('miRNA'))
#                                   choices = co.infection.list,
#                                   selected = co.infection.default,
#                                   multiple = FALSE, width = 300,
#                                   options = list(placeholder = 'Co-infection',
#                                                  server = TRUE, selectOnTab=TRUE
#                                   ))
# single.infection.submit.id <- actionButton("single.infection.submit.id", "Submit", 
#                                            icon = icon("refresh"))
#=====

tab_home <- dashboardPage(
    dashboardHeader(disable = T), 
    dashboardSidebar(disable = T), 
    
    
    dashboardBody(
        fluidRow(
            box(
                title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
                width = 12,
                
                h1(strong("ViRAL"), align='center'),
                h4(strong("Virus Response AnaLysis"), align='center'),
                hr(),
                tags$hr(style="border-top: 2px solid #A9A9A9"),
                
                column(12,
                       h3(strong('The Cancer Genome Atlas (TCGA) miRNome')),

                       valueBox(value = tags$p(strong("33"), style = "font-size: 90%;"), color = 'aqua', width = 3,
                                subtitle = tags$p(strong("Cancer types"), style = "font-size: 160%;"),  icon = icon("dna fa-0.5x")),
                       # valueBox(value = '88', color = 'teal', width = 3,
                       #          subtitle = tags$p(strong("Studies"), style = "font-size: 200%;"), icon = icon("database")),
                       valueBox(value = tags$p(strong("10,554"), style = "font-size: 90%;"), color = 'aqua', width = 3,
                                subtitle = tags$p(strong("Samples"), style = "font-size: 160%;"),  icon = icon("user-circle"))
                )
                #
                # br(),
                #
                # column(12,
                #        h3(strong('Cancer Circulating miRNome')),
                #
                #        valueBox(value = tags$p(strong("31"), style = "font-size: 90%;"), color = 'teal', width = 3,
                #                 subtitle = tags$p(strong("Cancer types"), style = "font-size: 160%;"),  icon = icon("dna")),
                #        valueBox(value = tags$p(strong("40"), style = "font-size: 90%;"), color = 'teal', width = 3,
                #                 subtitle = tags$p(strong("Studies"), style = "font-size: 160%;"), icon = icon("database")),
                #        valueBox(value = tags$p(strong("28,633"), style = "font-size: 90%;"), color = 'teal', width = 3,
                #                 subtitle = tags$p(strong("Samples"), style = "font-size: 160%;"),  icon = icon("user-circle"))
                #
                # )
                
            ),
            # box(
            #     title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
            #     width = 9,
            #     
            #     column(12,
            #            h4(strong('The Cancer Genome Atlas (TCGA) miRNome'), style='font-family:Georgia;color:#2C3E50'),
            #            
            #            valueBox(value = tags$p(strong("33"), style = "font-size: 80%;"), color = 'aqua', width = 4,
            #                     subtitle = tags$p(strong("Cancer types"), style = "font-size: 150%;"), tags$i(class = "fa fa-dna", style="font-size: 60px")),
            #            # valueBox(value = '88', color = 'teal', width = 3,
            #            #          subtitle = tags$p(strong("Studies"), style = "font-size: 200%;"), icon = icon("database")),
            #            valueBox(value = tags$p(strong("10,554"), style = "font-size: 80%;"), color = 'aqua', width = 4,
            #                     subtitle = tags$p(strong("Samples"), style = "font-size: 150%;"), tags$i(class = "fa fa-user-circle", style="font-size: 65px"))
            #     ),
            #     
            #     br(),
            #     
            #     column(12,
            #            h4(strong('Circulating miRNome Profiles of Human Cancer'), style='font-family:Georgia;color:#2C3E50'),
            #            
            #            valueBox(value = tags$p(strong("32"), style = "font-size: 80%;"), color = 'teal', width = 4,
            #                     subtitle = tags$p(strong("Cancer types"), style = "font-size: 150%;"), tags$i(class = "fa fa-dna", style="font-size: 60px")),
            #            valueBox(value = tags$p(strong("40"), style = "font-size: 80%;"), color = 'teal', width = 4,
            #                     subtitle = tags$p(strong("Studies"), style = "font-size: 150%;"), tags$i(class = "fa fa-database", style="font-size: 60px")),
            #            valueBox(value = tags$p(strong("28,633"), style = "font-size: 80%;"), color = 'teal', width = 4,
            #                     subtitle = tags$p(strong("Samples"), style = "font-size: 150%;"), tags$i(class = "fa fa-user-circle", style="font-size: 65px"))
            #            
            #     )
            # )
        )
    )
)

tab_query <- dashboardPage(
    dashboardHeader(disable = T), 
    dashboardSidebar(disable = T), 
    
    
    dashboardBody(
        fluidRow(
            box(
                title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
                width = 12,
                
                column(width = 3,
                valueBox(value = tags$p(strong("ViRAL"), style = "font-size: 140%;"), color = 'aqua', width = 12,
                        subtitle = tags$p(strong("querying a gene of interest across multiple viral infections"), style = "font-size: 100%;"),  icon = icon("search"))       
                #h1(strong("ViRAL"), align='left')
                ),
                column(6, 
                       gene.blood.id
                       # tags$div(id='div_gene_blood',
                       #          class='gene.blood',
                       #          gene.blood.id)
                ),
 
                box(
                    title = 'News', solidHeader = TRUE, collapsible = FALSE, status = 'primary',
                    width = 3, height = '361px', # solidHeader=TRUE can remove the top boarder
                    #h5(strong("News")),
                    tags$p(strong("2022-09-29"),
                           style = "font-size: 100%;text-align:justify; color:orange;"),
                    tags$p("Source code of ViRAL is available at ",
                           style = "font-size: 100%; text-align: justify; display:inline;"),
                    tags$p(HTML("<a href='https://github.com/yeli7068/ViRAL' target='_blank'>https://github.com/yeli7068/ViRAL</a>"))
                )
            ),
            
            box(
                title = NULL, status = "success", solidHeader = FALSE, collapsible = FALSE,
                width = 12,
                tabsetPanel(id = 'query',
                            tabPanel(strong("Pan-Viral Infection"),
                                     br(),
                                     
                                     hr(),
                                     # Expression Boxplot
                                     column(6, align = 'center',
                                            h5("Gene Expression in Symptomatic cases v.s. Control samples", align = 'center'),
                                            h6("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                            br(),
                                            withSpinner(
                                                plotOutput('Pan.Symptomatic',width = "100%", height = 700), # 1100 * 500
                                                type = 3,
                                                color.background = klein.blue
                                            ),
                                            column(8),
                                            # column(8, align = "left",
                                            #        checkboxInput(inputId = "Pan_WholeBlood_facet", label = "multi-panel plots", value = FALSE)
                                            # ),
                                            column(4,
                                                   downloadButton(outputId='Pan.Symptomatic.boxplot.data.downbttn', label = "CSV"),
                                                   downloadButton(outputId='Pan.Symptomatic.boxplot.pdf.downbttn', label = "PDF")
                                            )
                                            
                                     ),
                                     column(6, align = 'center',
                                            h5("Gene Expression in Asymptomatic cases v.s. Control samples", align = 'center'),
                                            h6("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                            br(),
                                            withSpinner(
                                                plotOutput('Pan.Asymptomatic',width = "100%", height = 700), # 1100 * 500
                                                type = 3,
                                                color.background = klein.blue
                                            ),
                                            column(8),
                                            column(4,
                                                   downloadButton(outputId='Pan.Asymptomatic.boxplot.data.downbttn', label = "CSV"),
                                                   downloadButton(outputId='Pan.Asymptomatic.boxplot.pdf.downbttn', label = "PDF")
                                            )
                                            
                                     ),
                                     
                                     # seprator                                     
                                     column(12, align = 'center',
                                            tags$hr(style="border-top: 1px dashed #A9A9A9")
                                     ),
                                     
                                     # ROC Forest plot
                                     column(6, align = 'center',
                                            h5("ROC Analysis Between Symptomatic cases v.s. Control samples", align = 'center'),
                                            br(),
                                            withSpinner(plotOutput('Pan.Symptomatic.Rocplot.Forest',width = "108%", height = 750), # 1100 * 500
                                                        type = 3,
                                                        color.background = klein.blue
                                            ), 
                                            column(8),
                                            column(4,
                                                   downloadButton(outputId='Pan.Symptomatic.forestplot.data.downbttn', label = "CSV"),
                                                   downloadButton(outputId='Pan.Symptomatic.forestplot.pdf.downbttn', label = "PDF")
                                            )
                                     ),
                                     column(6, align = 'center',
                                            h5("ROC Analysis Between Asymptomatic cases v.s. Control samples", align = 'center'),
                                            br(),
                                            withSpinner(plotOutput('Pan.Asymptomatic.Rocplot.Forest',width = "108%", height = 750), # 1100 * 500
                                                        type = 3,
                                                        color.background = klein.blue
                                            ), 
                                            column(8),
                                            column(4,
                                                   downloadButton(outputId='Pan.Asymptomatic.forestplot.data.downbttn', label = "CSV"),
                                                   downloadButton(outputId='Pan.Asymptomatic.forestplot.pdf.downbttn', label = "PDF")
                                            )
                                     )
                            ),
                            
                            tabPanel(strong("Single-Viral Infection"),
                                     br(),
                                     column(3,
                                        single.infection.id
                                     ),
                                     column(3,
                                        single.infection.symptom.id
                                     ),
                                     # column(3,
                                     #    h5(strong('Step4: Submit?')),
                                     #    single.infection.submit.id
                                     # ),
                                     box(
                                         #title = 'Select a TCGA miRNome Dataset', status = "danger", solidHeader = TRUE, collapsible = FALSE,
                                         #title = tags$div(HTML('<b><i class="fa fa-chevron-circle-down" style = "color:black;"></i> Select a TCGA miRNome Dataset</b>')), status = "danger", solidHeader = TRUE, collapsible = FALSE,
                                         title = "Step3: Select a Dataset",status = "danger", solidHeader = TRUE, collapsible = FALSE,
                                         width = 12,
                                         DT::dataTableOutput("single.infection.datasets.print")
                                     ),
                                     
                                     hr(),
                                     # Expression Boxplot
                                     column(6, align = 'center',
                                            #h5("Gene Expression along with Progression (Source: Whole Blood)", align = 'center'),
                                            #h6("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                            br(),
                                            withSpinner(
                                                plotOutput('single.project.boxplot.p',width = "100%", height = 600), # 1100 * 500
                                                type = 3,
                                                color.background = klein.blue
                                            )
                                            # ,
                                            # column(8, align = "left",
                                            #        checkboxInput("Pan_WholeBlood_facet", "multi-panel plots", FALSE)
                                            #        ),
                                            # , column(4,
                                            #         downloadButton(outputId='Pan.blood.box.summ.downbttn.csv', label = "CSV"),
                                            #         #downloadButton(outputId='tcga.box.summ.downbttn.png', label = "PNG"),
                                            #         downloadButton(outputId='Pan.blood.box.summ.downbttn.pdf', label = "PDF")
                                            # )
                                     ),
                                     
                                     column(6, align = 'center',
                                            h5("ROC Analysis", align = 'center'),
                                            #                                            h6("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                            br(),
                                            withSpinner(
                                                plotOutput('single.project.rocplot.p',width = "100%", height = 600), # 1100 * 500
                                                type = 3,
                                                color.background = klein.blue
                                            )
                                            
                                     ),
                            ),
                            tabPanel(strong("Multiple-Viral Infection"),
                                     br(),
                                     # column(3,
                                     #        co.infection.id
                                     # ),
                                     box(
                                         title = "Select a Multiple Infection Dataset",status = "danger", solidHeader = TRUE, collapsible = FALSE,
                                         width = 12,
                                         DT::dataTableOutput("multiple.infection.datasets.print")
                                     ),
                                     hr(),
                                     # Expression Boxplot
                                     column(6, align = 'center',
                                            #h5("Gene Expression along with Progression (Source: Whole Blood)", align = 'center'),
                                            #h6("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                            br(),
                                            withSpinner(
                                                plotOutput('multiple.infection.boxplot.p',width = "100%", height = 600), # 1100 * 500
                                                type = 3,
                                                color.background = klein.blue
                                            )
                                            # ,
                                            # column(8, align = "left",
                                            #        checkboxInput("Pan_WholeBlood_facet", "multi-panel plots", FALSE)
                                            #        ),
                                            # , column(4,
                                            #         downloadButton(outputId='Pan.blood.box.summ.downbttn.csv', label = "CSV"),
                                            #         #downloadButton(outputId='tcga.box.summ.downbttn.png', label = "PNG"),
                                            #         downloadButton(outputId='Pan.blood.box.summ.downbttn.pdf', label = "PDF")
                                            # )
                                     ),
                                     
                                     column(6, align = 'center',
                                            h5("ROC Analysis", align = 'center'),
                                            #                                            h6("(Wilcoxon rank-sum test, ***: P < 0.001; **: P < 0.01; *: P < 0.05; ns: P > 0.05)", align = 'center'),
                                            br(),
                                            withSpinner(
                                                plotOutput('multiple.infection.rocplot.p',width = "100%", height = 600), # 1100 * 500
                                                type = 3,
                                                color.background = klein.blue
                                            )
                                            
                                     ),
                            )
                )       
            )
        )
    )
)

ui <- fluidPage(#theme = shinytheme('cerulean'),
    navbarPage(
        title = NULL,
        id = 'navbar',
        windowTitle = "ViRAL",
        
        #tags$script(HTML("$('body').addClass('fixed');")), # fix header & sidebar
        
        #theme = shinytheme("flatly"),
        
        # tabPanel('Home', value = 'home', tab_home, icon=icon('home')), #,'fa-2x'
        tabPanel('Query', value = 'query', tab_query, icon = icon('search'))
        # tabPanel("TCGA miRNome", tab_tcga, icon = icon('database')),
        # tabPanel("Circulating miRNome", tab_circulating, icon = icon('database')),
        # tabPanel("Download", tab_download, icon = icon('download')),
        # tabPanel("Tutorial", tab_tutorial, icon = icon('file-alt'))#,
        # 
        # # tags$style(type = 'text/css', href = 'bootstrap.css') 
        # # tags$style(type = 'text/css', '.navbar-nav {padding-left: 400px; font-size: 24px;}',
        # #            '.navbar-default {margin-left: 2px;margin-right: 18px;margin-top: -2px;}'
    )
)

shinyUI(ui)
