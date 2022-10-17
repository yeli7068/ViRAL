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
gene.blood.default <- "RBBP6"

gene.blood.id <- selectizeInput(inputId = "gene.blood.id", label=h4(strong(icon("dna"), "Search a Gene of Interest"), align='left', style='font-family:Georgia;color:#2C3E50'),
                                choices = NULL, selected = gene.blood.default, #RBBP6,
                                multiple = FALSE, width = 300,
                                options = list(
                                    placeholder = "e.g. RBBP6",
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
                column(2, 
                       gene.blood.id,
                       #h5(strong(uiOutput("gene.annos")))
                       # tags$div(id='div_gene_blood',
                       #          class='gene.blood',
                       #          gene.blood.id)
                ),
                column(4, 
                       #gene.blood.id,
                       h4(strong(textOutput("Gene.EnsemblID"))),
                       h4(strong(textOutput("Gene.Alias"))),
                       h4(strong(textOutput("Gene.Description"))),
                       h4(strong(uiOutput("gene.annos")))
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
                                        single.infection.id,
                                        helpText("IFV: Influenza Virus", br(),
                                                 "RSV: Respiratory Syncytial Virus", br(),
                                                 "HRV: Human Rhinovirus", br(),
                                                 "SARS-CoV-2: Severe Acute Respiratory Syndrome Coronavirus 2", br(),
                                                 "Seasnal CoVs: Seasonal Coronavirus including HKU-1, OC43, etc.")
                                     ),
                                     column(3,
                                        single.infection.symptom.id,
                                        helpText("Re-standardized severity:", br(),
                                                 "Symptomatic: symptomatic individuals with the viral infection.", br(),
                                                 "Asymptomatic: individuals who tested positive for a virus but presented no symptoms", br(),
                                                 "Controls: uninfected healthy individuals")
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

tab_tutorial <- dashboardPage(
    dashboardHeader(disable = T), 
    dashboardSidebar(disable = T), 
    
    
    dashboardBody(
        fluidRow(
            box(
                title = NULL, status = "primary", solidHeader = FALSE, collapsible = FALSE,
                width = 12,
                h4(strong('Tutorial'), align='center', style='font-family:Georgia;color:#2C3E50')
            ),
            
            box(
                title = NULL, status = "primary", solidHeader = TRUE, collapsible = FALSE,
                width = 12,
                
                navlistPanel(id = 'pipeline', widths = c(3, 9),
                             
                             tabPanel(
                                 strong("1. Introduction"),
                                 box(
                                     title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
                                     width = 12,
                                     
                                     h4(strong('Introduction'), align='center'),
                                     br(),
                                     
                                     tags$p("ViRAL is a database for facilitating the use of publicly available transcriptomics in the field of host responses
                                     to virus infections. It has integrated 1,489 blood samples from 
                                     both asymptomatic and symptomatic individuals infected with one of the following viruses: SARS-CoV-2, 
                                     seasonal coronavirus (Seasonal CoVs), influenza virus (IFV), respiratory syncytial virus (RSV), or 
                                     human rhinovirus (HRV) from 10 independent cohorts across continents of Asia, Europe, and North America.
                                     A suite of advanced functions is provided to understand a gene of interest across multiple viral infections.", 
                                            style = "font-size: 100%; text-align: justify;"),
                                     
                                     br(),
                                     
                                     tags$p("When querying a gene of interest, by default, the results will be automatically 
                                            generated for : (i) pan-respiratory-virus differential expression (DE) analysis to determine 
                                            if the gene of interest is differentially expressed between infected and un-infected 
                                            samples presenting symptoms or not, and ROC analysis is performed to measure the 
                                            performance of the gene biomarker in distinguishing infected samples from control samples 
                                            with a forest plot, (ii) The DE and ROC analyses can also be implemented for a selected 
                                            single or multiple viral infection project to show more detailed information about the 
                                            dynamics of the gene and its associated diagnostic power for a viral infection type of interest.", 
                                            style = "font-size: 100%; text-align: justify;"),
                                     
                                     br(),
                                     
                                     tags$p("By using ViRAL, experimental biologists can easily ask specific questions and test their hypotheses. 
                                            For example, one can easily find the gene RBBP6 specifically expressed in SARS-CoV-2 infection 
                                            regardless of symptom (Figure 1).", 
                                            style = "font-size: 100%; text-align: justify;"),
                                     
                                     column(3),
                                     tags$img(src='img/overview.jpg', width=800),
                                     h5(strong('Figure 1. Overview of the ViRAL.'), align='center')

                                 )
                                     
                                      
                                      
                             ),
                         tabPanel(
                             strong("2. Query a Gene of Interest"),
                             box(
                                 title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
                                 width = 12,
                                 
                                 h4(strong('Query a Gene of Interest'), align='center'),
                                 br(),
                                 
                                 tags$p("Users can query a gene of interest by typing the gene symbol in 
                                   the 'Search a Gene' field and selecting the gene from the dropdown 
                                   list. In addition to the general information including Ensembl ID, Alias, and Description 
                                   of the queried gene, links to several public databases including 
                                   ENSEMBL, NCBI, GeneCards, DrugBank, and 
                                   EBI are also provided.", style = "font-size: 100%; text-align: justify;"),
                                 column(3),
                                 tags$img(src='img/query_a_gene.jpg', width=800),
                                 h6(strong('Figure 1. Overview of UI for Querying a Gene of Interest'), align='center')
                                 
                             )
                         ),
                         tabPanel(
                             strong("3. Pan-Viral Infection"),
                             box(
                                 title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
                                 width = 12,
                                 
                                 h4(strong('Pan-Viral Infection'), align='center'),
                                 br(),
                                 
                                 tags$p("Pan-respiratory-virus differential expression (DE) analysis to determine 
                                        if the gene of interest is differentially expressed between infected and 
                                        un-infected samples presenting symptoms or not. Wilcoxon rank sum test is 
                                        used for DE analysis. 
                                   
                                        The expression levels and statistical significances 
                                        of the gene of interest can be visualized 
                                        in a box plot.", 
                                        style = "font-size: 100%;"),
                                 
                                 br(),
                                 
                                 tags$p("Receiver operating characteristic (ROC) analysis is performed to measure 
                                        the performance of the gene biomarker in distinguishing infected samples 
                                        from control samples with a forest plot.
                            
                                        A forest plot with the number of Infected and uninfected samples, 
                                        area under the curve (AUC), and 95% confidence interval (CI) 
                                        of the AUC for each project is used to visualize the result.", 
                                        style = "font-size: 100%;"),
                                        
                                 column(3),
                                 tags$img(src='img/pan_viral.jpg', width=800),
                                 h6(strong('Figure 1. Pan-respiratory-virus Infection Analysis'), align='center')
                                 
                             )
                         
                         ),
                         tabPanel(
                             strong("4. Single-Viral Infection"),
                             box(
                                 title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
                                 width = 12,
                                 
                                 h4(strong('Single-Viral Infection'), align='center'),
                                 br(),
                                 
                                 tags$p("ViRAL provides functions to focus the DE analysis and
                                         ROC analysis for the gene of interest in a selected project.",
                                        style = "font-size: 100%;"),
                                 
                                 br(),
                                 
                                 tags$p("To explore the gene of interest in a selected project, the steps were:",
                                        style = "font-size: 100%;"),
                                
                                 tags$p("(1) Select the virus, including one of the followings:",
                                        style = "font-size: 100%;"
                                        ),
                                 
                                 tags$p(
                                     "IFV: Influenza Virus",
                                     style = "font-size: 100%; text-indent: 2em;"
                                 ),
                                 tags$p(
                                     "RSV: Respiratory Syncytial Virus",
                                     style = "font-size: 100%; text-indent: 2em;"
                                 ),
                                 tags$p(
                                     "HRV: Human Rhinovirus",
                                     style = "font-size: 100%; text-indent: 2em;"
                                 ),
                                 tags$p(
                                     "SARS-CoV-2: Severe Acute Respiratory Syndrome Coronavirus 2",
                                     style = "font-size: 100%; text-indent: 2em;"
                                 ),
                                 tags$p(
                                     "Seasnal CoVs: Seasonal Coronavirus including HKU-1, OC43, etc.",
                                     style = "font-size: 100%; text-indent: 2em;"
                                 ),
                                 
                                 tags$p("(2) Select the infected cases presenting symptoms or not:",
                                        style = "font-size: 100%;"
                                 ),
                                 tags$p(
                                     "Symptomatic: symptomatic individuals with the viral infection.",
                                     style = "font-size: 100%; text-indent: 2em;"
                                 ),
                                 tags$p(
                                     "Asymptomatic: individuals who tested positive for a virus but presented no symptoms.",
                                     style = "font-size: 100%; text-indent: 2em;"
                                 ),
                                 tags$p("(3) Select the dataset of interest from the data table.",
                                        style = "font-size: 100%;"
                                 ),
                                 
                                        # 
                                        # "IFV: Influenza Virus", br(),
                                        # "RSV: Respiratory Syncytial Virus", br(),
                                        # "HRV: Human Rhinovirus", br(),
                                        # "SARS-CoV-2: Severe Acute Respiratory Syndrome Coronavirus 2", br(),
                                        # "Seasnal CoVs: Seasonal Coronavirus including HKU-1, OC43, etc.",
                                        # style = "font-size: 100%;  text-indent: 2em;"),
                                 # 
                                 # tags$p("(1) Select the virus, including one of the followings:", br(),
                                 #        "IFV: Influenza Virus", br(),
                                 #        "RSV: Respiratory Syncytial Virus", br(),
                                 #        "HRV: Human Rhinovirus", br(),
                                 #        "SARS-CoV-2: Severe Acute Respiratory Syndrome Coronavirus 2", br(),
                                 #        "Seasnal CoVs: Seasonal Coronavirus including HKU-1, OC43, etc.",
                                 #        style = "font-size: 100%;  text-indent: 2em;"),
                                 
                                 column(3),
                                 tags$img(src='img/single_viral.jpg', width=800),
                                 h6(strong('Figure 1. Single-Viral Infection Analysis'), align='center')
                                 
                             )
                             
                         ),
                         tabPanel(
                             strong("5. Multiple-Viral Infection"),
                             box(
                                 title = NULL, status = "info", solidHeader = TRUE, collapsible = FALSE,
                                 width = 12,
                                 
                                 h4(strong('Multiple-Viral Infection'), align='center'),
                                 br(),
                                 
                                 tags$p("Multiple-Viral infection, also known as viral co-infection, is 
                                        associated with increased rates of hospitalization.", br(),
                                        "ViRAL provides functions to focus the DE analysis and
                                        ROC analysis for the gene of interest in such a dataset with multiple-viral infection.
                                        ", 
                                        style = "font-size: 100%;"),
                                 
                                 br(),
                                 
                                 # tags$p("Receiver operating characteristic (ROC) analysis is performed to measure 
                                 #        the performance of the gene biomarker in distinguishing infected samples 
                                 #        from control samples with a forest plot.
                                 # 
                                 #        A forest plot with the number of Infected and uninfected samples, 
                                 #        area under the curve (AUC), and 95% confidence interval (CI) 
                                 #        of the AUC for each project is used to visualize the result.", 
                                 #        style = "font-size: 100%;"),
                                 
                                 column(3),
                                 tags$img(src='img/multiple_viral.jpg', width=800),
                                 h6(strong('Figure 1. Multiple-Viral Infection Analysis'), align='center')
                                 
                             )
                             
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
        tabPanel('Query', value = 'query', tab_query, icon = icon('search')),
        # tabPanel("TCGA miRNome", tab_tcga, icon = icon('database')),
        # tabPanel("Circulating miRNome", tab_circulating, icon = icon('database')),
        # tabPanel("Download", tab_download, icon = icon('download')),
        tabPanel("Tutorial", tab_tutorial, icon = icon('file-alt'))#,
        # 
        # # tags$style(type = 'text/css', href = 'bootstrap.css') 
        # # tags$style(type = 'text/css', '.navbar-nav {padding-left: 400px; font-size: 24px;}',
        # #            '.navbar-default {margin-left: 2px;margin-right: 18px;margin-top: -2px;}'
    ),
    dashboardFooter(right = HTML('<footer><script type="text/javascript" src="//rf.revolvermaps.com/0/0/6.js?i=5ost4vt1oo2&amp;m=7&amp;c=e63100&amp;cr1=ffffff&amp;f=arial&amp;l=0&amp;bv=90&amp;lx=-420&amp;ly=420&amp;hi=20&amp;he=7&amp;hc=a8ddff&amp;rs=80" async="async"></script></footer>'),
                    left = HTML("<footer><h6>Contact: <a href='https://github.com/yeli7068/ViRAL' target='_blank'>Yang LI</a><br>Email: yeli7068@outlook.com</h6><strong><h5><a href='https://sph.pku.edu.cn/info/1416/4185.htm' target='_blank'>Zhou Lab @ Peking University, Peking, China</a></h5></strong></footer>"))
)

shinyUI(ui)
