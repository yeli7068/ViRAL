#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
#library(shinyWidgets)
library(shinycssloaders)
#library(readxl)
library(ggpubr)
library(Matrix)
library(stringr)
library(Biobase)

library(limma)
library(DT)
library(dplyr)
library(pROC)
library(digest)

library(heatmaply)
library(htmlwidgets)
library(slickR)
library(AnnoProbe)

library(dashboardthemes)
library(shinythemes)
library(shinyalert)

source("./functions/ViRAL.functions.R")

china.red <- '#d7000f'
hermes.orange <- '#fabd03'
marrs.green <- '#34a853'
klein.blue <- '#4286f5'

##
gene.blood.default <- "RBBP6"

Pan.ViRAL.Before.Merged <- readRDS("./data/Pan.ViRAL.Before.Merged.rds")
#gene.annotation <- readRDS("./data/gene.annotation.rds")

single.virus.projects <- readRDS('./data/single.virus.datasets.rds')
coinfection.virus.projects <- readRDS("./data/Symptomatic.GSE68310.coinfection.rds")

single.virus.datasets <- readr::read_csv("./data/projects.csv")
co.virus.datasets <- readr::read_csv("./data/coinfection.csv")


#Prognosis.db <- readRDS("Prognosis.GSE101702.rds")


#=========================================================
Pan.ViRAL.Before.Merged <- Pan.extract.Exprs.Pdata(Pan.ViRAL.Before.Merged)
#Viral_NASAL <- extractExprsPdata_Nasal(Viral_NASAL)

#gene.annotation <- rownames(Pan.ViRAL.Before.Merged$exprs)

single.virus.projects <- Pan.extract.Exprs.Pdata(single.virus.projects)
#Prognosis.db <- extractExprsPdata_Nasal(Prognosis.db)

#gene.annotation <- rownames(single.virus.projects$exprs)
gene.annotation.info <- readRDS("./data/gene.annotation.rds")
gene.annotation <- gene.annotation.info$HGNC.symbol

coinfection.virus.projects <- Pan.extract.Exprs.Pdata(coinfection.virus.projects)
#=========================================================

single.virus.datasets <- 
    single.virus.datasets %>% 
    dplyr::select(!Purpose) %>%
    dplyr::filter(!str_detect(Viruses, "Co-infection"))

co.virus.datasets <- 
    co.virus.datasets %>% 
    dplyr::select(!Purpose)

# =======================

server <- function(input, output, session) { 
    
    updateSelectizeInput(session, 'gene.blood.id', choices = gene.annotation, selected = gene.blood.default, server = TRUE)
    
    output$gene.annos <- renderUI({ 
      Gene <- input$gene.blood.id

      Gene.Ensembl <- paste0('http://asia.ensembl.org/Homo_sapiens/Gene/Summary?g=', Gene)
      Gene.Ensembl <- a('ENSEMBL', href = Gene.Ensembl, target="_blank", style = "font-size:100%; color:#3b8dbc")
      
      #https://www.ncbi.nlm.nih.gov/gene?term=(RBBP6[gene])%20AND%20(Homo%20sapiens[orgn])%20AND%20alive[prop]%20NOT%20newentry[gene]&sort=weight
      Gene.NCBI <- paste0('https://www.ncbi.nlm.nih.gov/gene?term=(', Gene, '[gene])%20AND%20(Homo%20sapiens[orgn])%20AND%20alive[prop]%20NOT%20newentry[gene]&sort=weight')
      Gene.NCBI <- a('NCBI', href = Gene.NCBI, target="_blank", style = "font-size:100%; color:#3b8dbc")
      
      #https://www.genecards.org/cgi-bin/carddisp.pl?gene=RBBP6
      Gene.Genecards <- paste0('https://www.genecards.org/cgi-bin/carddisp.pl?gene=', Gene)
      Gene.Genecards <- a('GeneCards', href = Gene.Genecards, target="_blank", style = "font-size:100%; color:#3b8dbc")
      
      #https://go.drugbank.com/unearth/q?searcher=drugs&query=RBBP6
      Gene.Drugbank <- paste0('https://go.drugbank.com/unearth/q?searcher=drugs&query=', Gene)
      Gene.Drugbank <- a('DrugBank', href = Gene.Drugbank, target="_blank", style = "font-size:100%; color:#3b8dbc")
      
      #https://www.ebi.ac.uk/gxa/search?=&geneQuery=RBBP6
      Gene.EBI <- paste0('https://www.ebi.ac.uk/gxa/search?=&geneQuery=', Gene)
      Gene.EBI <- a('EBI', href = Gene.EBI, target="_blank", style = "font-size:100%; color:#3b8dbc")
      
      if (is.na(Gene)) {
        tagList("Lookup this gene in:")
      } else {
        tagList("Lookup this gene in:", 
                br(),
                Gene.Ensembl, Gene.NCBI, Gene.Genecards, Gene.Drugbank, Gene.EBI)
        #tagList(Gene.Ensembl, Gene.NCBI, Gene.Genecards)
      }
      
      })
    
    output$Gene.EnsemblID <- renderText({ 
        Gene <- input$gene.blood.id
        
        Gene.EnsemblID <- 
            gene.annotation.info %>%
            filter(HGNC.symbol == Gene)
        
        Gene.EnsemblID <- paste0('Ensemble ID: ', ifelse(is.na(Gene.EnsemblID$Gene.stable.ID), '', Gene.EnsemblID$Gene.stable.ID))
        Gene.EnsemblID
    })
    
    output$Gene.Alias <- renderText({ 
        Gene <- input$gene.blood.id
        
        Gene.Alias <- 
            gene.annotation.info %>%
            filter(HGNC.symbol == Gene)
        
        Gene.Alias <- paste0('Alias: ', ifelse(is.na(Gene.Alias$Alias), '', Gene.Alias$Alias))
        Gene.Alias
    })
    
    output$Gene.Description <- renderText({ 
        Gene <- input$gene.blood.id
        
        Gene.Description <- 
            gene.annotation.info %>%
            filter(HGNC.symbol == Gene)
        
        Gene.Description <- paste0('Description: ', ifelse(is.na(Gene.Description$Description), '', Gene.Description$Description))
        Gene.Description
    })
    # ================================================ Pan infection ================================================
    
    observeEvent(input$gene.blood.id, {
        
        Gene <- input$gene.blood.id
        Prognosis.overview <- Pan.overview <- reactiveValues()
        
        
        ## =============================================== Pan infection boxplot start ===============================================

        output$Pan.Symptomatic <- renderPlot({
            if (Gene == "") {
                return()
            }

            Viral_WB_traits <- Pan.ViRAL.Before.Merged$traits
            Viral_WB_exprs <- Pan.ViRAL.Before.Merged$exprs
            
            Pan.Symptomatic.box <- Pan_WholeBlood_Nofacet_plot(Viral_WB_exprs, Viral_WB_traits, Gene, "Symptomatic")
            #blood.roc.data <- blood.box$data %>% as.data.frame()
            
            #Pan.overview$blood.roc.data <- blood.roc.data
            Pan.overview$Pan.Symptomatic.box.data <- Pan.Symptomatic.box$data %>% as.data.frame()
            #print(blood.box$data)
            
            Pan.overview$Pan.Symptomatic.box.plot <- Pan.Symptomatic.box$plot
            
            Pan.Symptomatic.box$plot
            
        }, height = 700)
        
        output$Pan.Symptomatic.boxplot.data.downbttn <- downloadHandler(
            filename = function(){paste(Gene,'.Pan.Symptomatic.boxplot.data.csv', sep = '')},
            
            content = function(filename){
                write.csv(Pan.overview$Pan.Symptomatic.box.data, filename, row.names = FALSE, quote = F)
            }
        )

        output$Pan.Symptomatic.boxplot.pdf.downbttn <- downloadHandler(
            filename = function(){paste(Gene,'.Pan.Symptomatic.boxplot.pdf', sep = '')},
            
            content = function(filename){
                pdf(filename, width = 10, height = 6)
                print(Pan.overview$Pan.Symptomatic.box.plot)
                dev.off()
            }
        )

        output$Pan.Asymptomatic <- renderPlot({
            if (Gene == "") {
                return()
            }
            
            Viral_WB_traits <- Pan.ViRAL.Before.Merged$traits
            Viral_WB_exprs <- Pan.ViRAL.Before.Merged$exprs
            
            Pan.Asymptomatic.box <- Pan_WholeBlood_Nofacet_plot(Viral_WB_exprs, Viral_WB_traits, Gene, "Asymptomatic")
            #blood.roc.data <- blood.box$data %>% as.data.frame()
            
            #Pan.overview$blood.roc.data <- blood.roc.data
            Pan.overview$Pan.Asymptomatic.box.data <- Pan.Asymptomatic.box$data %>% as.data.frame()
            #print(blood.box$data)
            
            Pan.overview$Pan.Asymptomatic.box.plot <- Pan.Asymptomatic.box$plot
            
            Pan.Asymptomatic.box$plot
            
        }, height = 700)

        output$Pan.Asymptomatic.boxplot.data.downbttn <- downloadHandler(
            filename = function(){paste(Gene,'.Pan.Asymptomatic.boxplot.data.csv', sep = '')},
            
            content = function(filename){
                write.csv(Pan.overview$Pan.Asymptomatic.box.data, filename, row.names = FALSE, quote = F)
            }
        )

        output$Pan.Asymptomatic.boxplot.pdf.downbttn <- downloadHandler(
            filename = function(){paste(Gene,'.Pan.Asymptomatic.box.plot.pdf', sep = '')},
            
            content = function(filename){
                pdf(filename, width = 10, height = 6)
                print(Pan.overview$Pan.Asymptomatic.box.plot)
                dev.off()
            }
        )

        ## =============================================== Pan infection Forest plot start ===============================================
        
        output$Pan.Symptomatic.Rocplot.Forest <- renderPlot({
            if (Gene == "") {
                return()
            }
            
            Pan.Symptomatic.roc.data <- Pan.overview$Pan.Symptomatic.box.data
            #print(Pan.Symptomatic.roc.data)
            # 
            Pan.Symptomatic.roc.data %>% write.csv(file = "Pan.Symptomatic.roc.data.csv")
            # # 
            Pan.Symptomatic.roc.data <- readr::read_csv("Pan.Symptomatic.roc.data.csv")
            
            Pan.Symptomatic.forest.data <- c()
            
            for (prj in unique(Pan.Symptomatic.roc.data$Project) ) {
                # prj <- "GSE17156: HRV"
                # print(prj)
                prj.data <- 
                    Pan.Symptomatic.roc.data %>% 
                    filter(Project == prj)
                
                prj.counts <- 
                    prj.data %>% group_by(Symptom) %>%
                    summarise(counts = n())
                
                prj.data$Symptom <- factor(prj.data$Symptom, levels = c("Controls", "Symptomatic"))
                
                prj.roc <- roc(prj.data$Symptom, prj.data$zscore, plot=FALSE, ci=TRUE, auc=TRUE, direction = "<")
                ci.auc <- prj.roc$ci
                
                auc <- ci.auc[2]
                auc.ci.lower95 <- ci.auc[1]
                auc.ci.upper95 <- ci.auc[3]
                
                auc <- format(auc, digits = 2, nsmall=2)
                auc.ci.lower95 <- format(auc.ci.lower95, digits = 2, nsmall=2)
                auc.ci.upper95 <- format(auc.ci.upper95, digits = 2, nsmall=2)
                
                Pan.Symptomatic.forest.data <- rbind(Pan.Symptomatic.forest.data,
                                           c(prj.counts$counts[1], prj.counts$counts[2], auc, auc.ci.lower95, auc.ci.upper95)
                )
                # print(Pan.Symptomatic.forest.data)
            }
            
            
            Pan.Symptomatic.forest.data <- apply(Pan.Symptomatic.forest.data, 2, as.numeric)
            
            Pan.Symptomatic.forest.data <- data.frame(Pan.Symptomatic.forest.data,
                                            row.names = unique(Pan.Symptomatic.roc.data$Project),
                                            stringsAsFactors = F)
            
            colnames(Pan.Symptomatic.forest.data) <- c("N.Controls", "N.Infected", "AUC", "Lower95", "Upper95")
            
            Pan.Symptomatic.forest.data$Gene <- Gene
            
            o <- order(Pan.Symptomatic.forest.data$AUC, decreasing = F)
            
            Pan.Symptomatic.forest.data <- Pan.Symptomatic.forest.data[o,]
            
            Pan.Symptomatic.forest.data$Project <- rownames(Pan.Symptomatic.forest.data)
            
            Pan.Symptomatic.forest.data.tmp <-
            Pan.Symptomatic.forest.data %>%
                mutate(Project = str_replace(Project, ": ", "\n"))
            p <- Pan_forest_plot(Pan.Symptomatic.forest.data.tmp)

            #p <- Pan_forest_plot(Pan.Symptomatic.forest.data)
            
            Pan.overview$Pan.Symptomatic.forest.data <- Pan.Symptomatic.forest.data
            Pan.overview$Pan.Symptomatic.forest.plot <- p
            
            p
        }, height = 700)
        
        output$Pan.Symptomatic.forestplot.data.downbttn <- downloadHandler(
            filename = function(){paste(Gene,'.Pan.Symptomatic.forestplot.data.csv', sep = '')},
            
            content = function(filename){
                write.csv(Pan.overview$Pan.Symptomatic.forest.data, filename, row.names = FALSE, quote = F)
            }
        )

        output$Pan.Symptomatic.forestplot.pdf.downbttn <- downloadHandler(
            filename = function(){paste(Gene,'.Pan.Symptomatic.forestplot.pdf', sep = '')},
            
            content = function(filename){
                pdf(filename, width = 14, height = 6)
                print(Pan.overview$Pan.Symptomatic.forest.plot)
                dev.off()
            }
        )
        
        output$Pan.Asymptomatic.Rocplot.Forest <- renderPlot({
            if (Gene == "") {
                return()
            }
            
            Pan.Asymptomatic.roc.data <- Pan.overview$Pan.Asymptomatic.box.data
            #print(Pan.Asymptomatic.roc.data)
# 
            Pan.Asymptomatic.roc.data %>% write.csv(file = "Pan.Asymptomatic.roc.data.csv")

            Pan.Asymptomatic.roc.data <- read_csv("Pan.Asymptomatic.roc.data.csv")
            
            Pan.Asymptomatic.forest.data <- c()
            
            for (prj in unique(Pan.Asymptomatic.roc.data$Project) ) {
                # prj <- "GSE17156: HRV"
                # print(prj)
                prj.data <- 
                    Pan.Asymptomatic.roc.data %>% 
                    filter(Project == prj)
                
                prj.counts <- 
                    prj.data %>% group_by(Symptom) %>%
                    summarise(counts = n())
                
                prj.data$Symptom <- factor(prj.data$Symptom, levels = c("Controls", "Asymptomatic"))
                
                prj.roc <- roc(prj.data$Symptom, prj.data$zscore, plot=FALSE, ci=TRUE, auc=TRUE, direction = "<")
                ci.auc <- prj.roc$ci
                
                auc <- ci.auc[2]
                auc.ci.lower95 <- ci.auc[1]
                auc.ci.upper95 <- ci.auc[3]
                
                auc <- format(auc, digits = 2, nsmall=2)
                auc.ci.lower95 <- format(auc.ci.lower95, digits = 2, nsmall=2)
                auc.ci.upper95 <- format(auc.ci.upper95, digits = 2, nsmall=2)
                
                Pan.Asymptomatic.forest.data <- rbind(Pan.Asymptomatic.forest.data,
                                                      c(prj.counts$counts[1], prj.counts$counts[2], auc, auc.ci.lower95, auc.ci.upper95)
                )
                # print(Pan.Asymptomatic.forest.data)
            }
            
            
            Pan.Asymptomatic.forest.data <- apply(Pan.Asymptomatic.forest.data, 2, as.numeric)
            
            Pan.Asymptomatic.forest.data <- data.frame(Pan.Asymptomatic.forest.data,
                                                       row.names = unique(Pan.Asymptomatic.roc.data$Project),
                                                       stringsAsFactors = F)
            
            colnames(Pan.Asymptomatic.forest.data) <- c("N.Infected", "N.Controls", "AUC", "Lower95", "Upper95")
            
            Pan.Asymptomatic.forest.data$Gene <- Gene
            
            o <- order(Pan.Asymptomatic.forest.data$AUC, decreasing = F)
            
            Pan.Asymptomatic.forest.data <- Pan.Asymptomatic.forest.data[o,]
            
            Pan.Asymptomatic.forest.data$Project <- rownames(Pan.Asymptomatic.forest.data)
            
            Pan.Asymptomatic.forest.data.tmp <-
            Pan.Asymptomatic.forest.data %>%
                mutate(Project = str_replace(Project, ": ", "\n"))
            p <- Pan_forest_plot(Pan.Asymptomatic.forest.data.tmp)

            #p <- Pan_forest_plot(Pan.Asymptomatic.forest.data)
            
            Pan.overview$Pan.Asymptomatic.forest.data <- Pan.Asymptomatic.forest.data
            Pan.overview$Pan.Asymptomatic.forest.plot <- p
            
            p
        }, height = 700)

        output$Pan.Asymptomatic.forestplot.data.downbttn <- downloadHandler(
            filename = function(){paste(Gene,'.Pan.Asymptomatic.forestplot.data.csv', sep = '')},
            
            content = function(filename){
                write.csv(Pan.overview$Pan.Asymptomatic.forest.data, filename, row.names = FALSE, quote = F)
            }
        )

        output$Pan.Asymptomatic.forestplot.pdf.downbttn <- downloadHandler(
            filename = function(){paste(Gene,'.Pan.Asymptomatic.forestplot.pdf', sep = '')},
            
            content = function(filename){
                pdf(filename, width = 14, height = 6)
                print(Pan.overview$Pan.Asymptomatic.forest.plot)
                dev.off()
            }
        )
    })
    
    # ================================================    single infection project    ================================================
    
    observeEvent(input$gene.blood.id, {
        Gene <- input$gene.blood.id
#        print(Gene)
        Project.overview <- reactiveValues()

        observeEvent(input$single.infection.id, {
            Virus.selected <- single.infection.id <- input$single.infection.id # IFV RSV

            single.virus.datasets.selected <-
                single.virus.datasets %>%
                filter(Viruses == single.infection.id)
            
            output$single.infection.datasets.print <- 
                DT::renderDataTable(single.virus.datasets.selected,
                                    options = list(pageLength = 5),
                                    selection = list(mode='single', selected=2)
                )
            
            observeEvent(input$single.infection.symptom.id, {
                Symptom.status <- single.infection.symptom.id <- input$single.infection.symptom.id # Asymptomatic Symptomatic
                
                observeEvent(input$single.infection.datasets.print_rows_selected, {
                    idx <- input$single.infection.datasets.print_rows_selected
                    #print(idx)
                    P.DatasetID <- single.virus.datasets.selected[idx, 1] %>% as.list()
                    #print(P.DatasetID[[1]])
                    
                    selected.project.traits <- 
                        single.virus.projects$traits %>% 
                        filter(str_detect(DatasetID, P.DatasetID[[1]])) %>% 
                        filter(Symptom %in% c("Controls", Symptom.status)) %>% 
                        filter(Infection == Virus.selected)
                    
                    #print(c(P.DatasetID[[1]], Symptom.status, Virus.selected))
                    #print(seleted.project.traits)
                    
                    
                    selected.project.exprs <- 
                        single.virus.projects$exprs[,rownames(selected.project.traits)] %>% as.matrix()
                    
                    #seleted.project.traits$Gene <- Gene
                    #seleted.project.traits$Value <- seleted.project.exprs[Gene,]
                    
                    output$single.project.boxplot.p <- renderPlot({
                        selected.project.include.asy <- selected.project.traits$Symptom %>% unique() %>% length()
                        validate(
                            need(selected.project.include.asy > 1, "This dataset did not include ASYMPTOMATIC cases. Please Re-select!")
                        )
                        
                        if (Gene == "") {
                            return()
                        }
                        #print(c(P.DatasetID[[1]], Symptom.status, Virus.selected))
                        single.project.boxplot.data <- single.project.boxplot.prepare(selected.project.exprs, selected.project.traits, Gene, Symptom.status)
                        #print(single.project.boxplot.data)
                        Project.overview$roc <- single.project.boxplot.data
                        single.project.boxplot.plot(single.project.boxplot.data, Gene, Symptom.status)
                        
                    })
                    output$single.project.rocplot.p <- renderPlot({
                        selected.project.include.asy <- selected.project.traits$Symptom %>% unique() %>% length()
                        validate(
                            need(selected.project.include.asy > 1, "This dataset did not include ASYMPTOMATIC cases. Please Re-select!")
                        )
                        
                        if (Gene == "") {
                            return()
                        }
                        #print(c(P.DatasetID[[1]], Symptom.status, Virus.selected))
                        single.project.boxplot.data <- single.project.boxplot.prepare(selected.project.exprs, selected.project.traits, Gene, Symptom.status)
                        # print(single.project.boxplot.data)
                        # single.project.boxplot.data %>% write_csv(file = "tmp.csv")
                        # single.project.boxplot.data <- read_csv("tmp.csv")
                        single.project.rocplot.data <- single.project.boxplot.data
                        single.project.rocplot.plot(single.project.rocplot.data = single.project.rocplot.data, Gene, Symptom.status)                        
                    })
                })
            })
        })
    })
    
    # ================================================    co-infection  project  ================================================
    
    observeEvent(input$gene.blood.id, {
        Gene <- input$gene.blood.id
        Multiple.overview <- reactiveValues()
        
        output$multiple.infection.datasets.print <- 
            DT::renderDataTable(co.virus.datasets,
                                options = list(pageLength = 5),
                                selection = list(mode='single', selected=2)
            )
        
        observeEvent(input$multiple.infection.datasets.print_rows_selected, {
            idx <- input$multiple.infection.datasets.print_rows_selected
            co.infection <- co.virus.datasets[idx, 4] %>% as.list()
            co.infection.selected <- co.infection[[1]]
            #print(co.infection.selected)
            
            selected.coinfection.traits <- 
                coinfection.virus.projects$traits %>% 
                filter(Infection == co.infection.selected)
            
            selected.coinfection.exprs <- 
                coinfection.virus.projects$exprs[,rownames(selected.coinfection.traits)]
            
            
            output$multiple.infection.boxplot.p <- renderPlot({
                if (Gene == "") {
                    return()
                }
                #print(c(P.DatasetID[[1]], Symptom.status, Virus.selected))
                
                multiple.infection.boxplot.plot(selected.coinfection.exprs,
                                                selected.coinfection.traits,
                                                Gene,
                                                "Symptomatic")
                
            })
            
            output$multiple.infection.rocplot.p <- renderPlot({
                if (Gene == "") {
                    return()
                }
                #print(c(P.DatasetID[[1]], Symptom.status, Virus.selected))
                
                multiple.infection.rocplot.plot(selected.coinfection.exprs,
                                                selected.coinfection.traits,
                                                Gene,
                                                "Symptomatic")           
            })
        })
    })
}

