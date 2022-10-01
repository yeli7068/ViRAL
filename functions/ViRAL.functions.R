china.red <- '#d7000f'
hermes.orange <- '#fabd03'
marrs.green <- '#34a853'
klein.blue <- '#4286f5'

Prognosis.extract.Exprs.Pdata <- function(Rds) {
    traits <- pData(Rds)
    exprs <- exprs(Rds)
    
    traits <- 
        traits %>% 
        dplyr::select(Prognosis, DatasetID, Platform, Infection, AgeGroup, Subject) 
    
    exprs <- exprs[,rownames(traits)]
    return(list(traits = traits, exprs = exprs))
}


Pan.extract.Exprs.Pdata <- function(Rds) {
    traits <- pData(Rds)
    exprs <- exprs(Rds)
    
    traits <- 
        traits %>% 
        dplyr::select(DatasetID, Subject, AgeGroup, Symptom, Infection)
    
    exprs <- exprs[,rownames(traits)]
    return(list(traits = traits, exprs = exprs))
}

extractExprsPdata_Nasal <- function(Rds) {
    traits <- pData(Rds)
    exprs <- exprs(Rds)
    
    traits <- 
        traits %>% 
        dplyr::select(GEO, Platform, Infection, Phase) %>%
        filter(Phase %in% c("Controls", "Acute"))
    
    exprs <- exprs[,rownames(traits)]
    return(list(traits = traits, exprs = exprs))
}

Pan_WholeBlood_Nofacet_plot <- function(ViRAL_exprs, ViRAL_traits, Gene, Symptom.status) {
    
    ViRAL_traits <- 
        ViRAL_traits %>% 
        filter(Symptom %in% c("Controls", Symptom.status)) %>%
        mutate(Project = paste0(DatasetID, ": ",Infection))
    
    ViRAL_exprs <- ViRAL_exprs[, rownames(ViRAL_traits)] %>% as.matrix()
    
    ViRAL_traits$Gene <- Gene
    ViRAL_traits$Value <- ViRAL_exprs[Gene,]
    
    dataForBoxPlot.zscore <- 
        ViRAL_traits %>% group_by(Infection) %>% 
        mutate(zscore = (Value - mean(Value))/sd(Value))
    
    if (Symptom.status == "Symptomatic") {
        case.color = "#d7000f" # china.red
    }  else {
        case.color = "#fabd03" # hermes.orange
    }

    p <- ggboxplot(data = dataForBoxPlot.zscore,
                   x = "Project", y = "zscore",
                   fill = "Symptom",
                   ggtheme = theme_bw()) +
        stat_compare_means(aes(group = Symptom), label = "p.signif") +
        scale_fill_manual(values=c(marrs.green, case.color)) +
        theme(axis.title=element_text(size=16, face = 'bold'), 
              axis.text = element_text(color='black', size=12, face='bold'),
              axis.text.x = element_text(angle = 45, hjust=1, face='bold')) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=14),
              legend.position = "top") +
        xlab('')+ylab(paste0("Expression Level of ", Gene, "\n(Z-score)"))
    
    return(list(data = dataForBoxPlot.zscore, plot = p))
}

Pan_WholeBlood_facet_plot <- function(ViRAL_exprs, ViRAL_traits, Gene) {
    ViRAL_facet_traits <- ViRAL_traits
    ViRAL_facet_traits$Gene <- Gene
    ViRAL_facet_traits$Value <- ViRAL_exprs[Gene,]
    
    dataForBoxPlot.zscore <- 
        ViRAL_facet_traits %>% group_by(Infection, Group) %>% 
        mutate(zscore = (Value - mean(Value))/sd(Value))
    
    p <- ggboxplot(data = dataForBoxPlot.zscore,
              x = "Infection", y = "zscore",
              fill = "Phase",
              ggtheme = theme_bw()) +
        stat_compare_means(aes(group = Phase), label = "p.signif") +
        facet_wrap(~Group, ncol = 1) +
        scale_fill_manual(values=c(china.red, klein.blue)) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=14),
              legend.position = c(0.9, 0.9)) +
        theme(axis.title=element_text(size=16, face = 'bold'), 
              axis.text = element_text(color='black', size=12, face='bold'),
              axis.text.x = element_text(angle = 45, hjust=1, face='bold')) +
        xlab('')+ylab(paste0("Expression Level of ", Gene, "\n(Z-score)"))
    
    return(list(data = dataForBoxPlot.zscore, plot = p))
    
}



Pan_Nasal_plot <- function(ViRAL_exprs, ViRAL_traits, Gene) {
    ViRAL_traits$Gene <- Gene
    ViRAL_traits$Value <- ViRAL_exprs[Gene,]
    
    dataForBoxPlot.zscore <- 
        ViRAL_traits %>% group_by(Infection) %>% 
        mutate(zscore = (Value - mean(Value))/sd(Value))
    
    p <- ggboxplot(data = dataForBoxPlot.zscore,
              x = "Infection", y = "zscore",
              fill = "Phase",
              ggtheme = theme_bw()) +
        stat_compare_means(aes(group = Phase), label = "p.signif") +
        #    facet_wrap(~Group, ncol = 1) +
        scale_fill_manual(values=c(china.red, klein.blue)) +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=14),
              legend.position = "top") +
        theme(axis.title=element_text(size=16, face = 'bold'), 
              axis.text = element_text(color='black', size=12, face='bold'),
              axis.text.x = element_text(angle = 45, hjust=1, face='bold')) +
        xlab('')+ylab(paste0("Expression Level of ", Gene, "\n(Z-score)"))
    
    return(list(data = dataForBoxPlot.zscore, plot = p))
    
}

Pan_forest_plot <- function(forest.data) {
    # This is modified from CancerMiRome
    p <- ggplot(forest.data, aes(x=seq_along(Project), y=AUC)) +
        
        geom_rect(aes(xmin = seq_along(Project) - .5, xmax = seq_along(Project) + .5,
                      ymin = -1.05, ymax = 1.01,
                      fill = ordered(seq_along(Project) %% 2 + 1))) +
        scale_fill_manual(values = c("#00000033", "#FFFFFF33"), guide = "none") +
        #xlim(c(0.5, length(dataForForestPlot$Project)+1.5)) +
        scale_y_continuous(breaks = seq(0,1,0.5), labels = seq(0,1,0.5)) +
        scale_x_continuous(limits = c(0, length(forest.data$Project)+2), expand = c(0,0)) +
        #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
        #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
        geom_errorbar(aes(ymin=Lower95, ymax=Upper95),width=0.4, size=0.8, color='black') + 
        geom_point(color=china.red, size=4, shape=18) + #shape=15, facet_grid(.~type) +
        geom_text(data =forest.data, aes(x=seq_along(Project), y=-1, label=Project, group=NULL),
                  size=5, hjust = 0) +#, fontface='bold'
        geom_text(data =forest.data, aes(x=seq_along(Project), y=-0.7, label=N.Infected, group=NULL),
                  size=5) +#, fontface='bold'
        geom_text(data =forest.data, aes(x=seq_along(Project), y=-0.5, label=N.Controls, group=NULL),
                  size=5) +#, fontface='bold'
        geom_text(data =forest.data, aes(x=seq_along(Project), y=-0.33, label=AUC, group=NULL),
                  size=5) +
        geom_text(data =forest.data, aes(x=seq_along(Project), y=-0.15, label=paste0('(', Lower95, '-', Upper95, ')'), group=NULL),
                  size=5) +
        
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-6.7,-7.3,-2.6,2.6,6.1,5.9), label=P, group=NULL),
        #          size=4.4) +
        geom_hline(yintercept = 0.5, linetype=2, color='black') +
        geom_hline(yintercept = 0, linetype=1, color='grey') +
        #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
        #          size=4.4) +
        #scale_y_continuous(trans = 'log10',
        #                   breaks = c(0, 1, 2.5,50,250,7500),
        #                   labels = c(0, 1, 2.5,50,250,7500)) +
        coord_flip() +
        #ylim(0,0.05) +
        xlab('')+ylab('AUC') +
        #ggtitle(paste0('ROC Analysis of ', mir.name, ' in TCGA')) +
        #xlim(0,100) +
        theme_bw()+
        #theme_set(theme_minimal()) #
        theme(legend.title = element_blank(),
              legend.text = element_text(size=14),
              legend.position = 'right') +
        #theme(plot.title = element_text(color='black', size=18, face = 'bold', hjust = 0.5)) +
        theme(axis.title.x=element_text(size=16, face = 'bold', hjust = 0.71),
              axis.title.y=element_blank(),
              axis.ticks=element_blank(),
              axis.text = element_text(color='black', size=14, face = 'bold'),
              axis.text.y = element_blank(),
              #axis.text.x = element_text(angle = 0, hjust=0.5),
              strip.text = element_text(size=14)) +
        theme(#axis.line = element_line(colour = "black"),
            axis.line.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
        annotate(geom = 'text', x = length(forest.data$Project)+1.25, y=c(-.95,-0.70,-0.5,-0.33, -.15),
                 label=c('Project','Infected','Controls','AUC', '95% CI'), size=5, fontface='bold')
    return(p)
}
# Pan_forest_plot <- function(forest.data) {
#     # This is modified from CancerMiRome
#     p <- ggplot(forest.data, aes(x=seq_along(Project), y=AUC)) +
        
#         geom_rect(aes(xmin = seq_along(Project) - .5, xmax = seq_along(Project) + .5,
#                       ymin = -1.05, ymax = 1.01,
#                       fill = ordered(seq_along(Project) %% 2 + 1))) +
#         scale_fill_manual(values = c("#00000033", "#FFFFFF33"), guide = "none") +
#         #xlim(c(0.5, length(dataForForestPlot$Project)+1.5)) +
#         scale_y_continuous(breaks = seq(0,1,0.5), labels = seq(0,1,0.5)) +
#         scale_x_continuous(limits = c(0, length(forest.data$Project)+2), expand = c(0,0)) +
#         #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
#         #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
#         geom_errorbar(aes(ymin=Lower95, ymax=Upper95),width=0.4, size=0.8, color='black') + 
#         geom_point(color=china.red, size=4, shape=18) + #shape=15, facet_grid(.~type) +
#         geom_text(data =forest.data, aes(x=seq_along(Project), y=-1, label=Project, group=NULL),
#                   size=5, hjust = 0) +#, fontface='bold'
#         geom_text(data =forest.data, aes(x=seq_along(Project), y=-0.5, label=N.Infected, group=NULL),
#                   size=5) +#, fontface='bold'
#         geom_text(data =forest.data, aes(x=seq_along(Project), y=-0.35, label=N.Controls, group=NULL),
#                   size=5) +#, fontface='bold'
#         geom_text(data =forest.data, aes(x=seq_along(Project), y=-0.23, label=AUC, group=NULL),
#                   size=5) +
#         geom_text(data =forest.data, aes(x=seq_along(Project), y=-0.1, label=paste0('(', Lower95, '-', Upper95, ')'), group=NULL),
#                   size=5) +
        
#         #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-6.7,-7.3,-2.6,2.6,6.1,5.9), label=P, group=NULL),
#         #          size=4.4) +
#         geom_hline(yintercept = 0.5, linetype=2, color='black') +
#         geom_hline(yintercept = 0, linetype=1, color='grey') +
#         #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
#         #          size=4.4) +
#         #scale_y_continuous(trans = 'log10',
#         #                   breaks = c(0, 1, 2.5,50,250,7500),
#         #                   labels = c(0, 1, 2.5,50,250,7500)) +
#         coord_flip() +
#         #ylim(0,0.05) +
#         xlab('')+ylab('AUC') +
#         #ggtitle(paste0('ROC Analysis of ', mir.name, ' in TCGA')) +
#         #xlim(0,100) +
#         theme_bw()+
#         #theme_set(theme_minimal()) #
#         theme(legend.title = element_blank(),
#               legend.text = element_text(size=14),
#               legend.position = 'right') +
#         #theme(plot.title = element_text(color='black', size=18, face = 'bold', hjust = 0.5)) +
#         theme(axis.title.x=element_text(size=16, face = 'bold', hjust = 0.71),
#               axis.title.y=element_blank(),
#               axis.ticks=element_blank(),
#               axis.text = element_text(color='black', size=14, face = 'bold'),
#               axis.text.y = element_blank(),
#               #axis.text.x = element_text(angle = 0, hjust=0.5),
#               strip.text = element_text(size=14)) +
#         theme(#axis.line = element_line(colour = "black"),
#             axis.line.y = element_blank(),
#             panel.grid.minor.y = element_blank(),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.major.y = element_blank(),
#             panel.border = element_blank(),
#             panel.background = element_blank()) +
#         annotate(geom = 'text', x = length(forest.data$Project)+1.25, y=c(-.95,-0.5,-0.35,-0.23, -.1),
#                  label=c('Project','Infected','Controls','AUC', '95% CI'), size=5, fontface='bold')
#     return(p)
# }

single.project.boxplot.prepare <- function(selected.project.exprs, selected.project.traits, Gene, Symptom.status) {
    selected.project.traits$Gene <- Gene
    selected.project.traits$Value <- selected.project.exprs[Gene,]
    selected.project.traits$Symptom <- factor(selected.project.traits$Symptom,
                                       levels = c("Controls", Symptom.status))
    return(selected.project.traits)
}

Prognosis.boxplot.data.prepare <- function(Progression.exprs, Progression.traits, Gene) {
    Progression.traits$Gene <- Progression.exprs[Gene,]
    Progression.traits$Prognosis <- factor(Progression.traits$Prognosis,
                                             levels = c("Controls", "Non-Severe", "Severe"))
    
    return(Progression.traits)
    # Progression.overview <- c()
}

single.project.boxplot.plot <- function(single.project.boxplot.data, Gene, Symptom.status) {
    library(ggpubr)
    study <- single.project.boxplot.data$DatasetID %>% unique
    virus <- single.project.boxplot.data$Infection %>% unique
    if (Symptom.status == "Symptomatic") {
        case.color = "#d7000f" # china.red
    }  else {
        case.color = "#fabd03" # hermes.orange
    }
    single.project.boxplot.data <- 
        single.project.boxplot.data %>% 
        mutate(zscore = (Value - mean(Value))/sd(Value))

    ggboxplot(data = single.project.boxplot.data,
            x = "Symptom", y = "zscore",
            fill = "Symptom", # color = c(marrs.green, hermes.orange, china.red),
            add = "jitter",
            ggtheme = theme_bw(base_size = 18)) + 
            ylab(paste0("Gene Express of ", Gene, "\n(Z-scored)")) + 
            stat_compare_means(ref.group = "Controls", label = "p.signif") +
            scale_fill_manual(values=c(marrs.green, case.color)) +
            ggtitle(paste0(Symptom.status, " v.s. Controls\n(", virus, " infection in ", study, ")"))
}

single.project.rocplot.plot <- function(single.project.rocplot.data, Gene, Symptom.status) {
    
    single.project.rocplot.data <- 
        single.project.rocplot.data %>% 
        mutate(zscore = (Value - mean(Value))/sd(Value))

    study <- single.project.rocplot.data$DatasetID %>% unique
    virus <- single.project.rocplot.data$Infection %>% unique
    library(pROC)
    library(reportROC)

    single.project.rocplot.data$Symptom <- factor(single.project.rocplot.data$Symptom, levels = c("Controls", Symptom.status))

    roc1 <- roc(Symptom ~ zscore, data = single.project.rocplot.data, ci = T, direction = "<")
    reportroc1 <- reportROC(single.project.rocplot.data$Symptom, single.project.rocplot.data$zscore)
    
    if (as.numeric(reportroc1$SEN) == 1) {
        tp <- single.project.rocplot.data %>%
            filter(Symptom == Symptom.status) %>% 
            nrow()
        fn <- 0
        
        Se_AC <- (tp+1.96*1.96/2)/(tp+fn+1.96*1.96)
        reportroc1$SEN <- round(Se_AC, 3)
        Var_AC <- (Se_AC*(1-Se_AC))/(tp+fn+1.96*1.96)
        
        reportroc1$SEN.low <- round(max(Se_AC-1.96*sqrt(Var_AC),0), 3)
        reportroc1$SEN.up <- round(min(Se_AC+1.96*sqrt(Var_AC),1), 3)
    }

    dpi = 300
    library(ggsci)
    ggroc(roc1, legacy.axes = TRUE, colour = "1") + 
        geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed") +
        geom_text(aes(
            label = paste0(
                "P-value: ", reportroc1$P, "\n",
                "AUC: ", round(roc1$auc, 3), " (95% CI ", reportroc1$AUC.low, "-", reportroc1$AUC.up, ")"#, "\n",
                #"Sensitivity: ", reportroc1$SEN, " (95% CI: ", reportroc1$SEN.low, "-", reportroc1$SEN.up, ")", "\n",
                #"Specificity: ", reportroc1$SPE, " (95% CI: ", reportroc1$SPE.low, "-", reportroc1$SPE.up, ")"
            ), 
            y = 0.1, x = 0.7), size = 5.5) +
#        ggtitle(label = paste0(study, ": " , Symptom.status, " vs. Controls")) + 
        scale_fill_npg() + 
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        theme(axis.text.x = element_text(size = 16),
              axis.text.y = element_text(size = 18),
              legend.title = element_text(size = 18),
              axis.title =element_text(size = 18) ) +
        theme_bw(base_size = 16) +
        ggtitle(paste0(Symptom.status, " v.s. Controls\n(", virus, " infection in ", study, ")"))
}

multiple.infection.boxplot.plot <- function(selected.coinfection.exprs, selected.coinfection.traits, Gene, Symptom.status) {
    library(ggpubr)
    selected.coinfection.exprs <- selected.coinfection.exprs %>% as.matrix

    selected.coinfection.traits$Gene <- Gene
    selected.coinfection.traits$Value <- selected.coinfection.exprs[Gene,]
    selected.coinfection.traits$Symptom <- factor(selected.coinfection.traits$Symptom,
                                                  levels = c("Controls", "Symptomatic"))

    study <- selected.coinfection.traits$DatasetID %>% unique
    virus <- selected.coinfection.traits$Infection %>% unique
    
    case.color = "#d7000f" # china.red
    
   selected.coinfection.traits <- 
        selected.coinfection.traits %>% 
        mutate(zscore = (Value - mean(Value))/sd(Value))

    ggboxplot(data = selected.coinfection.traits,
            x = "Symptom", y = "zscore",
            fill = "Symptom", # color = c(marrs.green, hermes.orange, china.red),
            add = "jitter",
            ggtheme = theme_bw(base_size = 18)) + 
            ylab(paste0("Gene Express of ", Gene, "\n(Z-scored)")) + 
            stat_compare_means(ref.group = "Controls", label = "p.signif") +
            scale_fill_manual(values=c(marrs.green, case.color)) +
            ggtitle(paste0(Symptom.status, " v.s. Controls\n(", virus, " infection in ", study, ")"))
}

multiple.infection.rocplot.plot <- function(selected.coinfection.exprs, selected.coinfection.traits, Gene, Symptom.status) {
    
    selected.coinfection.exprs <- selected.coinfection.exprs %>% as.matrix

    selected.coinfection.traits$Gene <- Gene
    selected.coinfection.traits$Value <- selected.coinfection.exprs[Gene,]
    selected.coinfection.traits$Symptom <- factor(selected.coinfection.traits$Symptom,
                                                  levels = c("Controls", "Symptomatic"))

    study <- selected.coinfection.traits$DatasetID %>% unique
    virus <- selected.coinfection.traits$Infection %>% unique

    selected.coinfection.traits <- 
        selected.coinfection.traits %>% 
        mutate(zscore = (Value - mean(Value))/sd(Value))

    library(pROC)
    library(reportROC)

    selected.coinfection.traits$Symptom <- factor(selected.coinfection.traits$Symptom, levels = c("Controls", Symptom.status))

    roc1 <- roc(Symptom ~ zscore, data = selected.coinfection.traits, ci = T, direction = "<")
    reportroc1 <- reportROC(selected.coinfection.traits$Symptom, selected.coinfection.traits$zscore)
    
    if (as.numeric(reportroc1$SEN) == 1) {
        tp <- selected.coinfection.traits %>%
            filter(Symptom == Symptom.status) %>% 
            nrow()
        fn <- 0
        
        Se_AC <- (tp+1.96*1.96/2)/(tp+fn+1.96*1.96)
        reportroc1$SEN <- round(Se_AC, 3)
        Var_AC <- (Se_AC*(1-Se_AC))/(tp+fn+1.96*1.96)
        
        reportroc1$SEN.low <- round(max(Se_AC-1.96*sqrt(Var_AC),0), 3)
        reportroc1$SEN.up <- round(min(Se_AC+1.96*sqrt(Var_AC),1), 3)
    }

    dpi = 300
    library(ggsci)
    ggroc(roc1, legacy.axes = TRUE, colour = "1") + 
        geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey", linetype="dashed") +
        geom_text(aes(
            label = paste0(
                "P-value: ", reportroc1$P, "\n",
                "AUC: ", round(roc1$auc, 3), " (95% CI ", reportroc1$AUC.low, "-", reportroc1$AUC.up, ")"#, "\n",
                #"Sensitivity: ", reportroc1$SEN, " (95% CI: ", reportroc1$SEN.low, "-", reportroc1$SEN.up, ")", "\n",
                #"Specificity: ", reportroc1$SPE, " (95% CI: ", reportroc1$SPE.low, "-", reportroc1$SPE.up, ")"
            ), 
            y = 0.1, x = 0.7), size = 5.5) +
#        ggtitle(label = paste0(study, ": " , Symptom.status, " vs. Controls")) + 
        scale_fill_npg() + 
        theme(plot.title = element_text(hjust = 0.5, size = 16)) +
        theme(axis.text.x = element_text(size = 16),
              axis.text.y = element_text(size = 18),
              legend.title = element_text(size = 18),
              axis.title =element_text(size = 18) ) +
        theme_bw(base_size = 16) +
        ggtitle(paste0(Symptom.status, " v.s. Controls\n(", virus, " infection in ", study, ")"))
}

Prognosis.boxplot.data.plot <- function(Prognosis.boxplot.data, Gene) {
    library(ggpubr)
    compare.list <- list(c("Controls", "Non-Severe"),
                         c("Non-Severe", "Severe"),
                         c("Controls", "Severe"))

    ggboxplot(data = Prognosis.boxplot.data,
                   x = "Prognosis", y = "Gene",
                   fill = "Prognosis", # color = c(marrs.green, hermes.orange, china.red),
                   add = "jitter",
                   ggtheme = theme_bw()) + 
                   ylab(paste0("Expression of ", Gene)) + 
                   stat_compare_means(comparisons = compare.list, label = "p.signif") +
                   scale_fill_manual(values=c(marrs.green, hermes.orange, china.red))

}

