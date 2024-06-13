qatar_physeq_after_RelAbund_Sqrt <- transform_sample_counts(qatar_physeq_after_RelAbund, function(OTU) sqrt(OTU+1))
qatar_after_PCoA_ord <- ordinate(qatar_physeq_after_RelAbund_Sqrt, "PCoA", "bray")

#PCoA
legend_ord <- levels(with(alphaDiversityAfterOTU, reorder(Area.Type, Observed)))

area.pantone.col <- c("Pathology Lab" = "#9A2B2E", "NICU" = "#FBC85F","PICU" =  "#F96815", "IPAC Offices" =  "#C042BA","Microbiology Lab" = "#207329", "Pediatric Surgery Ward" = "#F0BDD5") # "#96D8DE", "864C24", "#325B74", "#726A4E")

PCoAAfterByAreaOTU <- plot_ordination(qatar_physeq_after_RelAbund_Sqrt, qatar_after_PCoA_ord, color = "Area.Type", shape = "Area.Type") + 
  scale_shape_manual(values = c(15, 15, 19, 2, 1, 1), 
                     name = "Area Type", 
                     breaks = legend_ord) + 
  geom_point(size = 3, stroke = 1.5) + 
  scale_color_manual(values = area.pantone.col, name = "Area Type", breaks = legend_ord)  + 
  theme(panel.background = element_rect(fill='white', colour = 'black'), 
        axis.line = element_blank()
        ) + 
  xlab(paste("PCo1 [",round(qatar_after_PCoA_ord$values$Relative_eig[1]*100,1),"%]",sep = "")) + 
  ylab(paste("PCo2 [",round(qatar_after_PCoA_ord$values$Relative_eig[2]*100,1),"%]",sep = "")) + 
  theme(text = element_text(family = "sans", size = 14))
PCoAAfterByAreaOTU$layers[[1]] <- NULL
df <- PCoAAfterByAreaOTU$data
PCoAAfterByAreaOTU <- PCoAAfterByAreaOTU + 
  stat_ellipse(data = df[df$Group == "Low Diversity Group",], 
               mapping = aes(x = Axis.1, y = Axis.2, group = Group, size = "Low Diversity Group"), 
               lty = 2, 
               color = "#4c4c4c", 
               level = 0.99,
               type = "norm"
               ) + 
  stat_ellipse(data = df[df$Group == "High Diversity Group",],
               aes(x = Axis.1, y = Axis.2, group = Group, size = "High Diversity Group"), 
               lty = 2, color = "#808080", 
               level = 0.99, 
               type = "norm") +
  scale_size_manual(
    "Group", values = rep(0.3,2),
    guide = guide_legend(override.aes = list(colour=c("#4c4c4c", "#808080")))) + 
  guides(shape = guide_legend(override.aes = list(linetype = 0)))


PCoAAfterByAreaFacetOTU <- plot_ordination(qatar_physeq_after_RelAbund_Sqrt, qatar_after_PCoA_ord, color = "Area.Type", shape = "Area.Type") + 
  scale_shape_manual(values = c(15, 15, 19, 2, 1, 1), 
                     name = "Area Type", 
                     breaks = legend_ord
                     ) + 
  geom_point(size = 3, stroke = 1.5) + 
  scale_color_manual(values = area.pantone.col, name = "Area Type", breaks = legend_ord) + 
  theme(panel.background = element_rect(fill='white', colour = 'black'), 
        axis.line = element_blank()
        ) + 
  xlab(paste("PCo1 [",round(qatar_after_PCoA_ord$values$Relative_eig[1]*100,1),"%]",sep = "")) + 
  ylab(paste("PCo2 [",round(qatar_after_PCoA_ord$values$Relative_eig[2]*100,1),"%]",sep = "")) + 
  theme(text = element_text(family = "sans", size = 14), 
        axis.text = element_text(size = 8), 
        panel.border = element_rect(fill=NA,color="#5c5c5c", size=0.5, linetype="solid"), 
        strip.background = element_blank()
        ) + 
  facet_wrap(~Area.Type, labeller = label_wrap_gen(12)) + 
  scale_x_continuous(breaks=seq(-0.001,0.002,length.out = 4)) + 
  guides(shape = guide_legend(override.aes = list(linetype = 0)))
PCoAAfterByAreaFacetOTU$layers[[1]] <- NULL

PCoAAfterByAreaGrid <- ggarrange(PCoAAfterByAreaOTU, PCoAAfterByAreaFacetOTU, 
                                 labels = c("(A)", "(B)"),
                                 font.label = list(face = "plain"),
                                 ncol = 1, nrow = 2,
                                 common.legend = TRUE,
                                 legend = "right"
)
print(PCoAAfterByAreaGrid)

sampledf <- data.frame(sample_data(qatar_physeq_after_RelAbund_Sqrt))
qatar_after_bray <- phyloseq::distance(qatar_physeq_after_RelAbund_Sqrt, method = "bray")

#Statistical Tests
disper_f <- c()
disper_p <- c()

sampledf <- data.frame(sample_data(qatar_physeq_after_RelAbund_Sqrt))
qatar_after_bray <- phyloseq::distance(qatar_physeq_after_RelAbund_Sqrt, method = "bray")

perm <- adonis(qatar_after_bray ~ Surface.Type.I + Area.Type + Time.Post.Opening , data = sampledf)

#Surface Type I
beta <- betadisper(qatar_after_bray, sampledf$Surface.Type.I)
disperSurface <- permutest(beta)
disper_f <- c(disper_f, disperSurface$tab$F[1])
disper_p <- c(disper_p, disperSurface$tab$`Pr(>F)`[1])

##Pairwise
pw.permSurface <- pairwise.adonis(qatar_after_bray, sampledf$Surface.Type.I, p.adjust.m = "BH")

pw.permadjSurfacep.valPlotData <- rbind(rep(NA,8), rep(NA,8), rep(NA,8), pw.permSurface)
pw.permadjSurfacep.valPlotData$pairs[1:3] <- c("Door Handle vs Door Handle", "Keyboard vs Keyboard", "Office Electronics vs Office Electronics")
pw.permadjSurfacep.valPlotData<- separate(pw.permadjSurfacep.valPlotData, col = pairs, into=c('Surface Category 1', 'Surface Category 2'), sep=' vs ', remove = FALSE)
pw.permadjSurfacep.valPlotData$`Surface Category 1` <- factor(pw.permadjSurfacep.valPlotData$`Surface Category 1`, levels = c("Door Handle", "Keyboard", "Office Electronics"))
pw.permadjSurfacep.valPlotData$`Surface Category 2` <- factor(pw.permadjSurfacep.valPlotData$`Surface Category 2`, levels = c("Door Handle", "Keyboard", "Office Electronics"))
pw.permadjSurfacep.valPlotData <- unite(pw.permadjSurfacep.valPlotData,c("Surface Category 2", "Surface Category 1"), col = "pairs_flip", sep = " vs ", remove = FALSE)
temp1 <- pw.permadjSurfacep.valPlotData[,-2]
temp2 <- pw.permadjSurfacep.valPlotData[-c(1:3),c(2,4,3,5:11)]
colnames(temp2) <- colnames(temp1)
pw.permadjSurfacep.valPlotData <- rbind(temp1, temp2)
pw.permadjSurfacep.valPlotData <- arrange(pw.permadjSurfacep.valPlotData, `Surface Category 1`, `Surface Category 2`)
pw.permadjSurfacep.valPlotData[c(2,3,6),4:10] <- NA
pw.permadjSurfacep.valPlotData$`Adjusted p-value` <- ifelse(pw.permadjSurfacep.valPlotData$`p.adjusted` < 0.05, "p < 0.05", ifelse(pw.permadjSurfacep.valPlotData$`p.adjusted` < 0.1, "0.05 < p < 0.1","p > 0.1"))

cols  <- c(
  "p < 0.05"  = "#000000",
  "0.05 < p < 0.1" = "#4c4c4c",
  "p > 0.1" = "#808080"
)
pw.permadjSurfacep.valPlot <- ggplot(pw.permadjSurfacep.valPlotData,aes(x = `Surface Category 1`, y = `Surface Category 2`, fill = `Adjusted p-value`)) + 
  scale_fill_manual(values = cols, na.value = "#FFFFFF") + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank()
        )
print(pw.permadjSurfacep.valPlot)

#Area Type
beta <- betadisper(qatar_after_bray, sampledf$Area.Type)
disperArea <- permutest(beta)
disper_f <- c(disper_f, disperArea$tab$F[1])
disper_p <- c(disper_p, disperArea$tab$`Pr(>F)`[1])

##Pairwise
pw.permArea <- pairwise.adonis(qatar_after_bray, sampledf$Area.Type, p.adjust.m = "BH")

pw.permadjAreap.valPlotData <- rbind(rep(NA,8), rep(NA,8), rep(NA,8), rep(NA,8), rep(NA,8), rep(NA,8), pw.permArea)
pw.permadjAreap.valPlotData$pairs[1:6] <- c("IPAC Offices vs IPAC Offices", "Pediatric Surgery Ward vs Pediatric Surgery Ward", "NICU vs NICU", "PICU vs PICU", "Pathology Lab vs Pathology Lab", "Microbiology Lab vs Microbiology Lab")
pw.permadjAreap.valPlotData<- separate(pw.permadjAreap.valPlotData, col = pairs, into=c('Area Type 1', 'Area Type 2'), sep=' vs ', remove = FALSE)
pw.permadjAreap.valPlotData$`Area Type 1` <- factor(pw.permadjAreap.valPlotData$`Area Type 1`, levels = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"))
pw.permadjAreap.valPlotData$`Area Type 2` <- factor(pw.permadjAreap.valPlotData$`Area Type 2`, levels = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"))
pw.permadjAreap.valPlotData <- unite(pw.permadjAreap.valPlotData,c("Area Type 2", "Area Type 1"), col = "pairs_flip", sep = " vs ", remove = FALSE)
temp1 <- pw.permadjAreap.valPlotData[,-2]
temp2 <- pw.permadjAreap.valPlotData[-c(1:6),c(2,4,3,5:11)]
colnames(temp2) <- colnames(temp1)
pw.permadjAreap.valPlotData <- rbind(temp1, temp2)
pw.permadjAreap.valPlotData <- arrange(pw.permadjAreap.valPlotData, `Area Type 1`, `Area Type 2`)
pw.permadjAreap.valPlotData[is.na(pw.wiladjArea1p.valPlotData$`Observed p_adjusted`),4:10] <- NA
pw.permadjAreap.valPlotData$`Adjusted p-value` <- ifelse(pw.permadjAreap.valPlotData$`p.adjusted` < 0.05, "p < 0.05", ifelse(pw.permadjAreap.valPlotData$`p.adjusted` < 0.1, "0.05 < p < 0.1","p > 0.1"))

pw.permadjAreap.valPlot <- ggplot(pw.permadjAreap.valPlotData,aes(x = `Area Type 1`, y = `Area Type 2`, fill = `Adjusted p-value`)) + 
  scale_fill_manual(values = cols, na.value = "#FFFFFF") + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank()
        )
print(pw.permadjAreap.valPlot)

#Time Post Opening
beta <- betadisper(qatar_after_bray, sampledf$Time.Post.Opening)
disperTime <- permutest(beta)
disper_f <- c(disper_f, disperTime$tab$F[1])
disper_p <- c(disper_p, disperTime$tab$`Pr(>F)`[1])

pw.permTime <- pairwise.adonis(qatar_after_bray, sampledf$Time.Post.Opening, p.adjust.m = "BH")

#PERMADISPER adjusted
disper_p_adjusted <- p.adjust(disper_p, method = "BH")

disp.p <- data.frame(disper_f, disper_p, disper_p_adjusted, row.names = c("Surface.Type.I", "Area.Type", "Time.Post.Opening"))

perm.p <- merge(perm$aov.tab, disp.p, by = 0, sort = FALSE)
