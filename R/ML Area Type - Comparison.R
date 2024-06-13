#Compare
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
venn.diagram(
  x = list(unique(lassoAreaTaxaAll$ASV_name), svmLinearAreaTaxaAll$ASV_name, svmRadialAreaTaxaAll$ASV_name, rfAreaTaxaAll$ASV_name),
  category.names = c("LASSO" , "Linear SVM", "Radial SVM", "Random Forest"),
  filename = "./Data/Venn Diagram - ML Models ASV.png",
  output=TRUE,
  disable.logging = TRUE,
  fill = brewer.pal(4, "Pastel2"),
)

commonASVAll <- Reduce(intersect, list(unique(lassoAreaTaxaAll$ASV_name), svmLinearAreaTaxaAll$ASV_name, svmRadialAreaTaxaAll$ASV_name, rfAreaTaxaAll$ASV_name))
commonASVMulticlass <- Reduce(intersect, list(svmLinearAreaTaxaAll$ASV_name, svmRadialAreaTaxaAll$ASV_name, rfAreaTaxaAll$ASV_name))
commonASVMulticlass <- commonASVMulticlass[!(commonASVMulticlass %in% commonASVAll)]

allModelsAreaTaxaAll <- rbind(lassoAreaTaxaAll, data.frame(svmLinearAreaTaxaAll, Model = "Linear SVM"), data.frame(svmRadialAreaTaxaAll, Model = "Radial SVM"), data.frame(rfAreaTaxaAll, Model = "Random Forest"))
featureImportanceCommon <- allModelsAreaTaxaAll[allModelsAreaTaxaAll$ASV_name %in% c(commonASVAll, commonASVMulticlass),]
featureImportanceCommon[featureImportanceCommon$Model == "Pediatric.Surgery.Ward", "Model"] <- "LASSO:Pediatric Surgery Ward"
featureImportanceCommon[featureImportanceCommon$Model == "PICU", "Model"] <- "LASSO:PICU"

print(ggplot(featureImportanceCommon, aes(ASV_name,Value, fill = Model)) + geom_col(position = 'dodge'))

featureImportanceCore <- allModelsAreaTaxaAll[allModelsAreaTaxaAll$ASV_name %in% topPrevalent_ASV$ASV_name,]

qatar_physeq_after_RelAbund_df <- psmelt(qatar_physeq_after_RelAbund)
qatar_physeq_after_df_commonASVAll <- qatar_physeq_after_RelAbund_df[qatar_physeq_after_RelAbund_df$ASV_name %in% commonASVAll,]
qatar_physeq_after_merged_AreaType_commonASVAll <-  prune_taxa(qatar_physeq_after_df_commonASVAll$OTU, qatar_physeq_after_merged_AreaType)

commonASVAllAreaPlot <- plot_heatmap(qatar_physeq_after_merged_AreaType_commonASVAll, 
                                     method = NULL, 
                                     sample.order = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"), 
                                     taxa.label = "ASV_name"
                                     ) +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", trans = scales::log_trans(4), labels = scales::number_format(accuracy = 0.0001)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank(),
        text = element_text(family = "sans", size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(face = "italic", size = 8)) + 
  guides(fill = guide_colourbar(barwidth = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  #scale_y_discrete(labels = function(x) str_wrap(x, width = 35), name = paste("Informative ASVs\n in the ", y.AreaName[i])) +
  xlab("Area Type") +
  labs(fill = "Relative \nAbundance (%)")
print(commonASVAllAreaPlot)

boxplotsCommonASVAll <- plot_jittered_boxplot(qatar_physeq_after_df_commonASVAll, "Area.Type", "Abundance", "Group") + 
  scale_fill_grey(start = .4, end = .8) + 
  xlab("Area Type") + 
  ylab ("Relative Abundance (%)") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(text = element_text(family = "sans", size = 10), 
        legend.position="right",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=8),
        plot.title = element_text(size = 10))
print(boxplotsCommonASVAll)

qatar_physeq_after_df_commonASVMulticlass <- qatar_physeq_after_RelAbund_df[qatar_physeq_after_RelAbund_df$ASV_name %in% commonASVMulticlass,]
qatar_physeq_after_merged_AreaType_commonASVMulticlass <-  prune_taxa(qatar_physeq_after_df_commonASVMulticlass$OTU, qatar_physeq_after_merged_AreaType)

commonASVMulticlassAreaPlot <- plot_heatmap(qatar_physeq_after_merged_AreaType_commonASVMulticlass, 
                                            method = NULL, 
                                            sample.order = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"), 
                                            taxa.order = rev(rfAreaTaxaAll[rfAreaTaxaAll$ASV_name %in% commonASVMulticlass, "OTU"]),
                                            taxa.label = "ASV_name"
) +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", trans = scales::log_trans(4), labels = scales::number_format(accuracy = 0.0001)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank(),
        text = element_text(family = "sans", size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(face = "italic", size = 8)) + 
  guides(fill = guide_colourbar(barwidth = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  #scale_y_discrete(labels = function(x) str_wrap(x, width = 35), name = paste("Informative ASVs\n in the ", y.AreaName[i])) +
  xlab("Area Type") +
  labs(fill = "Relative \nAbundance (%)")
print(commonASVMulticlassAreaPlot)

boxplotsCommonASVMulticlass <- plot_jittered_boxplot(qatar_physeq_after_df_commonASVMulticlass, "Area.Type", "Abundance", "Group") + 
  scale_fill_grey(start = .4, end = .8) + 
  xlab("Area Type") + 
  ylab ("Relative Abundance (%)") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(text = element_text(family = "sans", size = 10), 
        legend.position="right",
        legend.title = element_text(size=10), 
        legend.text = element_text(size=8),
        plot.title = element_text(size = 10))
print(boxplotsCommonASVMulticlass)

print(plot_tree(qatar_physeq_after_merged_AreaType_commonASVMulticlass, nodelabf=nodeplotboot(80,0,3), color="Area.Type", label.tips="ASV_name", ladderize="left"))

qatar_physeq_after_merged_AreaType_allASV <-  prune_taxa(allModelsAreaTaxaAll$OTU, qatar_physeq_after_merged_AreaType)
print(plot_tree(qatar_physeq_after_merged_AreaType_allASV, "treeonly", nodelabf=nodeplotblank, ladderize="left") + coord_polar(theta="y"))
mat1 <- allModelsAreaTaxaAll[,c("ASV_name","Value","Area.Type","Model")]
mat1[mat1$Model == "IPAC.Offices", "Model"] <- "LASSO:IPAC Offices"
mat1[mat1$Model == "NICU", "Model"] <- "LASSO:NICU"
mat1[mat1$Model == "Pediatric.Surgery.Ward", "Model"] <- "LASSO:Pediatric Surgery Ward"
mat1[mat1$Model == "PICU", "Model"] <- "LASSO:PICU"
mat1[mat1$Model == "Pathology.Lab", "Model"] <- "LASSO:Pathology Lab"
mat1[mat1$Model == "Microbiology.Lab", "Model"] <- "LASSO:Microbiology Lab"

mat1[(mat1$Model == "LASSO:IPAC Offices" & mat1$Area.Type == "Other"), "Area.Type"] <- "IPAC.Offices"
mat1[(mat1$Model == "LASSO:NICU" & mat1$Area.Type == "Other"), "Area.Type"] <- "NICU"
mat1[(mat1$Model == "LASSO:Pediatric Surgery Ward" & mat1$Area.Type == "Other"), "Area.Type"] <- "Pediatric.Surgery.Ward"
mat1[(mat1$Model == "LASSO:PICU" & mat1$Area.Type == "Other"), "Area.Type"] <- "PICU"
mat1[(mat1$Model == "LASSO:Pathology Lab" & mat1$Area.Type == "Other"), "Area.Type"] <- "Pathology.Lab"
mat1[(mat1$Model == "LASSO:Microbiology Lab" & mat1$Area.Type == "Other"), "Area.Type"] <- "Microbiology.Lab"

mat1 <- mat1[mat1$ASV_name != "(Intercept)",]

mat_IPAC <- mat1[mat1$Area.Type == "IPAC.Offices", c("ASV_name","Value","Model")] %>%
  pivot_wider(values_from = Value ,id_cols = ASV_name, names_from = Model) %>%
  merge(rfAreaTaxaAll[,c("ASV_name","Value")], by = "ASV_name", all.x = TRUE) %>%
  rename("Random Forest" = "Value") %>%
  column_to_rownames(var = "ASV_name")
mat_IPAC$`LASSO:IPAC Offices` <-  scales::rescale(mat_IPAC$`LASSO:IPAC Offices`)*100

mat_NICU <- mat1[mat1$Area.Type == "NICU", c("ASV_name","Value","Model")] %>%
  pivot_wider(values_from = Value ,id_cols = ASV_name, names_from = Model) %>%
  merge(rfAreaTaxaAll[,c("ASV_name","Value")], by = "ASV_name", all.x = TRUE) %>%
  rename("Random Forest" = "Value") %>%
  column_to_rownames(var = "ASV_name")
mat_NICU$`LASSO:NICU` <-  scales::rescale(mat_NICU$`LASSO:NICU`)*100

mat_Surgery <- mat1[mat1$Area.Type == "Pediatric.Surgery.Ward", c("ASV_name","Value","Model")] %>%
  pivot_wider(values_from = Value ,id_cols = ASV_name, names_from = Model) %>%
  merge(rfAreaTaxaAll[,c("ASV_name","Value")], by = "ASV_name", all.x = TRUE) %>%
  rename("Random Forest" = "Value") %>%
  column_to_rownames(var = "ASV_name")
mat_Surgery$`LASSO:Pediatric Surgery Ward` <-  scales::rescale(mat_Surgery$`LASSO:Pediatric Surgery Ward`)*100

mat_PICU <- mat1[mat1$Area.Type == "PICU", c("ASV_name","Value","Model")] %>%
  pivot_wider(values_from = Value ,id_cols = ASV_name, names_from = Model) %>%
  merge(rfAreaTaxaAll[,c("ASV_name","Value")], by = "ASV_name", all.x = TRUE) %>%
  rename("Random Forest" = "Value") %>%
  column_to_rownames(var = "ASV_name")
mat_PICU$`LASSO:PICU` <-  scales::rescale(mat_PICU$`LASSO:PICU`)*100

mat_Path <- mat1[mat1$Area.Type == "Pathology.Lab", c("ASV_name","Value","Model")] %>%
  pivot_wider(values_from = Value ,id_cols = ASV_name, names_from = Model) %>%
  merge(rfAreaTaxaAll[,c("ASV_name","Value")], by = "ASV_name", all.x = TRUE) %>%
  rename("Random Forest" = "Value") %>%
  column_to_rownames(var = "ASV_name")
mat_Path$`LASSO:Pathology Lab` <-  scales::rescale(mat_Path$`LASSO:Pathology Lab`)*100

##None for Microbiology Lab

mat_rf <- mat1[!(mat1$ASV_name %in% c(rownames(mat_IPAC), rownames(mat_NICU), rownames(mat_Surgery), rownames(mat_PICU), rownames(mat_Path))), c("ASV_name","Value","Model")] %>%
  pivot_wider(values_from = Value ,id_cols = ASV_name, names_from = Model) %>%
  add_column(LASSO = NA, `Linear SVM` = NA, `Radial SVM` = NA) %>%
  relocate("Random Forest", .after = "Radial SVM") %>%
  column_to_rownames(var = "ASV_name")



# #Circular Heatmaps - note that trees are not accurate since they are forced into being dendrograms which causes issues in code
# ##IPAC
# temp <- subset_taxa(qatar_physeq_after, ASV_name %in% rownames(mat_IPAC) )
# taxa_names(temp) <- tax_table(temp)[,"ASV_name"]
# tree_IPAC <- force.ultrametric(phy_tree(temp))
# tree_IPAC <- as.dendrogram(tree_IPAC)
# col_fun1 = colorRamp2(c(0, 100), c("white", "red"))
# 
# circos.clear()
# circos.par(start.degree = 90)
# circos.heatmap(mat_IPAC, col = col_fun1, dend.side = "inside", cluster = tree_IPAC, rownames.side = "outside", rownames.cex = 0.3, track.height = 0.1, cell.border = "darkgray")
# lgd = Legend(title = "Importance", col_fun = col_fun1, labels_gp = gpar(fontsize = 8), title_gp = gpar(fontsize = 8))
# draw(lgd, x = unit(1,"snpc"), y = unit(0.8,"snpc"), just = "left")
# 
# ##NICU
# temp <- subset_taxa(qatar_physeq_after, ASV_name %in% rownames(mat_NICU) )
# taxa_names(temp) <- tax_table(temp)[,"ASV_name"]
# tree_NICU <- force.ultrametric(phy_tree(temp))
# tree_NICU <- as.dendrogram(tree_NICU)
# col_fun1 = colorRamp2(c(0, 100), c("white", "red"))
# 
# circos.clear()
# circos.par(start.degree = 90)
# circos.heatmap(mat_NICU, col = col_fun1, dend.side = "inside", cluster = tree_NICU, rownames.side = "outside", rownames.cex = 0.3, track.height = 0.1, cell.border = "darkgray")
# lgd = Legend(title = "Importance", col_fun = col_fun1, labels_gp = gpar(fontsize = 8), title_gp = gpar(fontsize = 8))
# draw(lgd, x = unit(1,"snpc"), y = unit(0.8,"snpc"), just = "left")
# 
# ##Surgery
# temp <- subset_taxa(qatar_physeq_after, ASV_name %in% rownames(mat_Surgery) )
# taxa_names(temp) <- tax_table(temp)[,"ASV_name"]
# tree_Surgery <- force.ultrametric(phy_tree(temp))
# tree_Surgery <- as.dendrogram(tree_Surgery)
# col_fun1 = colorRamp2(c(0, 100), c("white", "red"))
# 
# circos.clear()
# circos.par(start.degree = 90)
# circos.heatmap(mat_Surgery, col = col_fun1, dend.side = "inside", cluster = tree_Surgery, rownames.side = "outside", rownames.cex = 0.3, track.height = 0.1, cell.border = "darkgray")
# lgd = Legend(title = "Importance", col_fun = col_fun1, labels_gp = gpar(fontsize = 8), title_gp = gpar(fontsize = 8))
# draw(lgd, x = unit(1,"snpc"), y = unit(0.8,"snpc"), just = "left")
# 
# ##PICU
# temp <- subset_taxa(qatar_physeq_after, ASV_name %in% rownames(mat_PICU) )
# taxa_names(temp) <- tax_table(temp)[,"ASV_name"]
# tree_PICU <- force.ultrametric(phy_tree(temp))
# tree_PICU <- as.dendrogram(tree_PICU)
# col_fun1 = colorRamp2(c(0, 100), c("white", "red"))
# 
# circos.clear()
# circos.par(start.degree = 90)
# circos.heatmap(mat_PICU, col = col_fun1, dend.side = "inside", cluster = tree_PICU, rownames.side = "outside", rownames.cex = 0.3, track.height = 0.1, cell.border = "darkgray")
# lgd = Legend(title = "Importance", col_fun = col_fun1, labels_gp = gpar(fontsize = 8), title_gp = gpar(fontsize = 8))
# draw(lgd, x = unit(1,"snpc"), y = unit(0.8,"snpc"), just = "left")
# 
# ##Pathology
# temp <- subset_taxa(qatar_physeq_after, ASV_name %in% rownames(mat_Path) )
# taxa_names(temp) <- tax_table(temp)[,"ASV_name"]
# tree_Path <- force.ultrametric(phy_tree(temp))
# tree_Path <- as.dendrogram(tree_Path)
# col_fun1 = colorRamp2(c(0, 100), c("white", "red"))
# 
# circos.clear()
# circos.par(start.degree = 90)
# circos.heatmap(mat_Path, col = col_fun1, dend.side = "inside", cluster = tree_Path, rownames.side = "outside", rownames.cex = 0.3, track.height = 0.1, cell.border = "darkgray")
# lgd = Legend(title = "Importance", col_fun = col_fun1, labels_gp = gpar(fontsize = 8), title_gp = gpar(fontsize = 8))
# draw(lgd, x = unit(1,"snpc"), y = unit(0.8,"snpc"), just = "left")
# 
# ##All
# temp <- subset_taxa(qatar_physeq_after, ASV_name %in% mat1$ASV_name )
# taxa_names(temp) <- tax_table(temp)[,"ASV_name"]
# tree_all <- force.ultrametric(phy_tree(temp))
# tree_all <- as.dendrogram(tree_all)
# col_fun1 = colorRamp2(c(0, 100), c("white", "red"))
# split <- as.factor(c(rep("IPAC", nrow(mat_IPAC)), rep("NICU", nrow(mat_NICU)),rep("Surgery", nrow(mat_Surgery)), rep("PICU", nrow(mat_PICU)),rep("Pathology", nrow(mat_Path)), rep("Overall", nrow(mat_rf))))
# col_area_type <- structure(brewer.pal(length(levels(split)), "Set3"), names = levels(split))
# 
# circos.clear()
# circos.par(start.degree = 90)
# circos.heatmap(rbind(setNames(mat_IPAC, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")), 
#                      setNames(mat_NICU, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")), 
#                      setNames(mat_Surgery, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")), 
#                      setNames(mat_PICU, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")), 
#                      setNames(mat_Path, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")),
#                      mat_rf), 
#                col = col_fun1, split = split, dend.side = "inside", show.sector.labels = TRUE, cluster = FALSE, track.height = 0.1, cell.border = "darkgray")
# lgd = Legend(title = "Importance", col_fun = col_fun1, labels_gp = gpar(fontsize = 8), title_gp = gpar(fontsize = 8))
# draw(lgd, x = unit(1,"snpc"), y = unit(0.8,"snpc"), just = "left")
# 
# mat_all <- rbind(setNames(mat_IPAC, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")), 
#                  setNames(mat_NICU, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")), 
#                  setNames(mat_Surgery, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")), 
#                  setNames(mat_PICU, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")), 
#                  setNames(mat_Path, c("LASSO", "Linear SVM", "Radial SVM", "Random Forest")),
#                  mat_rf)
# area.heatmap <- split[rownames(mat_all) %in% mat1$ASV_name]
# mat_all <- mat_all[rownames(mat_all) %in% mat1$ASV_name,]
# 
# circos.clear()
# circos.par(start.degree = 90)
# circos.heatmap(mat_all, 
#                col = col_fun1, dend.side = "outside", cluster = tree_all, track.height = 0.1, dend.track.height = 0.3, cell.border = "darkgray")
# circos.heatmap(as.vector(area.heatmap), col = col_area_type, track.height = 0.01)
# lgd = Legend(title = "Importance", col_fun = col_fun1, labels_gp = gpar(fontsize = 8), title_gp = gpar(fontsize = 8))
# draw(lgd, x = unit(1,"snpc"), y = unit(0.8,"snpc"), just = "left")
