qatar_physeq_after_merged_AreaType_inform <-  prune_taxa(lassoAreaTaxaAllInformative$OTU, qatar_physeq_after_merged_AreaType)
rankLabel <- "Most Informative ASVs"
colnames(tax_table(qatar_physeq_after_merged_AreaType_inform))[8] <- rankLabel
numColors <- length(unique(as.data.frame(tax_table(qatar_physeq_after_merged_AreaType_inform))$Class))
myPalette <- brewer.pal(n = numColors, name = "Dark2")
names(myPalette) <- unique(as.data.frame(tax_table(qatar_physeq_after_merged_AreaType_inform))$Class) # Give every color an appropriate name


lassoAreaPlotInform <- plot_heatmap(qatar_physeq_after_merged_AreaType_inform, 
                                          method = NULL, 
                                          sample.order = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"), 
                                          taxa.order = rev(lassoAreaTaxaAllInformative$OTU[lassoAreaTaxaAllInformative$OTU != "(Intercept)"]),
                                          taxa.label = rankLabel
) +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", trans = scales::log_trans(4), labels = scales::number_format(accuracy = 0.0001)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank(),
        text = element_text(family = "sans", size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(face = "italic", size = 8, color = myPalette[as.data.frame(tax_table(qatar_physeq_after_merged_AreaType_inform))$Class])) + 
  guides(fill = guide_colourbar(barwidth = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  #scale_y_discrete(labels = function(x) str_wrap(x, width = 35), name = paste("Informative ASVs\n in the ", y.AreaName[i])) +
  xlab("Area Type") +
  labs(fill = "Relative \nAbundance (%)")
print(lassoAreaPlotInform)


#Most Abundant
qatar_physeq_after_merged_AreaType_abund <-  prune_taxa(lassoAreaTaxaAll[lassoAreaTaxaAll$Value > 0,"OTU"], qatar_physeq_after_merged_AreaType)
qatar_physeq_after_merged_AreaType_abund <- prune_taxa(taxa_sums(qatar_physeq_after_merged_AreaType_abund) > 0.02, qatar_physeq_after_merged_AreaType_abund)
rankLabel <- "Most Abundant Positively Informative ASVs"
colnames(tax_table(qatar_physeq_after_merged_AreaType_abund))[8] <- rankLabel
numColors <- length(unique(as.data.frame(tax_table(qatar_physeq_after_merged_AreaType_abund))$Class))
myPalette <- color("muted")(numColors)# brewer.pal(n = numColors, name = "Set3")
names(myPalette) <- unique(as.data.frame(tax_table(qatar_physeq_after_merged_AreaType_abund))$Class) # Give every color an appropriate name
#myPalette[[7]] <- "#DDD53C"
#myPalette[[6]] <- "#B7D093"
ASV_order <- lassoAreaTaxaAll[(lassoAreaTaxaAll$OTU %in% taxa_names(qatar_physeq_after_merged_AreaType_abund)) & lassoAreaTaxaAll$Value > 0, "OTU"]
#ASV_order <- ASV_order[-c(17,29:31)]

tmp_df <- as.data.frame(tax_table(qatar_physeq_after_merged_AreaType_abund))
tmp_df <- tmp_df[rev(ASV_order),]
lassoAreaPlotAbund <- plot_heatmap(qatar_physeq_after_merged_AreaType_abund, 
                                    method = NULL, 
                                    sample.order = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"), 
                                    taxa.order = rev(ASV_order),
                                    taxa.label = rankLabel
) +
  scale_fill_gradient(low = "white", high = "black", na.value = "white", trans = scales::log_trans(4), labels = scales::number_format(accuracy = 0.0001)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank(),
        text = element_text(family = "sans", size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(face = "italic", size = 8, color = myPalette[tmp_df$Class])) + 
  guides(fill = guide_colourbar(barwidth = 0.5)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
  #scale_y_discrete(labels = function(x) str_wrap(x, width = 35), name = paste("Informative ASVs\n in the ", y.AreaName[i])) +
  xlab("Area Type") +
  labs(fill = "Relative \nAbundance (%)")
print(lassoAreaPlotAbund)