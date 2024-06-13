load("./Data/qatar_physeq_gg2.RData")

lasso_gg2_physeq <- prune_taxa(lassoAreaTaxaAllInformative$OTU, qatar_physeq_gg2)
lasso_gg2_taxa <- as.data.frame(tax_table(lasso_gg2_physeq)) %>% rownames_to_column()
lasso_silva_physeq <- prune_taxa(lassoAreaTaxaAllInformative$OTU, qatar_physeq)
lasso_silva_taxa <- as.data.frame(tax_table(lasso_silva_physeq)) %>% rownames_to_column()
lasso_combined_taxa <- left_join(lasso_silva_taxa, lasso_gg2_taxa, by ="rowname")

lasso_abund_gg2_physeq <- prune_taxa(taxa_names(qatar_physeq_after_merged_AreaType_abund), qatar_physeq_gg2)
lasso_abund_gg2_taxa <- as.data.frame(tax_table(lasso_abund_gg2_physeq)) %>% rownames_to_column()
lasso_abund_silva_physeq <- prune_taxa(taxa_names(qatar_physeq_after_merged_AreaType_abund), qatar_physeq)
lasso_abund_silva_taxa <- as.data.frame(tax_table(lasso_abund_silva_physeq)) %>% rownames_to_column()
lasso_abund_combined_taxa <- left_join(lasso_abund_silva_taxa, lasso_abund_gg2_taxa, by ="rowname")

core_gg2_physeq <- prune_taxa(topPrevalent_ASV$ASV, qatar_physeq_gg2)
core_gg2_taxa <- as.data.frame(tax_table(core_gg2_physeq)) %>% rownames_to_column()
core_silva_physeq <- prune_taxa(topPrevalent_ASV$ASV, qatar_physeq)
core_silva_taxa <- as.data.frame(tax_table(core_silva_physeq)) %>% rownames_to_column()
core_combined_taxa <- left_join(core_gg2_taxa, core_silva_taxa, by = "rowname" )
