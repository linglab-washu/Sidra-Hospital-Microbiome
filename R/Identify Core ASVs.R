qatar_physeq_rarefied_RelAbund <- transform_sample_counts(qatar_physeq_rarefied,function(x) 100*x/sum(x))
qatar_physeq_rarefied_RelAbund_df <- psmelt(qatar_physeq_rarefied_RelAbund)
OTU_taxonomy <- as.data.frame(tax_table(qatar_physeq_rarefied_RelAbund))

qatar_physeq_rarefied_df_ASV_RelAbund <-  aggregate(qatar_physeq_rarefied_RelAbund_df[c("Abundance")],by = list(qatar_physeq_rarefied_RelAbund_df[,"OTU"]),FUN = mean)
colnames(qatar_physeq_rarefied_df_ASV_RelAbund) <- c("ASV", "Relative Abundance")
qatar_physeq_rarefied_df_ASV_SD <-  aggregate(qatar_physeq_rarefied_RelAbund_df[c("Abundance")],by = list(qatar_physeq_rarefied_RelAbund_df[,"OTU"]),FUN = sd)
colnames(qatar_physeq_rarefied_df_ASV_SD) <- c("ASV", "Standard Deviation")
qatar_physeq_rarefied_df_ASV_RelAbund <- merge(qatar_physeq_rarefied_df_ASV_RelAbund, qatar_physeq_rarefied_df_ASV_SD, by = "ASV")
qatar_physeq_rarefied_df_ASV_RelAbund<- qatar_physeq_rarefied_df_ASV_RelAbund[order(-qatar_physeq_rarefied_df_ASV_RelAbund$`Relative Abundance`),]

top20ASV <- qatar_physeq_rarefied_df_ASV_RelAbund$ASV[1:20]

# Count prevalence (n = # of samples)
ASVFrequencyTable <- qatar_physeq_rarefied_RelAbund_df %>% data.frame %>% filter(Abundance > 0) %>% plyr::count("OTU")  

#Merge prevalence with data frame
qatar_physeq_rarefied_df_ASV_RelAbundPrev <- merge(qatar_physeq_rarefied_df_ASV_RelAbund,ASVFrequencyTable, by.x = "ASV", by.y = "OTU")
qatar_physeq_rarefied_df_ASV_RelAbundPrev <- qatar_physeq_rarefied_df_ASV_RelAbundPrev[order(-qatar_physeq_rarefied_df_ASV_RelAbundPrev$`Relative Abundance`),]

#Convert # of samples to % (p)
qatar_physeq_rarefied_df_ASV_RelAbundPrev$`Prevalence` <- qatar_physeq_rarefied_df_ASV_RelAbundPrev$freq/nsamples(qatar_physeq_rarefied)
qatar_physeq_rarefied_df_ASV_RelAbundPrev$neg_logp_o <- -log10(1 - qatar_physeq_rarefied_df_ASV_RelAbundPrev$`Prevalence`)
qatar_physeq_rarefied_df_ASV_RelAbundPrev$`Prevalence` <- qatar_physeq_rarefied_df_ASV_RelAbundPrev$`Prevalence`*100

#Add taxa info
qatar_physeq_rarefied_df_ASV_RelAbundPrev <- merge(qatar_physeq_rarefied_df_ASV_RelAbundPrev, OTU_taxonomy, by.x = "ASV", by.y = "row.names")

#List only top 5 Phylum 
top20Phylum <- OTU_taxonomy[row.names(OTU_taxonomy) %in% top20phylum, "Phylum"]
qatar_physeq_rarefied_df_ASV_RelAbundPrev[!(qatar_physeq_rarefied_df_ASV_RelAbundPrev$Phylum %in% top20Phylum[1:5]), "Phylum"] <- "Other"

#Prevalence-Abundance Plot
relAbundPrevPlotASV <- ggplot(qatar_physeq_rarefied_df_ASV_RelAbundPrev,aes(Prevalence, `Relative Abundance`)) + geom_point() + scale_y_log10(labels = scales::comma)  + xlab("Prevalence (%)") + ylab("Relative Abundance (%)") + theme(panel.background = element_rect(fill='white', colour = 'black'), axis.line = element_blank())
print(relAbundPrevPlotASV)

print(
  cor.test(
    qatar_physeq_rarefied_df_ASV_RelAbundPrev[, "Prevalence"],
    qatar_physeq_rarefied_df_ASV_RelAbundPrev[, "Relative Abundance"],
    method = "spearman"
  )
)
#Histogram of relative abundance
relAbundHistogramASV <- ggplot(qatar_physeq_rarefied_df_ASV_RelAbundPrev, aes(`Relative Abundance`)) + geom_histogram(binwidth = 0.5) + ylab("Number of ASVs") + theme(panel.background = element_rect(fill='white', colour = 'black'), axis.line = element_blank())
print(relAbundHistogramASV)

#Histogram of prevalence
prevHistogramASV <- ggplot(qatar_physeq_rarefied_df_ASV_RelAbundPrev, aes(Prevalence)) + geom_histogram(binwidth = 2) + scale_y_log10() + ylab("Number of ASVs") + theme(panel.background = element_rect(fill='white', colour = 'black'), axis.line = element_blank())
print(prevHistogramASV)

#Top Prevalent (based off 2nd peak in histogram)
topPrevalent_ASV <- qatar_physeq_rarefied_df_ASV_RelAbundPrev[qatar_physeq_rarefied_df_ASV_RelAbundPrev$Prevalence > 50,]

pantone.col <- c("#9A2B2E", "#FBC85F", "#F96815", "#C042BA","#207329", "#F0BDD5", "#96D8DE", "864C24", "#325B74", "#726A4E")
legend_ord <- c("Actinobacteriota", "Bacteroidota", "Firmicutes", "Fusobacteriota", "Proteobacteria")

barPlotRelAbundTopPrevalentASV <- ggplot(topPrevalent_ASV,aes(reorder(Genus,`Relative Abundance`, sum),`Relative Abundance`, fill = Phylum)) + geom_bar(stat = "identity") + coord_flip()  + xlab("Genuses of Core ASV") + ylab("Relative Abundance (%)") + scale_fill_manual(values = pantone.col, name = "Phylum", breaks = legend_ord) + theme(panel.background = element_rect(fill='white', colour = 'black'), axis.line = element_blank())
print(barPlotRelAbundTopPrevalentASV )

print(sum(topPrevalent_ASV$`Relative Abundance`))

#Top 10 by Abundance
top10ASV <- qatar_physeq_rarefied_df_ASV_RelAbundPrev[order(-qatar_physeq_rarefied_df_ASV_RelAbundPrev$`Relative Abundance`),][1:10,"ASV"]