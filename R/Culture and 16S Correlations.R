qatar_physeq_culture <- subset_samples(qatar_physeq_after, Sample.Name %in% cultureCount$Sample.Name)
qatar_physeq_culture <- transform_sample_counts(qatar_physeq_culture,function(x) 100*x/sum(x))

qatar_physeq_culture_Genus <- tax_glom(qatar_physeq_culture,"Genus", NArm = FALSE)
qatar_physeq_culture_Genus <- transform_sample_counts(qatar_physeq_culture_Genus,function(x) 100*x/sum(x))
qatar_physeq_culture_Genus_df <- psmelt(qatar_physeq_culture_Genus)

qatar_physeq_culture_Genus_df_RelAbund <-  aggregate(qatar_physeq_culture_Genus_df[c("Abundance")],by = list(qatar_physeq_culture_Genus_df[,"Genus"]),FUN = mean)
colnames(qatar_physeq_culture_Genus_df_RelAbund) <- c("Genus", "16S Relative Abundance")

qatar_physeq_culture_Genus_df_SD <-  aggregate(qatar_physeq_culture_Genus_df[c("Abundance")],by = list(qatar_physeq_culture_Genus_df[,"Genus"]),FUN = sd)
colnames(qatar_physeq_culture_Genus_df_SD) <- c("Genus", "Standard Deviation")
qatar_physeq_culture_Genus_df_RelAbund <- merge(qatar_physeq_culture_Genus_df_RelAbund, qatar_physeq_culture_Genus_df_SD, by = "Genus")


# Count prevalence (n = # of samples)
GenusFrequencyTable <- qatar_physeq_culture_Genus_df %>% data.frame %>% filter(Abundance > 0) %>% count_("Genus")
totalSamples <- length(unique(qatar_physeq_culture_Genus_df$Sample.Name))

#Merge prevalence with data frame
qatar_physeq_culture_Genus_df_RelAbundPrev <- merge(qatar_physeq_culture_Genus_df_RelAbund,GenusFrequencyTable, by = "Genus")

#Convert # of samples to % (p)
qatar_physeq_culture_Genus_df_RelAbundPrev$`16S Prevalence` <- qatar_physeq_culture_Genus_df_RelAbundPrev$n/totalSamples
qatar_physeq_culture_Genus_df_RelAbundPrev$neg_logp_o <- -log10(1 - qatar_physeq_culture_Genus_df_RelAbundPrev$`16S Prevalence`)
qatar_physeq_culture_Genus_df_RelAbundPrev$`16S Prevalence` <- qatar_physeq_culture_Genus_df_RelAbundPrev$`16S Prevalence`*100

cultureCountGenusSample <- aggregate(cultureCount$CFU,by = list(cultureCount$Sample.Name, cultureCount$Genus), FUN = sum, na.rm = TRUE)
colnames(cultureCountGenusSample) <- c("Sample.Name", "Genus", "CFU")
cultureCountGenusPrevalence <- aggregate(cultureCountGenusSample$CFU,by = list(cultureCountGenusSample$Genus), FUN = function(x) sum(x > 0)/length(x) * 100)
colnames(cultureCountGenusPrevalence) <- c("Genus", "Culture Prevalence")
cultureCountGenusAvg <- aggregate(cultureCount$CFU,by = list(cultureCount$Genus), FUN = mean, na.rm = TRUE, na.action = na.pass)
colnames(cultureCountGenusAvg) <- c("Genus", "Average CFU")
cultureCountGenusAvg$`Average CFU`[is.nan(cultureCountGenusAvg$`Average CFU`)] <- NA

cultureCountGenusAvg$`Culture Relative CFU` <- cultureCountGenusAvg$`Average CFU`/sum(cultureCountGenusAvg$`Average CFU`, na.rm = TRUE) * 100


cultureCountGenus <- merge(cultureCountGenusPrevalence, cultureCountGenusAvg, by = "Genus")
cultureCountGenus <- cultureCountGenus[!is.nan(cultureCountGenus$`Average CFU`),]

temp <- qatar_physeq_culture_Genus_df_RelAbundPrev[,c("Genus","16S Prevalence", "16S Relative Abundance")]

temp2 <- cultureCountGenus[cultureCountGenus$Genus != "No growth" & cultureCountGenus$Genus != "No identification", c("Genus", "Culture Prevalence", "Culture Relative CFU")]

Genus16SCulture <- merge(temp, temp2, by = "Genus", all.y = TRUE)
Genus16SCulture <- Genus16SCulture %>% mutate_at(c(2:5),~replace(., is.na(.), 0))

Genus16SCulturePrevalencePlot <- ggplot(Genus16SCulture, aes(x = `16S Prevalence`, y = `Culture Prevalence`, label = Genus)) + 
  geom_text_repel(max.overlaps = 20, 
                  size = 2, 
                  min.segment.length = unit(0, 'lines'), 
                  nudge_y = 1.5, 
                  segment.color = 'grey'
                  ) + 
  geom_point()  + 
  theme(text = element_text(family = "sans", size = 10), 
        panel.background = element_rect(fill='white', colour = 'black'), 
        axis.line = element_blank()
        ) 

cor1 <- spearman.ci(Genus16SCulture$`16S Prevalence`, Genus16SCulture$`Culture Prevalence`)

fit <- lm(`Culture Prevalence` ~ `16S Prevalence`, Genus16SCulture)
print(summary(fit))

Genus16SCultureRelAbundPlot <- ggplot(Genus16SCulture, aes(x = `16S Relative Abundance`, y = `Culture Relative CFU`, label = Genus)) +
  geom_text_repel(max.overlaps = 20, 
                  size = 2, 
                  min.segment.length = unit(0, 'lines'), 
                  segment.color = 'grey'
                  ) + 
  geom_point() + 
  theme(text = element_text(family = "sans", size = 10), 
        panel.background = element_rect(fill='white', colour = 'black'), 
        axis.line = element_blank()
        )

cor2 <- spearman.ci(Genus16SCulture$`16S Relative Abundance`, Genus16SCulture$`Culture Relative CFU`)

fit <- lm(`Culture Relative CFU` ~ `16S Relative Abundance`, Genus16SCulture)
print(summary(fit))

Genus16SCultureCorrelations <- data.frame(Measure = c("Prevalence","Relative Abundance"), Correlation = c(cor1$estimate, cor2$estimate), Lower = c(cor1$conf.int[1],cor2$conf.int[1]), Upper = c(cor1$conf.int[2],cor2$conf.int[2]))

Genus16SCultureCorrelationsPlot <- ggplot(Genus16SCultureCorrelations, aes(x = Measure, y = Correlation)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) + 
  theme(text = element_text(family = "sans", size = 10),
        panel.background = element_rect(fill='white', colour = 'black'), 
        axis.line = element_blank()
        ) + 
  ylab("Spearman Correlation") + 
  ylim(-0.3, 1)
print(Genus16SCultureCorrelationsPlot)

Genus16SCulturePlots <- ggarrange(Genus16SCulturePrevalencePlot, Genus16SCultureRelAbundPlot,
                                  labels = c("(A)", "(B)"),
                                  font.label = list(face = "plain", size = 12),
                                  ncol = 1, nrow = 2, 
                                  align = "v",
                                  hjust = 0.05,
                                  vjust = 1)
print(Genus16SCulturePlots)

