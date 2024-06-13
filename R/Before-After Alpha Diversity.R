alphaDiversityBeforeAfterOTU <- estimate_richness(qatar_physeq_beforeafter, measures = c("Observed", "Shannon","Simpson"))

tmp <- t(otu_table(qatar_physeq_beforeafter))
alphaDiversityBeforeAfterOTU$Pielou <- alphaDiversityBeforeAfterOTU$Shannon/log(specnumber(tmp))

alphaDiversityBeforeAfterOTU <- merge(alphaDiversityBeforeAfterOTU, mdata, by = 0)
alphaDiversityBeforeAfterOTU <- data.frame(alphaDiversityBeforeAfterOTU)


#Figures
plot_jittered_boxplot <- function(df, x.value, y.value, fill.value) {
  if (missing(fill.value)){
    p <- ggplot(df, aes_string(x = x.value, y = y.value))
  } else {
    p <- ggplot(df, aes_string(x = x.value, y = y.value, fill = fill.value))
  }
  p +
    geom_half_boxplot(nudge = 0.2, outlier.color = NA) +
    theme(panel.background = element_rect(fill='white', colour = 'black'), axis.line = element_blank()) +
    geom_jitter(size=1, position = position_jitter(width=.1, height=0))+
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 1)) +
    scale_fill_manual(values = c("#ffaf12","#34558b")) +
    scale_color_manual(values = c("#34558b","#ffaf12")) +
    labs(x=NULL)+
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(text = element_text(family = "sans", size = 12))
}

alphaDiversityBeforeAfterOTU1Plot <- plot_jittered_boxplot(alphaDiversityBeforeAfterOTU, "Hospital.Opening", "Observed") + ylab("ASV Richness")

alphaDiversityBeforeAfterOTU2Plot <- plot_jittered_boxplot(alphaDiversityBeforeAfterOTU, "Hospital.Opening", "Shannon") + ylab("Shannon Index")

alphaDiversityBeforeAfterOTU3Plot <- plot_jittered_boxplot(alphaDiversityBeforeAfterOTU, "Hospital.Opening", "Pielou") + ylab("Pielou's Evenness")

alphaDiversityBeforeAfterOTUPlot <- ggarrange(alphaDiversityBeforeAfterOTU1Plot, alphaDiversityBeforeAfterOTU2Plot, alphaDiversityBeforeAfterOTU3Plot, 
                                              labels = c("(A)", "(B)", "(C)"),
                                              font.label = list(face = "plain"),
                                              ncol = 3, nrow = 1, 
                                              align = "v")
print(alphaDiversityBeforeAfterOTUPlot)


####Statistical Test on Alpha Diversities

#Wilcox u test
wcBeforeAfter1 <- wilcox.test(Observed ~ Hospital.Opening, data = alphaDiversityBeforeAfterOTU, paired = TRUE)
wcBeforeAfter2 <- wilcox.test(Shannon ~ Hospital.Opening, data = alphaDiversityBeforeAfterOTU, paired = TRUE)
wcBeforeAfter3 <- wilcox.test(Pielou ~ Hospital.Opening, data = alphaDiversityBeforeAfterOTU, paired = TRUE)

wc.p <- tibble("Hospital Opening", wcBeforeAfter1$statistic, wcBeforeAfter1$p.value, wcBeforeAfter2$statistic, wcBeforeAfter2$p.value, wcBeforeAfter3$statistic, wcBeforeAfter3$p.value)
colnames(wc.p)[-1] <- c("Observed U", "Observed","Shannon U", "Shannon", "Pielou U", "Pielou")
