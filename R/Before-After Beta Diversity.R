qatar_beforeafter_PCoA_ord <- ordinate(qatar_physeq_beforeafter_RelAbund, "PCoA", "bray")

#PCoA
PCoABeforeAfterByOpeningOTU <- plot_ordination(qatar_physeq_beforeafter_RelAbund, qatar_beforeafter_PCoA_ord, shape = "Hospital.Opening") + 
  scale_shape_manual(values = c(16, 1), name = "Hospital Opening") + 
  theme(panel.background = element_rect(fill='white', colour = 'black'), 
        axis.line = element_blank()
        ) + 
  xlab(paste("PCo1 [",round(qatar_beforeafter_PCoA_ord$values$Relative_eig[1]*100,1),"%]",sep = "")) + 
  ylab(paste("PCo2 [",round(qatar_beforeafter_PCoA_ord$values$Relative_eig[2]*100,1),"%]",sep = ""))

PCoABeforeAfterBySurface <- plot_ordination(qatar_physeq_beforeafter_RelAbund, qatar_beforeafter_PCoA_ord, shape = "Hospital.Opening") + 
  scale_shape_manual(values = c(16, 1), name = "Hospital Opening") + 
  facet_wrap(~Surface.Type.II) + 
  theme(panel.border = element_rect(fill=NA,color="#5c5c5c", size=0.5, linetype="solid"), 
        strip.background = element_blank()
        )  + 
  xlab(paste("PCo1 [",round(qatar_beforeafter_PCoA_ord$values$Relative_eig[1]*100,1),"%]",sep = "")) + 
  ylab(paste("PCo2 [",round(qatar_beforeafter_PCoA_ord$values$Relative_eig[2]*100,1),"%]",sep = ""))

PCoABeforeAfterByOpeningGrid <- ggarrange(PCoABeforeAfterByOpeningOTU, PCoABeforeAfterBySurface, 
                                          labels = c("(A)", "(B)"),
                                          font.label = list(face = "plain"),
                                          common.legend = TRUE,
                                          legend = "right",
                                          ncol = 1, nrow = 2,
                                          hjust = 0.05,
                                          vjust = 1
)
print(PCoABeforeAfterByOpeningGrid)

sampledf <- data.frame(sample_data(qatar_physeq_beforeafter_RelAbund))
qatar_beforeafter_bray <- phyloseq::distance(qatar_physeq_beforeafter_RelAbund, method = "bray")

## PERMANOVA
permOpening <- adonis(qatar_beforeafter_bray ~ Hospital.Opening, data = sampledf)
perm_df_Opening <- permOpening$aov.tab$Df[1] 
perm_f_Opening <- permOpening$aov.tab$F.Model[1]
perm_r2_Opening <- permOpening$aov.tab$R2[1]
perm_p_Opening <- permOpening$aov.tab$`Pr(>F)`[1]

## Beta Dispersion
beta <- betadisper(qatar_beforeafter_bray, sampledf$Hospital.Opening)
disperOpening <- permutest(beta)
disper_f_Opening <- disperOpening$tab$F[1]
disper_p_Opening <- disperOpening$tab$`Pr(>F)`[1]

perm.pOpening <- tibble(Grouping = "Hospital Opening", perm_df_Opening, perm_f_Opening, perm_r2_Opening, perm_p_Opening, disper_f_Opening, disper_p_Opening)