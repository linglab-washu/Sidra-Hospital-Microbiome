qatar_after_PCoA_ord_uni = ordinate(qatar_physeq_after_RelAbund, "PCoA", "unifrac")

PCoAAfterByAreaUni <-
  plot_ordination(
    qatar_physeq_after_RelAbund,
    qatar_after_PCoA_ord_uni,
    color = "Area.Type",
    shape = "Area.Type"
  ) + 
  stat_ellipse(aes(group = Group, lty = Group), type="norm") +
  scale_linetype_manual(values = c(2,3)) +
  scale_shape_manual(values = c(15, 15, 19, 2, 1, 1),
                     name = "Area Type",
                     breaks = legend_ord) + 
  geom_point(size = 3, stroke = 1.5) + 
  scale_color_manual(values = area.pantone.col,
                     name = "Area Type",
                     breaks = legend_ord)  + 
  guides(shape = guide_legend(override.aes = list(linetype = 0))) +
  theme(panel.border = element_rect(fill = NA, colour = 'black'),
        axis.line = element_blank()) + 
  xlab(paste("PCo1 [", round(qatar_after_PCoA_ord_uni$values$Relative_eig[1] * 100, 1), "%]", sep = "")) +
  ylab(paste("PCo2 [", round(qatar_after_PCoA_ord_uni$values$Relative_eig[2] * 100, 1),  "%]", sep = "")) +
  theme(text = element_text(family = "sans", size = 14))
PCoAAfterByAreaUni$layers[[1]] <- NULL

qatar_after_PCoA_ord_weight_uni = ordinate(qatar_physeq_after_RelAbund, "PCoA", "unifrac", weighted = TRUE)

PCoAAfterByAreaWeightUni <-
  plot_ordination(
    qatar_physeq_after_RelAbund,
    qatar_after_PCoA_ord_weight_uni,
    color = "Area.Type",
    shape = "Area.Type"
  ) + 
  stat_ellipse(aes(group = Group, lty = Group), type="norm") +
  scale_linetype_manual(values = c(2,3)) +
  scale_shape_manual(values = c(15, 15, 19, 2, 1, 1),
                     name = "Area Type",
                     breaks = legend_ord) + 
  geom_point(size = 3, stroke = 1.5) + 
  scale_color_manual(values = area.pantone.col,
                     name = "Area Type",
                     breaks = legend_ord)  + 
  guides(shape = guide_legend(override.aes = list(linetype = 0))) +
  theme(panel.border = element_rect(fill = NA, colour = 'black'),
        axis.line = element_blank()) + 
  xlab(paste("PCo1 [", round(qatar_after_PCoA_ord_uni$values$Relative_eig[1] * 100, 1), "%]", sep = "")) +
  ylab(paste("PCo2 [", round(qatar_after_PCoA_ord_uni$values$Relative_eig[2] * 100, 1),  "%]", sep = "")) +
  theme(text = element_text(family = "sans", size = 14))
PCoAAfterByAreaWeightUni$layers[[1]] <- NULL

qatar_after_PCoA_ord_jac = ordinate(qatar_physeq_after_RelAbund, "PCoA", "jaccard", binary = TRUE)

PCoAAfterByAreaJac <-
  plot_ordination(
    qatar_physeq_after_RelAbund,
    qatar_after_PCoA_ord_jac,
    color = "Area.Type",
    shape = "Area.Type"
  ) + 
  stat_ellipse(aes(group = Group, lty = Group), type="norm") +
  scale_linetype_manual(values = c(2,3)) +
  scale_shape_manual(values = c(15, 15, 19, 2, 1, 1),
                     name = "Area Type",
                     breaks = legend_ord) + 
  geom_point(size = 3, stroke = 1.5) + 
  scale_color_manual(values = area.pantone.col,
                     name = "Area Type",
                     breaks = legend_ord)  + 
  guides(shape = guide_legend(override.aes = list(linetype = 0))) +
  theme(panel.border = element_rect(fill = NA, colour = 'black'),
        axis.line = element_blank()) + 
  xlab(paste("PCo1 [", round(qatar_after_PCoA_ord_jac$values$Relative_eig[1] * 100, 1), "%]", sep = "")) +
  ylab(paste("PCo2 [", round(qatar_after_PCoA_ord_jac$values$Relative_eig[2] * 100, 1),  "%]", sep = "")) +
  theme(text = element_text(family = "sans", size = 14))
PCoAAfterByAreaJac$layers[[1]] <- NULL


PCoAAfterByAreaUniJacGrid <- ggarrange(PCoAAfterByAreaUni + ggtitle("Unweighted UniFrac"), PCoAAfterByAreaWeightUni + ggtitle("Weighted UniFrac"), PCoAAfterByAreaJac + ggtitle("Jaccard"),
                                     labels = c("(A)", "(B)","(C)"),
                                     font.label = list(face = "plain"),
                                     common.legend = TRUE,
                                     legend = "right",
                                     ncol = 1, nrow = 3,
                                     hjust = 0.05,
                                     vjust = 1
)
print(PCoAAfterByAreaUniJacGrid)

#NMDS
set.seed(1938)
qatar_after_NMDS_ord_bray1 = ordinate(qatar_physeq_after_RelAbund_Sqrt, "NMDS", "bray", k = 3, trace = FALSE)

NMDSAfterByAreaBray1 <-
  plot_ordination(
    qatar_physeq_after_RelAbund_Sqrt,
    qatar_after_NMDS_ord_bray1, axes = 2:3,
    color = "Area.Type",
    shape = "Area.Type",
  ) + 
  stat_ellipse(aes(group = Group, lty = Group), type="norm") +
  scale_linetype_manual(values = c(2,3)) +
  scale_shape_manual(values = c(15, 15, 19, 2, 1, 1),
                     name = "Area Type",
                     breaks = legend_ord) + 
  geom_point(size = 3, stroke = 1.5) + 
  scale_color_manual(values = area.pantone.col,
                     name = "Area Type",
                     breaks = legend_ord)  + 
  guides(shape = guide_legend(override.aes = list(linetype = 0))) +
  theme(panel.border = element_rect(fill = NA, colour = 'black'),
        axis.line = element_blank()) + 
  theme(text = element_text(family = "sans", size = 14))
NMDSAfterByAreaBray1$layers[[1]] <- NULL

qatar_physeq_after_RelAbund_Sqrt_subset <- subset_samples(qatar_physeq_after_RelAbund_Sqrt, Sample.Name != "PATV1W2-S5")
set.seed(1938)
qatar_after_NMDS_ord_bray2 = ordinate(qatar_physeq_after_RelAbund_Sqrt_subset, "NMDS", "bray", k = 2, trace = FALSE)

NMDSAfterByAreaBray2 <-
  plot_ordination(
    qatar_physeq_after_RelAbund_Sqrt,
    qatar_after_NMDS_ord_bray2,
    color = "Area.Type",
    shape = "Area.Type",
  ) + 
  stat_ellipse(aes(group = Group, lty = Group), type="norm") +
  scale_linetype_manual(values = c(2,3)) +
  scale_shape_manual(values = c(15, 15, 19, 2, 1, 1),
                     name = "Area Type",
                     breaks = legend_ord) + 
  geom_point(size = 3, stroke = 1.5) + 
  scale_color_manual(values = area.pantone.col,
                     name = "Area Type",
                     breaks = legend_ord)  + 
  guides(shape = guide_legend(override.aes = list(linetype = 0))) +
  theme(panel.border = element_rect(fill = NA, colour = 'black'),
        axis.line = element_blank()) + 
  theme(text = element_text(family = "sans", size = 14))
NMDSAfterByAreaBray2$layers[[1]] <- NULL

NMDSAfterByAreaBrayGrid <- ggarrange(NMDSAfterByAreaBray1, NMDSAfterByAreaBray2, 
                                          labels = c("(A)", "(B)"),
                                          font.label = list(face = "plain"),
                                          common.legend = TRUE,
                                          legend = "right",
                                          ncol = 1, nrow = 2,
                                          hjust = 0.05,
                                          vjust = 1
)
print(NMDSAfterByAreaBrayGrid)