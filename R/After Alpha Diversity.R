alphaDiversityAfterOTU <- estimate_richness(qatar_physeq_after, measures = c("Observed", "Shannon"))

tmp <- t(otu_table(qatar_physeq_after))
alphaDiversityAfterOTU$Pielou <- alphaDiversityAfterOTU$Shannon/log(specnumber(tmp))

alphaDiversityAfterOTU <- rownames_to_column(alphaDiversityAfterOTU)
colnames(alphaDiversityAfterOTU)[1] <- "samples"
alphaDiversityAfterOTU[1:9,1] <- substring(alphaDiversityAfterOTU[1:9,1],2)
alphaDiversityAfterOTU <- column_to_rownames(alphaDiversityAfterOTU, var = "samples")

alphaDiversityAfterOTU <- merge(alphaDiversityAfterOTU, mdata, by = 0)
alphaDiversityAfterOTU <- data.frame(alphaDiversityAfterOTU)

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
    #geom_half_point(size=0.0, range_scale=1.0, transformation = position_jitter(width=.1, height=0)) +
    geom_jitter(size=1, position = position_jitter(width=.1, height=0))+
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 1)) +
    scale_fill_manual(values = c("#ffaf12","#34558b")) +
    scale_color_manual(values = c("#34558b","#ffaf12")) +
    labs(x=NULL)+
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(text = element_text(family = "sans", size = 12))
}

plot_jittered_boxplot_ordered <- function(df, x.value, y.value, fill.value) {
  if (missing(fill.value)){
    p <- ggplot(df, aes_string(x = paste("reorder(",x.value,",", y.value,", FUN = median, na.rm = TRUE)"), y = y.value))
  } else {
    p <- ggplot(df, aes_string(x = paste("reorder(",x.value,",", y.value,", FUN = median, na.rm = TRUE)"), y = y.value, fill = fill.value))
  }
  set.seed(12)
  p +
    geom_half_boxplot(nudge = 0.2, outlier.color = NA) +
    theme(panel.background = element_rect(fill='white', colour = 'black'), axis.line = element_blank()) +
    #geom_half_point(size=0.0, range_scale=1.0, transformation = position_jitter(width=.1, height=0)) +
    geom_jitter(size=1, position = position_jitter(width=.1, height=0))+
    theme(legend.position = "bottom") +
    guides(color = guide_legend(nrow = 1)) +
    scale_fill_manual(values = c("#ffaf12","#34558b")) +
    scale_color_manual(values = c("#34558b","#ffaf12")) +
    labs(x=NULL)+
    theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(text = element_text(family = "sans", size = 12))
}

alphaDiversityAfterAreaOTU1Plot <- plot_jittered_boxplot_ordered(alphaDiversityAfterOTU, "Area.Type", "Observed") + 
  xlab("Area Type") + ylab("ASV Richness") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme(axis.text = element_text(family = "sans", size = 12))

alphaDiversityAfterAreaOTU2Plot <- plot_jittered_boxplot(alphaDiversityAfterOTU, "Area.Type", "Shannon") + 
  xlab("Area Type") + ylab("Shannon Index") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme(axis.text = element_text(family = "sans", size = 12))

alphaDiversityAfterAreaOTU3Plot <- plot_jittered_boxplot(alphaDiversityAfterOTU, "Area.Type", "Pielou") + 
  xlab("Area Type") + 
  ylab("Pielou's Evenness") + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  theme(axis.text = element_text(family = "sans", size = 12))

alphaDiversityAfterAreaOTUPlot <- ggarrange(alphaDiversityAfterAreaOTU1Plot + rremove("x.text") + rremove("xlab"), alphaDiversityAfterAreaOTU2Plot + rremove("x.text") + rremove("xlab"), alphaDiversityAfterAreaOTU3Plot, 
                                            labels = c("(A)", "(B)", "(C)"),
                                            font.label = list(face = "plain"),
                                            ncol = 1, nrow = 3, 
                                            align = "v",
                                            hjust = 0.05,
                                            vjust = 1)
print(alphaDiversityAfterAreaOTUPlot)

#Statistical Tests
kwdf <- c()
chi1 <- c()
p1 <- c()
chi2 <- c()
p2 <- c()
chi3 <- c()
p3 <- c()

#Surface Type

##Kruskal Wallis test
kwSurface1 <- kruskal.test(Observed ~ Surface.Type.I, data =alphaDiversityAfterOTU)
kwSurface2 <- kruskal.test(Shannon ~ Surface.Type.I, data = alphaDiversityAfterOTU)
kwSurface3 <- kruskal.test(Pielou ~ Surface.Type.I, data = alphaDiversityAfterOTU)

kwdf <- c(kwdf,kwSurface1$parameter[[1]])
chi1 <- c(chi1,kwSurface1$statistic[[1]])
p1 <- c(p1,kwSurface1$p.value)
chi2 <- c(chi2,kwSurface2$statistic[[1]])
p2 <- c(p2,kwSurface2$p.value)
chi3 <- c(chi3,kwSurface3$statistic[[1]])
p3 <- c(p3,kwSurface3$p.value)

#Area Type

##Kruskal Wallis test
kwArea1 <- kruskal.test(Observed ~ Area.Type, data = alphaDiversityAfterOTU)
kwArea2 <- kruskal.test(Shannon ~ Area.Type, data = alphaDiversityAfterOTU)
kwArea3 <- kruskal.test(Pielou ~ Area.Type, data = alphaDiversityAfterOTU)

kwdf <- c(kwdf,kwArea1$parameter[[1]])
chi1 <- c(chi1,kwArea1$statistic[[1]])
p1 <- c(p1,kwArea1$p.value)
chi2 <- c(chi2,kwArea2$statistic[[1]])
p2 <- c(p2,kwArea2$p.value)
chi3 <- c(chi3,kwArea3$statistic[[1]])
p3 <- c(p3,kwArea3$p.value)

##pairwise Wilcox test
###p val
pw.wilArea1 <- pairwise.wilcox.test(alphaDiversityAfterOTU$Observed, alphaDiversityAfterOTU$Area.Type, p.adj = "none")
pw.wilArea2 <- pairwise.wilcox.test(alphaDiversityAfterOTU$Shannon, alphaDiversityAfterOTU$Area.Type, p.adj = "none")
pw.wilArea3 <- pairwise.wilcox.test(alphaDiversityAfterOTU$Pielou, alphaDiversityAfterOTU$Area.Type, p.adj = "none")

pw.wilArea1p.val <- as.data.frame(pw.wilArea1$p.value)
pw.wilArea1p.val %<>% rownames_to_column("Area Type 1") %>% pivot_longer(-c("Area Type 1"), names_to = "Area Type 2", values_to = "Observed p")
pw.wilArea1p.val <- pw.wilArea1p.val[is.na(pw.wilArea1p.val$`Observed p`) == FALSE, ]
pw.wilArea1p.val <- unite(pw.wilArea1p.val,c("Area Type 1", "Area Type 2"), col = "Pairing", sep = " vs ", remove = FALSE)

pw.wilArea2p.val <- as.data.frame(pw.wilArea2$p.value)
pw.wilArea2p.val %<>% rownames_to_column("Area Type 1") %>% pivot_longer(-c("Area Type 1"), names_to = "Area Type 2", values_to = "Shannon p")
pw.wilArea2p.val <- pw.wilArea2p.val[is.na(pw.wilArea2p.val$`Shannon p`) == FALSE, ]
pw.wilArea2p.val <- unite(pw.wilArea2p.val,c("Area Type 1", "Area Type 2"), col = "Pairing", sep = " vs ", remove = FALSE)

pw.wilArea3p.val <- as.data.frame(pw.wilArea3$p.value)
pw.wilArea3p.val %<>% rownames_to_column("Area Type 1") %>% pivot_longer(-c("Area Type 1"), names_to = "Area Type 2", values_to = "Pielou p")
pw.wilArea3p.val <- pw.wilArea3p.val[is.na(pw.wilArea3p.val$`Pielou p`) == FALSE, ]
pw.wilArea3p.val <- unite(pw.wilArea3p.val,c("Area Type 1", "Area Type 2"), col = "Pairing", sep = " vs ", remove = FALSE)

###p adjusted
pw.wiladjArea1 <- pairwise.wilcox.test(alphaDiversityAfterOTU$Observed, alphaDiversityAfterOTU$Area.Type, p.adj = "BH")
pw.wiladjArea2 <- pairwise.wilcox.test(alphaDiversityAfterOTU$Shannon, alphaDiversityAfterOTU$Area.Type, p.adj = "BH")
pw.wiladjArea3 <- pairwise.wilcox.test(alphaDiversityAfterOTU$Pielou, alphaDiversityAfterOTU$Area.Type, p.adj = "BH")

pw.wiladjArea1p.val <- as.data.frame(pw.wiladjArea1$p.value)
pw.wiladjArea1p.val <- rbind(rep(NA,5),pw.wiladjArea1p.val)
pw.wiladjArea1p.val <- cbind(pw.wiladjArea1p.val, rep(NA,6))
rownames(pw.wiladjArea1p.val)[1] <- "IPAC Offices"
colnames(pw.wiladjArea1p.val)[6] <- "Microbiology Lab"
pw.wiladjArea1p.val %<>% rownames_to_column("Area Type 1") %>% pivot_longer(-c("Area Type 1"), names_to = "Area Type 2", values_to = "Observed p_adjusted")
pw.wiladjArea1p.valPlotData <- pw.wiladjArea1p.val
pw.wiladjArea1p.valPlotData$`Area Type 1` <- factor(pw.wiladjArea1p.valPlotData$`Area Type 1`, levels = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"))
pw.wiladjArea1p.valPlotData$`Area Type 2` <- factor(pw.wiladjArea1p.valPlotData$`Area Type 2`, levels = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"))

pw.wiladjArea1p.valPlotData$`Adjusted p-value` <- ifelse(pw.wiladjArea1p.valPlotData$`Observed p_adjusted` < 0.05, "p < 0.05", ifelse(pw.wiladjArea1p.valPlotData$`Observed p_adjusted` < 0.1, "0.05 < p < 0.1","p > 0.1"))
pw.wiladjArea1p.val <- pw.wiladjArea1p.val[is.na(pw.wiladjArea1p.val$`Observed p_adjusted`) == FALSE, ]

cols  <- c(
  "p < 0.05"  = "#000000",
  "0.05 < p < 0.1" = "#4c4c4c",
  "p > 0.1" = "#808080"
)
pw.wiladjArea1p.valPlot <- ggplot(pw.wiladjArea1p.valPlotData,aes(x = `Area Type 1`, y = `Area Type 2`, fill = `Adjusted p-value`)) + 
  scale_fill_manual(values = cols, na.value = "#FFFFFF") + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank())
print(pw.wiladjArea1p.valPlot)

pw.wiladjArea1p.val <- unite(pw.wiladjArea1p.val,c("Area Type 1", "Area Type 2"), col = "Pairing", sep = " vs ", remove = FALSE)

pw.wiladjArea2p.val <- as.data.frame(pw.wiladjArea2$p.value)
pw.wiladjArea2p.val <- rbind(rep(NA,5),pw.wiladjArea2p.val)
pw.wiladjArea2p.val <- cbind(pw.wiladjArea2p.val, rep(NA,6))
rownames(pw.wiladjArea2p.val)[1] <- "IPAC Offices"
colnames(pw.wiladjArea2p.val)[6] <- "Microbiology Lab"
pw.wiladjArea2p.val %<>% rownames_to_column("Area Type 1") %>% pivot_longer(-c("Area Type 1"), names_to = "Area Type 2", values_to = "Shannon p_adjusted")
pw.wiladjArea2p.valPlotData <- pw.wiladjArea2p.val
pw.wiladjArea2p.valPlotData$`Area Type 1` <- factor(pw.wiladjArea2p.valPlotData$`Area Type 1`, levels = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"))
pw.wiladjArea2p.valPlotData$`Area Type 2` <- factor(pw.wiladjArea2p.valPlotData$`Area Type 2`, levels = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"))
pw.wiladjArea2p.valPlotData$`Adjusted p-value` <- ifelse(pw.wiladjArea2p.valPlotData$`Shannon p_adjusted` < 0.05, "p < 0.05", ifelse(pw.wiladjArea2p.valPlotData$`Shannon p_adjusted` < 0.1, "0.05 < p < 0.1","p > 0.1"))
pw.wiladjArea2p.val <- pw.wiladjArea2p.val[is.na(pw.wiladjArea2p.val$`Shannon p_adjusted`) == FALSE, ]

pw.wiladjArea2p.valPlot <- ggplot(pw.wiladjArea2p.valPlotData,aes(x = `Area Type 1`, y = `Area Type 2`, fill = `Adjusted p-value`)) + 
  scale_fill_manual(values = cols, na.value = "#FFFFFF") + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank()
        )
print(pw.wiladjArea2p.valPlot)

pw.wiladjArea2p.val <- unite(pw.wiladjArea2p.val,c("Area Type 1", "Area Type 2"), col = "Pairing", sep = " vs ", remove = FALSE)

pw.wiladjArea3p.val <- as.data.frame(pw.wiladjArea3$p.value)
pw.wiladjArea3p.val <- rbind(rep(NA,5),pw.wiladjArea3p.val)
pw.wiladjArea3p.val <- cbind(pw.wiladjArea3p.val, rep(NA,6))
rownames(pw.wiladjArea3p.val)[1] <- "IPAC Offices"
colnames(pw.wiladjArea3p.val)[6] <- "Microbiology Lab"
pw.wiladjArea3p.val %<>% rownames_to_column("Area Type 1") %>% pivot_longer(-c("Area Type 1"), names_to = "Area Type 2", values_to = "Pielou p_adjusted")
pw.wiladjArea3p.valPlotData <- pw.wiladjArea3p.val
pw.wiladjArea3p.valPlotData$`Area Type 1` <- factor(pw.wiladjArea3p.valPlotData$`Area Type 1`, levels = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"))
pw.wiladjArea3p.valPlotData$`Area Type 2` <- factor(pw.wiladjArea3p.valPlotData$`Area Type 2`, levels = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"))
pw.wiladjArea3p.valPlotData$`Adjusted p-value` <- ifelse(pw.wiladjArea3p.valPlotData$`Pielou p_adjusted` < 0.05, "p < 0.05", ifelse(pw.wiladjArea3p.valPlotData$`Pielou p_adjusted` < 0.1, "0.05 < p < 0.1","p > 0.1"))
pw.wiladjArea3p.val <- pw.wiladjArea3p.val[is.na(pw.wiladjArea3p.val$`Pielou p_adjusted`) == FALSE, ]

pw.wiladjArea3p.valPlot <- ggplot(pw.wiladjArea3p.valPlotData,aes(x = `Area Type 1`, y = `Area Type 2`, fill = `Adjusted p-value`)) + 
  scale_fill_manual(values = cols, na.value = "#FFFFFF") + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.line = element_blank()
        )
print(pw.wiladjArea3p.valPlot)

pw.wiladjArea3p.val <- unite(pw.wiladjArea3p.val,c("Area Type 1", "Area Type 2"), col = "Pairing", sep = " vs ", remove = FALSE)

pw.wilArea1p.vals <- merge(pw.wilArea1p.val[,c("Pairing","Observed p")],pw.wiladjArea1p.val[,c("Pairing","Observed p_adjusted")], by = "Pairing")
pw.wilArea2p.vals <- merge(pw.wilArea2p.val[,c("Pairing","Shannon p")],pw.wiladjArea2p.val[,c("Pairing","Shannon p_adjusted")], by = "Pairing")
pw.wilArea3p.vals <- merge(pw.wilArea3p.val[,c("Pairing","Pielou p")],pw.wiladjArea3p.val[,c("Pairing","Pielou p_adjusted")], by = "Pairing")

pw.wilAreap.vals <- merge(pw.wilArea1p.vals,pw.wilArea2p.vals, by = "Pairing")
pw.wilAreap.vals <- merge(pw.wilAreap.vals,pw.wilArea3p.vals, by = "Pairing")


#Kruskal Wallis adjusted
p_adjusted1 <- p.adjust(p1, method = "BH")
p_adjusted2 <- p.adjust(p2, method = "BH")
p_adjusted3 <- p.adjust(p3, method = "BH")

kw.p <- tibble(Grouping = c("Surface Type I", "Area Type"), kwdf, chi1, p1, p_adjusted1, chi2, p2, p_adjusted2, chi3, p3, p_adjusted3)

#Time vs Richness
print(cor.test(alphaDiversityAfterOTU$Time.Post.Opening, alphaDiversityAfterOTU$Observed, method = "spearman"))

areas <- c("IPAC Offices", "Pediatric Surgery Ward", "PICU", "Pathology Lab", "Microbiology Lab")

timeCor <- list(Area.Type = c(), rho = c(), upper = c(), lower = c(), p = c())
for (i in seq_along(areas)) {
  data_area <- subset(alphaDiversityAfterOTU, Area.Type == areas[i])
  temp <- cor.test(data_area[,"Time.Post.Opening"], data_area[,"Observed"], method = "pearson")
  timeCor$Area.Type[i] <- areas[i]
  timeCor$rho[i] <- temp$estimate
  timeCor$lower[i] <- sprintf("%.4f",round(temp$conf.int[1],4))
  timeCor$upper[i] <- sprintf("%.4f",round(temp$conf.int[2],4))
  timeCor$p[i] <- temp$p.value
}
timeCor <- as.data.frame(timeCor)
timeCor <- unite(timeCor,c("lower", "upper"), col = "95% CI", sep = ", ")
timeCor$`95% CI` <- paste("[", timeCor$`95% CI`, "]", sep = "")

