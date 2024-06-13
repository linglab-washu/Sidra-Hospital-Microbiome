dadaPipelineF <- list()
dadaPipelineF[[1]] <- as.data.frame(qaDataAvg[[1]]) %>%
  add_column(Step = "Unfiltered", .before = "Cycle")
dadaPipelineF[[2]] <- as.data.frame(qaDataAvg[[3]]) %>%
  bind_rows(set_names(data.frame(matrix(NA, nrow = 16, ncol = 5)), colnames(.))) %>%
  add_column(Step = "Filtered", .before = "Cycle")
dadaPipelineF[[3]] <- as.data.frame(qaDataAvg[[5]]) %>%
  bind_rows(set_names(data.frame(matrix(NA, nrow = 16, ncol = 5)), colnames(.))) %>%
  add_column(Step = "Denoised", .before = "Cycle")

dadaPipelineR <- list()
dadaPipelineR[[1]] <- as.data.frame(qaDataAvg[[2]]) %>%
  add_column(Step = "Unfiltered", .before = "Cycle")
dadaPipelineR[[2]] <- as.data.frame(qaDataAvg[[4]]) %>%
  mutate(Cycle = Cycle + 15) %>%
  bind_rows(set_names(data.frame(matrix(NA, nrow = 15, ncol = 5)), colnames(.)), .) %>%
  bind_rows(set_names(data.frame(matrix(NA, nrow = 51, ncol = 5)), colnames(.))) %>%
  add_column(Step = "Filtered", .before = "Cycle")
dadaPipelineR[[3]] <- as.data.frame(qaDataAvg[[6]]) %>%
  mutate(Cycle = Cycle + 15) %>%
  bind_rows(set_names(data.frame(matrix(NA, nrow = 15, ncol = 5)), colnames(.)), .) %>%
  bind_rows(set_names(data.frame(matrix(NA, nrow = 51, ncol = 5)), colnames(.))) %>%
  add_column(Step = "Denoised", .before = "Cycle")

dadaPipelineF <- do.call("rbind", dadaPipelineF)
dadaPipelineR <- do.call("rbind", dadaPipelineR)

dadaPipelineF$Step <- factor(dadaPipelineF$Step, levels = c("Unfiltered", "Filtered", "Denoised"))
dadaPipelineR$Step <- factor(dadaPipelineR$Step, levels = c("Unfiltered", "Filtered", "Denoised"))

readQualPlotFOverlay <- ggplot(dadaPipelineF, aes(Cycle, Score, ymax = Q75 , ymin = Q25, group = Step)) + 
  geom_line(aes(color = Step)) + 
  scale_color_manual(values=c("red", "orange", "green")) 

readQualPlotFFacet <- ggplot(dadaPipelineF, aes(Cycle, Score, ymax = Q75 , ymin = Q25, group = Step)) + 
  geom_line(aes(color = Step)) + 
  geom_ribbon(alpha=0.3, fill = "gray") +
  facet_grid(Step~.) +
  scale_color_manual(values=c("red", "orange", "green")) 

readQualPlotROverlay <- ggplot(dadaPipelineR, aes(Cycle, Score, ymax = Q75 , ymin = Q25, group = Step)) + 
  geom_line(aes(color = Step)) + 
  scale_color_manual(values=c("red", "orange", "green"))

readQualPlotRFacet <- ggplot(dadaPipelineR, aes(Cycle, Score, ymax = Q75 , ymin = Q25, group = Step)) + 
  geom_line(aes(color = Step)) + 
  geom_ribbon(alpha=0.3, fill = "gray") +
  facet_grid(Step~.) +
  scale_color_manual(values=c("red", "orange", "green"))

readQualPlotGrid <- ggarrange(readQualPlotFOverlay + ggtitle("Forward Reads"), readQualPlotFFacet, readQualPlotROverlay + ggtitle("Reverse Reads"), readQualPlotRFacet,
                                       labels = c("(A)", "","(B)", ""),
                                       font.label = list(face = "plain"),
                                       common.legend = TRUE,
                                       legend = "right",
                                       ncol = 2, nrow = 2,
                                       hjust = 0.05,
                                       vjust = 1
)
print(readQualPlotGrid)


dadaPipelineF %>% 
  group_by(Step) %>%
  summarise(Average = mean(Score, na.rm = T), 
            SD = sd(Score, na.rm = T))
dadaPipelineR %>% 
  group_by(Step) %>%
  summarise(Average = mean(Score, na.rm = T), 
            SD = sd(Score, na.rm = T))