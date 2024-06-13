#LASSO
lassoResults <- list()
for (i in seq_along(lassoAreaType)) {
  lassoResults[[i]] <- data.frame(
    Model = "LASSO",
    Response = y.AreaName[i],
    Tuning = lassoAreaType[[i]][["bestTune"]][["lambda"]],
    Training = lassoAreaType[[i]]$results$Accuracy[which(lassoAreaType[[i]]$results$lambda==lassoAreaType[[i]]$bestTune$lambda)],
    Testing = lassoAreaTypeConfusionMatrix[[i]][["overall"]][["Accuracy"]],
    Testing.MCC = lassoAreaTypeMCC[[i]]
  )
}

#SVM
svmLinearResults <- data.frame(
  Model = c("Linear SVM",rep(NA,6)),
  Response = c("Area Type",rownames(svmLinearAreaTypeConfusionMatrix[["byClass"]])),
  Tuning = svmLinearAreaType[["bestTune"]][["C"]],
  Training = c(svmLinearAreaType$results$Accuracy[which(svmLinearAreaType$results$C==svmLinearAreaType$bestTune$C)], rep(NA,6)),
  Testing = c(svmLinearAreaTypeConfusionMatrix[["overall"]][["Accuracy"]],as.numeric(svmLinearAreaTypeConfusionMatrix[["byClass"]][,"Balanced Accuracy"])),
  Testing.MCC = c(svmLinearAreaTypeMCC, unlist(svmLinearAreaTypeMCC.Area))
)

svmRadialResults <- data.frame(
  Model = c("Radial SVM",rep(NA,6)),
  Response = c("Area Type",rownames(svmRadialAreaTypeConfusionMatrix[["byClass"]])),
  Tuning = svmRadialAreaType[["bestTune"]][["C"]],
  Training = c(svmRadialAreaType$results$Accuracy[which(svmRadialAreaType$results$C==svmRadialAreaType$bestTune$C)],rep(NA,6)),
  Testing = c(svmRadialAreaTypeConfusionMatrix[["overall"]][["Accuracy"]],as.numeric(svmRadialAreaTypeConfusionMatrix[["byClass"]][,"Balanced Accuracy"])),
  Testing.MCC = c(svmRadialAreaTypeMCC, unlist(svmRadialAreaTypeMCC.Area))
)

#Random Forest
rfResults <- data.frame(
  Model = c("Random Forest",rep(NA,6)),
  Response = c("Area Type",rownames(rfAreaTypeConfusionMatrix[["byClass"]])),
  Tuning = rfAreaType[["bestTune"]][["mtry"]],
  Training = c(rfAreaType$results$Accuracy[which(rfAreaType$results$mtry==rfAreaType$bestTune$mtry)], rep(NA,6)),
  Testing = c(rfAreaTypeConfusionMatrix[["overall"]][["Accuracy"]],as.numeric(rfAreaTypeConfusionMatrix[["byClass"]][,"Balanced Accuracy"])),
  Testing.MCC = c(rfAreaTypeMCC, unlist(rfAreaTypeMCC.Area))
)

#All
mlResults <- do.call(rbind.data.frame, lassoResults)
mlResults <- rbind.data.frame(mlResults, svmLinearResults, svmRadialResults, rfResults)


#LASSO features
qatar_physeq_after_merged_AreaType <- merge_samples(qatar_physeq_after, "Area.Type")
sample_data(qatar_physeq_after_merged_AreaType)[,"Area.Type"] <- levels(sample_data(qatar_physeq_after)[["Area.Type"]])
qatar_physeq_after_merged_AreaType <- transform_sample_counts(qatar_physeq_after_merged_AreaType,function(x) 100*x/sum(x))


lassoAreaTaxa <- list()
lassoAreaPlots <- list()

for (i in seq_along(lassoAreaType)) {
  coef_i <- coef(lassoAreaType[[i]]$finalModel, lassoAreaType[[i]]$bestTune$lambda)
  lassoAreaTaxa[[i]] <- data.frame(OTU = coef_i@Dimnames[[1]][coef_i@i + 1])
  lassoAreaTaxa[[i]]$ASV_name <- OTU_taxonomy[match(lassoAreaTaxa[[i]]$OTU,row.names(OTU_taxonomy)),"ASV_name"]
  lassoAreaTaxa[[i]]$ASV_name[lassoAreaTaxa[[i]]$OTU == "(Intercept)"] <- "(Intercept)"
  lassoAreaTaxa[[i]]$Value <- coef_i@x
  lassoAreaTaxa[[i]]$Area.Type <- ifelse(lassoAreaTaxa[[i]]$Value > 0, y.AreaName[[i]], "Other")
  lassoAreaTaxa[[i]]$Model <- y.AreaName[i]
  lassoAreaTaxa[[i]] <- lassoAreaTaxa[[i]][order(-lassoAreaTaxa[[i]]$Value),]

  if (length(lassoAreaTaxa[[i]]$OTU) != 1) {
    qatar_physeq_after_merged_AreaType_i <-  prune_taxa(lassoAreaTaxa[[i]]$OTU, qatar_physeq_after_merged_AreaType)
    rankLabel <- paste("Informative ASVs\n in the", y.AreaName[i])
    colnames(tax_table(qatar_physeq_after_merged_AreaType_i))[8] <- rankLabel
    lassoAreaPlots[[i]] <- plot_heatmap(qatar_physeq_after_merged_AreaType_i, 
                                        method = NULL, 
                                        sample.order = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab"), 
                                        taxa.order = rev(lassoAreaTaxa[[i]]$OTU),
                                        taxa.label = rankLabel
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
  }
}

for (i in 1:5) {
  print(lassoAreaPlots[[i]])
}

lassoAreaTaxaAll <- bind_rows(lassoAreaTaxa)
lassoAreaTaxaAllDuplicates <- lassoAreaTaxaAll[duplicated(lassoAreaTaxaAll$OTU),]
lassoAreaTaxaAllInformative <- lassoAreaTaxaAll[lassoAreaTaxaAll$Value > 1,]

#Linear SVM Features
svmLinearAreaTaxa <- varImp(svmLinearAreaType, scale = TRUE)

svmLinearAreaTaxaAll <- list()
for (i in 1:5) {
  svmLinearAreaTaxa$importance[i] <- svmLinearAreaTaxa$importance[i][order(svmLinearAreaTaxa$importance[[i]], decreasing = TRUE),]
  svmLinearAreaTaxaAll[[i]] <- data.frame(
    OTU = rownames(svmLinearAreaTaxa$importance[i])[1:(nrow(lassoAreaTaxa[[i]]) - 1)],
    Value = svmLinearAreaTaxa$importance[i][1:(nrow(lassoAreaTaxa[[i]]) - 1),], 
    Area.Type = names(svmLinearAreaTaxa$importance)[i]
    )
  svmLinearAreaTaxaAll[[i]]$ASV_name <- OTU_taxonomy[match(svmLinearAreaTaxaAll[[i]]$OTU,row.names(OTU_taxonomy)),"ASV_name"]
  
}

svmLinearAreaTaxaAll <- bind_rows(svmLinearAreaTaxaAll)

#Radial SVM Features
svmRadialAreaTaxa <- varImp(svmRadialAreaType, scale = TRUE)

svmRadialAreaTaxaAll <- list()
for (i in 1:5) {
  svmRadialAreaTaxa$importance[i] <- svmRadialAreaTaxa$importance[i][order(svmRadialAreaTaxa$importance[[i]], decreasing = TRUE),]
  svmRadialAreaTaxaAll[[i]] <- data.frame(
    OTU = rownames(svmRadialAreaTaxa$importance[i])[1:(nrow(lassoAreaTaxa[[i]]) - 1)],
    Value = svmRadialAreaTaxa$importance[i][1:(nrow(lassoAreaTaxa[[i]]) - 1),], 
    Area.Type = names(svmRadialAreaTaxa$importance)[i]
  )
  svmRadialAreaTaxaAll[[i]]$ASV_name <- OTU_taxonomy[match(svmRadialAreaTaxaAll[[i]]$OTU,row.names(OTU_taxonomy)),"ASV_name"]
  
}

svmRadialAreaTaxaAll <- bind_rows(svmRadialAreaTaxaAll)

#Random Forest Features
rfAreaTaxa <- varImp(rfAreaType, scale = TRUE)

rfAreaTaxa$importance[1] <- rfAreaTaxa$importance[1][order(rfAreaTaxa$importance[[1]], decreasing = TRUE),]
rfAreaTaxaAll <- data.frame(
  OTU = gsub("`", '', rownames(rfAreaTaxa$importance[1])[1:158]),
  Value = rfAreaTaxa$importance[1][1:158,], 
  Area.Type = names(rfAreaTaxa$importance[1])[1]
)
rfAreaTaxaAll$ASV_name <- OTU_taxonomy[match(rfAreaTaxaAll$OTU,row.names(OTU_taxonomy)),"ASV_name"]

rfAreaTaxaAll <- bind_rows(rfAreaTaxaAll)