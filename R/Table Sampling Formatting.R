tableSampA <- RoomTypeFrequencyTable[order(-RoomTypeFrequencyTable$freq),]
tableSampB <- SurfaceTypeIFrequencyTable[order(-SurfaceTypeIFrequencyTable$freq),]
tableSampC <- SurfaceTypeIIFrequencyTable[order(-SurfaceTypeIIFrequencyTable$freq),]

colnames(tableSampA) = colnames(tableSampB) = colnames(tableSampC)
tableSampA <- tableSampA[tableSampA$Surface.Type.II != "Neg Control",]
tableSampB <- tableSampB[tableSampB$Surface.Type.II != "Neg Control",]
tableSampC <- tableSampC[tableSampC$Surface.Type.II != "Neg Control",]

tableSamp <- rbind(tableSampA, tableSampB, tableSampC)

tableSamp <- tableSamp %>%
  gt() %>%
  tab_header(title = md("**Table S3.** Sampling output before rarefraction."),
             subtitle = NA) %>%
  cols_label(Surface.Type.II = "Sample Type", freq = "Sample Counts") %>%
  tab_row_group(label = "Room Type", rows = seq(1,nrow(tableSampA))) %>%
  tab_row_group(label = "Surface Category", rows = seq(nrow(tableSampA) + 1,nrow(tableSampA) + nrow(tableSampB))) %>%
  tab_row_group(label = "Surface Description", rows = seq(nrow(tableSampA) + nrow(tableSampB) + 1, nrow(tableSampA) + nrow(tableSampB) + nrow(tableSampC))) %>%
  row_group_order(groups = c("Room Type", "Surface Category", "Surface Description")) %>%
  tab_style(style = cell_text(font = "Times New Roman", align = "center", size = 12), locations = list(cells_body(), cells_column_labels(), cells_title(), cells_row_groups())) %>%
  tab_style(style = cell_text(align = "left"), locations =  cells_title()) %>%
  tab_style(style = cell_text(weight = "bold"), locations =  cells_column_labels()) %>%
  tab_style(style = cell_text(align = "left", weight = "bold", style = "italic"), locations =  cells_row_groups()) %>%
  tab_options(column_labels.border.bottom.color = "black", table.border.top.color = "transparent", table.border.bottom.width = 3, table.border.bottom.color = "black", table_body.hlines.color = "transparent", stub.border.color = "black", heading.border.bottom.width  = 3, heading.border.bottom.color = "black", row_group.border.top.color = "black", row_group.border.bottom.color = "black")
