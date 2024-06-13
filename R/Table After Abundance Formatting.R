tableAbundPhy_a <- qatar_physeq_after_df_PhylumRelAbund[1:27,]
tableAbundPhy_b <- qatar_physeq_after_df_ClassRelAbund[qatar_physeq_after_df_ClassRelAbund$Class %in% ProteoClass,]
colnames(tableAbundPhy_b) = colnames(tableAbundPhy_a)
tableAbundPhy <- add_row(tableAbundPhy_a, tableAbundPhy_b, .after = 2)

tableAbundFam <- qatar_physeq_after_df_FamilyRelAbund[1:30,]
tableAbundGen <- qatar_physeq_after_df_GenusRelAbund[1:30,]

colnames(tableAbundGen) = colnames(tableAbundFam) = colnames(tableAbundPhy)

tableAbund <- rbind(tableAbundPhy, tableAbundFam, tableAbundGen)
tableAbund <- tableAbund %>%
  gt(rowname_col = "Phylum") %>%
  fmt_scientific(columns = `Relative Abundance`, rows = `Relative Abundance` < 0.1) %>%
  fmt_number(columns = `Relative Abundance`, rows = `Relative Abundance` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Standard Deviation`, rows = `Standard Deviation` < 0.1) %>%
  fmt_number(columns = `Standard Deviation`, rows = `Standard Deviation` > 0.1, n_sigfig = 3) %>%
  tab_header(title = md("**Table S5.** Abundant taxa in Sidra hospital door handle, keyboard, and office electronic surfaces taken after opening (sample size = 120)."),
             subtitle = NA) %>%
  cols_label(`Relative Abundance` = "Relative Abundance (%)", `Standard Deviation` = "Standard Deviation (%)") %>%
  tab_stubhead(label = md("**Taxa**")) %>%
  tab_row_group(label = "Phylum (All 27)", rows = seq(1,29)) %>%
  tab_row_group(label = "Family (Top 30)", rows = seq(30,59)) %>%
  tab_row_group(label = "Genus (Top 30)", rows = seq(60, 89)) %>%
  row_group_order(groups = c("Phylum (All 27)", "Family (Top 30)", "Genus (Top 30)")) %>%
  tab_style(style = cell_text(font = "Times New Roman", align = "center", size = 12), locations = list(cells_body(), cells_column_labels(), cells_title(), cells_row_groups(), cells_stub(), cells_stubhead())) %>%
  tab_style(style = cell_text(align = "left"), locations =  list(cells_title(), cells_stub(), cells_stubhead())) %>%
  tab_style(style = cell_text(weight = "bold"), locations =  cells_column_labels()) %>%
  tab_style(style = cell_text(align = "left", weight = "bold", style = "italic"), locations =  cells_row_groups()) %>%
  tab_style(style = cell_text(indent = 20, style = "italic"), locations = cells_stub(rows = 3:4)) %>%
  tab_style(style = cell_text(style = "italic"), locations = cells_body(rows = 3:4)) %>%
  tab_options(column_labels.border.bottom.color = "black", table.border.top.color = "transparent", table.border.bottom.width = 3, table.border.bottom.color = "black", table_body.hlines.color = "transparent", stub.border.color = "black", heading.border.bottom.width  = 3, heading.border.bottom.color = "black", row_group.border.top.color = "black", row_group.border.bottom.color = "black")
