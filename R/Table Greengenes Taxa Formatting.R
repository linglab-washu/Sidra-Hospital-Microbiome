tableLassoTaxagg <- lasso_combined_taxa[, c("ASV_name","Family.x","Family.y","Genus.x","Genus.y", "Species.x","Species.y")]

tableLassoAbundTaxagg <- lasso_abund_combined_taxa[, c("ASV_name","Family.x","Family.y","Genus.x","Genus.y", "Species.x","Species.y")]

tableCoreTaxagg <- core_combined_taxa[, c("ASV_name","Family.x","Family.y","Genus.x","Genus.y", "Species.x","Species.y")]

tableTaxagg <- rbind(tableLassoTaxagg,tableLassoAbundTaxagg,tableCoreTaxagg)

tableTaxagg <- tableTaxagg %>%
  gt(rowname_col = "ASV_name") %>%
  tab_header(
    title = md(
      "**Table S8.** Taxonomic Classification Comparison of Key ASVs."
    ),
    subtitle = NA
  ) %>%
  tab_stubhead(label = md("**ASV**")) %>%
  cols_label(
    ASV_name = "ASV",
    Family.x = "SILVA",
    Family.y = "Greengenes2",
    Genus.x = "SILVA",
    Genus.y = "Greengenes2",
    Species.x = "SILVA",
    Species.y = "Greengenes2"
  ) %>%
  sub_missing(missing_text = "-") %>%
  tab_spanner(label = md("**Family**"), columns = c(Family.x, Family.y)) %>%
  tab_spanner(label = md("**Genus**"),columns = c(Genus.x, Genus.y)) %>%
  tab_spanner(label = md("**Species**"),columns = c(Species.x, Species.y)) %>%
  tab_row_group(label = "LASSO Influential ASVs", rows = 1:nrow(lasso_combined_taxa)) %>%
  tab_row_group(label = "LASSO Abundant ASVs", rows = nrow(lasso_combined_taxa)+1:nrow(lasso_abund_combined_taxa)) %>%
  tab_row_group(label = "Core ASVs", rows = nrow(lasso_combined_taxa)+nrow(lasso_abund_combined_taxa)+1:nrow(core_combined_taxa)) %>%
  row_group_order(groups = c("LASSO Influential ASVs", "LASSO Abundant ASVs", "Core ASVs")) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", align = "center", size = 12),
    locations = list(
      cells_body(),
      cells_column_labels(),
      cells_title(),
      cells_stub(),
      cells_stubhead(),
      cells_row_groups(),
      cells_column_spanners()
    )
  ) %>%
  tab_options(
    column_labels.border.bottom.color = "black",
    table.border.top.color = "transparent",
    table.border.bottom.width = 3,
    table.border.bottom.color = "black",
    table_body.hlines.color = "transparent",
    stub.border.color = "black",
    heading.border.bottom.width = 3,
    heading.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  ) %>%
  tab_style(
    style = cell_text(align = "left"),
    locations =  list(cells_title(), cells_row_groups())
  ) %>%
  tab_style(style = cell_text(weight = "bold"), locations =  cells_column_labels()) %>%
  tab_style(style = cell_borders(sides = c("top"),  weight = px(1.5), color = "black"),
            locations = list(cells_stub(rows = c(7, 8, 9)),cells_body(rows = c(7, 8, 9)))) %>%
  tab_options(
    column_labels.border.bottom.color = "black",
    table.border.top.color = "transparent",
    table.border.bottom.width = 3,
    table.border.bottom.color = "black",
    table_body.hlines.color = "transparent",
    stub.border.color = "black",
    heading.border.bottom.width  = 3,
    heading.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  ) 
