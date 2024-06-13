tableQualF <- dadaPipelineF %>% 
  group_by(Step) %>%
  summarise(Average = mean(Score, na.rm = T), 
            SD = sd(Score, na.rm = T)) 
tableQualR <- dadaPipelineR %>% 
  group_by(Step) %>%
  summarise(Average = mean(Score, na.rm = T), 
            SD = sd(Score, na.rm = T))

tableQual <- rbind(tableQualF, tableQualR)

tableQual <- tableQual %>%
  gt(rowname_col = "Step") %>%
  fmt_number(columns = Average, n_sigfig = 3) %>%
  fmt_number(columns = SD, n_sigfig = 3) %>%
  tab_header(
    title = md(
      "**Table S6.** Read Quality Through the Dada2 Pipeline."
    ),
    subtitle = NA
  ) %>%
  tab_stubhead(label = md("**Pipeline Step**")) %>%
  cols_label(
    Step = "Pipeline Step",
  ) %>%
  tab_spanner(label = md("**Quality Score**"), columns = c(Average, SD)) %>%
  tab_row_group(label = "Forward Reads", rows = 1:3) %>%
  tab_row_group(label = "Reverse Reads", rows = 4:6) %>%
  row_group_order(groups = c("Forward Reads","Reverse Reads")) %>%
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