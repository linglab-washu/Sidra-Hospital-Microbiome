#Format Table 3 
tableML <- mlResults %>%
  dplyr::select(-"Tuning") %>%
  add_row(Model = "Performance By Class",.before = 8) %>%
  add_row(Model = "Performance By Class",.before = 16) %>%
  add_row(Model = "Performance By Class",.before = 24)

tableML[c(9:14,17:22,25:30),1] <- NA

tableML$Response <- gsub(".", " ", tableML$Response, fixed = TRUE)

tableML <- tableML %>%
  gt(rowname_col = "Model") %>%
  fmt_percent(columns = Training, decimals = 1) %>%
  fmt_percent(columns = Testing, decimals = 1) %>%
  fmt_number(
    columns = Testing.MCC,
    rows = Testing.MCC != 1 & Testing.MCC != 0,
    n_sigfig = 3
  ) %>%
  fmt_number(
    columns = Testing.MCC,
    rows = Testing.MCC == 1 & Testing.MCC == 0,
    n_sigfig = 1
  ) %>%
  sub_missing(missing_text = "-") %>%
  sub_missing(row = c(8, 16, 24), missing_text = "") %>%
  sub_missing(columns = 1, missing_text = "") %>%
  tab_header(
    title = md(
      "**Table 3.** Machine learning models perform differently predicting the area type among the post-opening door handle, keyboard, and office electronic samples (n = 120)."
    ),
    subtitle = NA
  ) %>%
  tab_stubhead(label = md("**Machine Learning Model**")) %>%
  cols_label(
    Model = "Machine Learning Model",
    Response = "Response Variable",
    Training = "Training Accuracy",
    Testing = "Testing Accuracy",
    Testing.MCC = "Testing MCC"
  ) %>%
  tab_row_group(label = "A) Binary Models",
                rows = 1:6) %>%
  tab_row_group(label = "B) Multiclass Models",
                rows = 7:30) %>%
  row_group_order(groups = c("A) Binary Models", "B) Multiclass Models")) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", align = "center", size = 12),
    locations = list(
      cells_body(),
      cells_column_labels(),
      cells_title(),
      cells_stub(),
      cells_stubhead(),
      cells_footnotes(),
      cells_row_groups()
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
    locations =  list(cells_title(), cells_footnotes(), cells_stub(), cells_row_groups())
  ) %>%
  tab_style(style = cell_text(weight = "bold"), locations = list(cells_column_labels(), cells_row_groups())) %>%
  tab_style(style = cell_text(style = "italic"), locations =  cells_stub(rows = Model == "Performance By Class")) %>%
  tab_style(style = cell_borders(sides = c("top"),  weight = px(1.5), color = "black"),
            locations = list(cells_stub(rows = c(7, 15, 23)),cells_body(rows = c(7, 15, 23)))) %>%
  tab_style(style = cell_borders(sides = c("top"),  weight = px(1)),
            locations = list(cells_stub(rows = c(8, 16, 24)), cells_body(rows = c(8, 9, 16, 17, 24, 25))))%>%
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
  ) %>%
  tab_footnote(footnote = "Tuning was performed with 10-fold CV repeated 10 times maximizing the accuracy.",
               locations = cells_title(groups = "title"))


#Hyperparameter Supplementary
tableMLSupp <- mlResults[!is.na(mlResults$Model),] %>%
  dplyr::select(c("Model", "Response", "Tuning"))

tableMLSupp$Response <- gsub(".", " ", tableMLSupp$Response, fixed = TRUE)

tableMLSupp <- tableMLSupp %>%
  gt(rowname_col = "Model") %>%
  fmt_scientific(columns = Tuning, rows = abs(Tuning) < 0.1) %>%
  fmt_number(columns = Tuning, rows = abs(Tuning) > 0.1, n_sigfig = 3) %>%
  tab_header(
    title = md(
      "**Table S8.** Hyperparameter tuned values for each machine learning model."
    ),
    subtitle = NA
  ) %>%
  tab_stubhead(label = md("**Machine Learning Model**")) %>%
  cols_label(
    Model = "Machine Learning Model",
    Tuning = "Tuned Hyperparameter"
  ) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", align = "center", size = 12),
    locations = list(
      cells_body(),
      cells_column_labels(),
      cells_title(),
      cells_stub(),
      cells_stubhead(),
      cells_footnotes()
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
    locations =  list(cells_title(), cells_footnotes())
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
  ) %>%
  tab_footnote(footnote = "Tuning was performed with 10-fold CV repeated 10 times maximizing the accuracy.",
               locations = cells_title(groups = "title"))


#LASSO Features Supplemental
tableLasso <- lassoAreaTaxaAll[,c("ASV_name", "Value")]
rownames(tableLasso) <- NULL
tableLasso <- tableLasso %>%
  gt() %>%
  fmt_scientific(columns = Value, rows = abs(Value) < 0.1) %>%
  fmt_number(columns = Value, rows = abs(Value) > 0.1, n_sigfig = 3) %>%
  tab_header(title = "**Table S9.** The nonzero coefficients ASVs for each LASSO model and corresponding coefficients.", subtitle = NA) %>%
  cols_label(ASV_name = "ASV", Value = "Coefficient") %>%
  tab_row_group(label = "IPAC Offices", rows = seq(1,nrow(lassoAreaTaxa[[1]]))) %>%
  tab_row_group(label = "Pediatric Surgery Ward", rows = seq(nrow(lassoAreaTaxa[[1]]) + 1,nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]))) %>%
  tab_row_group(label = "NICU", rows = seq(nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]) + 1, nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]) + nrow(lassoAreaTaxa[[3]]))) %>%
  tab_row_group(label = "PICU", rows = seq(nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]) + nrow(lassoAreaTaxa[[3]]) + 1, nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]) + nrow(lassoAreaTaxa[[3]]) + nrow(lassoAreaTaxa[[4]]))) %>%
  tab_row_group(label = "Pathology Lab", rows = seq(nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]) + nrow(lassoAreaTaxa[[3]]) + nrow(lassoAreaTaxa[[4]]) + 1, nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]) + nrow(lassoAreaTaxa[[3]]) + nrow(lassoAreaTaxa[[4]]) + nrow(lassoAreaTaxa[[5]]))) %>%
  tab_row_group(label = "Microbiology Lab", rows = seq(nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]) + nrow(lassoAreaTaxa[[3]]) + nrow(lassoAreaTaxa[[4]]) + nrow(lassoAreaTaxa[[5]]) + 1, nrow(lassoAreaTaxa[[1]]) + nrow(lassoAreaTaxa[[2]]) + nrow(lassoAreaTaxa[[3]]) + nrow(lassoAreaTaxa[[4]]) + nrow(lassoAreaTaxa[[5]]) + nrow(lassoAreaTaxa[[6]]))) %>%
  row_group_order(groups = c("IPAC Offices", "Pediatric Surgery Ward", "NICU", "PICU", "Pathology Lab", "Microbiology Lab")) %>%
  tab_style(style = cell_text(font = "Times New Roman", align = "center", size = 12), locations = list(cells_body(), cells_column_labels(), cells_title(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(align = "left"), locations =  list(cells_title(), cells_footnotes())) %>%
  tab_style(style = cell_text(weight = "bold"), locations =  cells_column_labels()) %>%
  tab_style(style = cell_text(align = "left", weight = "bold", style = "italic"), locations =  cells_row_groups()) %>%
  tab_style(style = cell_text(style = "italic"), locations =  cells_body(rows = ASV_name == "(Intercept)")) %>%
  tab_options(column_labels.border.bottom.color = "black", table.border.top.color = "transparent", table.border.bottom.width = 3, table.border.bottom.color = "black", table_body.hlines.color = "transparent", stub.border.color = "black", heading.border.bottom.width  = 3, heading.border.bottom.color = "black", row_group.border.top.color = "black", row_group.border.bottom.color = "black", table_body.border.bottom.color = "black") %>%
  tab_footnote(footnote = "Positive values indicate the area type of interest, and negative values indicate other area types.", locations = cells_title())

