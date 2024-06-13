tableAlphaBeforeAfter <- wc.p[,c(1,3,5,7)] %>%
  add_column(obs_p_adjusted = NA,.after = "Observed") %>%
  add_column(shan_p_adjusted = NA,.after = "Shannon") %>%
  add_column(pie_p_adjusted = NA,.after = "Pielou")
tableAlphaBeforeAfter[,1] <- c("Before opening (n = 14) vs. After opening (n = 14)")

tableAlphaAfter <- kw.p[,c(1,4,5,7,8,10,11)]
tableAlphaAfter[,1] <- c("Surface Category (n = 3)", "Area Type (n = 6)")

tableAlphaSupp <- pw.wilAreap.vals
tableAlphaSupp <- tableAlphaSupp %>%
  add_row(Pairing = "Area Type",.before = 1)


colnames(tableAlphaBeforeAfter) = colnames(tableAlphaAfter) = colnames(tableAlphaSupp)

tableAlpha <- rbind(tableAlphaBeforeAfter, tableAlphaAfter)

tableAlpha <- tableAlpha %>%
  gt(rowname_col = "Pairing") %>%
  fmt_scientific(columns = `Observed p`, rows = `Observed p` < 0.1) %>%
  fmt_number(columns = `Observed p`, rows = `Observed p` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Observed p_adjusted`, rows = `Observed p_adjusted` < 0.1) %>%
  fmt_number(columns = `Observed p_adjusted`, rows = `Observed p_adjusted` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Shannon p`, rows = `Shannon p` < 0.1) %>%
  fmt_number(columns = `Shannon p`, rows = `Shannon p` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Shannon p_adjusted`, rows = `Shannon p_adjusted` < 0.1) %>%
  fmt_number(columns = `Shannon p_adjusted`, rows = `Shannon p_adjusted` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Pielou p`, rows = `Pielou p` < 0.1) %>%
  fmt_number(columns = `Pielou p`, rows = `Pielou p` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Pielou p_adjusted`, rows = `Pielou p_adjusted` < 0.1) %>%
  fmt_number(columns = `Pielou p_adjusted`, rows = `Pielou p_adjusted` > 0.1, n_sigfig = 3) %>%
  sub_missing(missing_text = "-") %>%
  tab_header(title = md("**Table 1.** Comparisons of means in alpha diversity metrics."),
             subtitle = NA) %>%
  tab_spanner(label = md("**Observed Richness**"), columns = c(`Observed p`, `Observed p_adjusted`)) %>%
  tab_spanner(label = md("**Shannon Index**"),columns = c(`Shannon p`, `Shannon p_adjusted`)) %>%
  tab_spanner(label = md("**Pielou's Eveness**"),columns = c(`Pielou p`, `Pielou p_adjusted`)) %>%
  tab_stubhead(label = md("**Grouping**")) %>%
  cols_label(`Observed p` = "p-value", `Observed p_adjusted` = "p-adjusted", `Shannon p` = "p-value", `Shannon p_adjusted` = "p-adjusted", `Pielou p` = "p-value", `Pielou p_adjusted` = "p-adjusted") %>%
  tab_row_group(label = "A) Wilcoxon rank sum tests on means of alpha diversity metrics in NICU patient rooms before and after opening.", rows = 1) %>%
  tab_row_group(label = "B) Kruskal-Wallis tests on means of alpha diversity metrics in different surface categories and area types in all door handle, keyboard, and office electronic samples taken after the hospital opened for inpatient care (n = 120).", rows = 2:3) %>%
  row_group_order(groups = c("A) Wilcoxon rank sum tests on means of alpha diversity metrics in NICU patient rooms before and after opening.", "B) Kruskal-Wallis tests on means of alpha diversity metrics in different surface categories and area types in all door handle, keyboard, and office electronic samples taken after the hospital opened for inpatient care (n = 120).")) %>%
  tab_style(style = cell_text(font = "Times New Roman", align = "center", size = 12), locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title(), cells_stubhead(), cells_stub(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(align = "left"), locations =  list(cells_title(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(style = "italic", weight = "bold"), locations =  cells_stub(rows = is.na(`Observed p`))) %>%
  tab_style(style = cell_borders(sides = c("top","bottom")), locations =  list(cells_body(rows = is.na(`Observed p`)), cells_stub(rows = is.na(`Observed p`)))) %>%
  tab_options(column_labels.border.bottom.color = "black", table.border.top.color = "transparent", table.border.bottom.width = 3, table.border.bottom.color = "black", table_body.hlines.color = "transparent", stub.border.color = "black", heading.border.bottom.width = 3, heading.border.bottom.color = "black", row_group.border.top.color = "black", row_group.border.bottom.color = "black", table_body.border.bottom.color = "black") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Observed p`,rows = `Observed p` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Observed p_adjusted`,rows = `Observed p_adjusted` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Shannon p`,rows = `Shannon p` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Shannon p_adjusted`,rows = `Shannon p_adjusted` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Pielou p`,rows = `Pielou p` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Pielou p_adjusted`,rows = `Pielou p_adjusted` <= 0.05)) %>%
  tab_footnote(footnote = "Bonferroni correction was used to adjust p-values, and significant p-values are bolded (α = 0.05).", locations = cells_row_groups(groups = 2))

#Pairing Supplementary

tableAlphaSupp <- tableAlphaSupp %>%
  gt(rowname_col = "Pairing") %>%
  fmt_scientific(columns = `Observed p`, rows = `Observed p` < 0.1) %>%
  fmt_number(columns = `Observed p`, rows = `Observed p` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Observed p_adjusted`, rows = `Observed p_adjusted` < 0.1) %>%
  fmt_number(columns = `Observed p_adjusted`, rows = `Observed p_adjusted` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Shannon p`, rows = `Shannon p` < 0.1) %>%
  fmt_number(columns = `Shannon p`, rows = `Shannon p` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Shannon p_adjusted`, rows = `Shannon p_adjusted` < 0.1) %>%
  fmt_number(columns = `Shannon p_adjusted`, rows = `Shannon p_adjusted` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Pielou p`, rows = `Pielou p` < 0.1) %>%
  fmt_number(columns = `Pielou p`, rows = `Pielou p` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Pielou p_adjusted`, rows = `Pielou p_adjusted` < 0.1) %>%
  fmt_number(columns = `Pielou p_adjusted`, rows = `Pielou p_adjusted` > 0.1, n_sigfig = 3) %>%
  sub_missing(row = 1, missing_text = "") %>%
  tab_header(title = md("**Table S6.** Pairwise Mann-Whitney U tests on means of alpha diversity metrics on the various area types in all door handle, keyboard, and office electronic samples taken after the hospital opened for inpatient care (n = 120)."),
             subtitle = NA) %>%
  tab_spanner(label = md("**Observed Richness**"), columns = c(`Observed p`, `Observed p_adjusted`)) %>%
  tab_spanner(label = md("**Shannon Index**"),columns = c(`Shannon p`, `Shannon p_adjusted`)) %>%
  tab_spanner(label = md("**Pielou's Eveness**"),columns = c(`Pielou p`, `Pielou p_adjusted`)) %>%
  tab_stubhead(label = md("**Pairing**")) %>%
  cols_label(`Observed p` = "p-value", `Observed p_adjusted` = "p-adjusted", `Shannon p` = "p-value", `Shannon p_adjusted` = "p-adjusted", `Pielou p` = "p-value", `Pielou p_adjusted` = "p-adjusted") %>%
  tab_style(style = cell_text(font = "Times New Roman", align = "center", size = 12), locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title(), cells_stubhead(), cells_stub(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(align = "left"), locations =  list(cells_title(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(style = "italic", weight = "bold"), locations =  cells_stub(rows = is.na(`Observed p`))) %>%
  tab_style(style = cell_borders(sides = c("top","bottom")), locations =  list(cells_body(rows = is.na(`Observed p`)), cells_stub(rows = is.na(`Observed p`)))) %>%
  tab_options(column_labels.border.bottom.color = "black", table.border.top.color = "transparent", table.border.bottom.width = 3, table.border.bottom.color = "black", table_body.hlines.color = "transparent", stub.border.color = "black", heading.border.bottom.width = 3, heading.border.bottom.color = "black", row_group.border.top.color = "black", row_group.border.bottom.color = "black", table_body.border.bottom.color = "black") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Observed p`,rows = `Observed p` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Observed p_adjusted`,rows = `Observed p_adjusted` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Shannon p`,rows = `Shannon p` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Shannon p_adjusted`,rows = `Shannon p_adjusted` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Pielou p`,rows = `Pielou p` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Pielou p_adjusted`,rows = `Pielou p_adjusted` <= 0.05)) %>%
  tab_footnote(footnote = "Bonferroni correction was used to adjust p-values, and significant p-values are bolded (α = 0.05).", locations = cells_title())


#Time Correlation Supplementary
tableCor <- timeCor
tableCor <- tableCor %>%
  gt(rowname_col = "Area.Type") %>%
  fmt_scientific(columns = rho, rows = abs(rho) < 0.1) %>%
  fmt_number(columns = rho, rows = abs(rho) > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = p, rows = p < 0.1) %>%
  fmt_number(columns = p, rows = p > 0.1, n_sigfig = 3) %>%
  tab_header(title = md("**Table S2.** Pearson correlation between observed richness and time after opening."),
             subtitle = NA) %>%
  tab_stubhead(label = md("**Area Type**")) %>%
  cols_label( rho = "ρ", `95% CI` = "95% Confidence Invterval", p = "p-value") %>%
  tab_style(style = cell_text(font = "Times New Roman", align = "center", size = 12), locations = list(cells_body(), cells_column_labels(), cells_title(), cells_stub(), cells_stubhead())) %>%
  tab_style(style = cell_text(align = "left"), locations =  cells_title()) %>%
  tab_style(style = cell_text(weight = "bold"), locations =  cells_column_labels()) %>%
  tab_options(
    column_labels.border.bottom.color = "black",
    table.border.top.color = "transparent",
    table.border.bottom.width = 3,
    table.border.bottom.color = "black",
    table_body.hlines.color = "transparent",
    stub.border.color = "black",
    heading.border.bottom.width  = 3,
    heading.border.bottom.color = "black"
  )
