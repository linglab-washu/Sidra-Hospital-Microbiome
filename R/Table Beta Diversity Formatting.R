tableBetaBeforeAfter <- perm.pOpening

tableBetaAfter <- perm.p[,c(1,2,5:10)]
tableBetaAfter$Row.names <- c("Surface Category", "Area Type", "Days After Opening")

tableBetaBeforeAfter <- tableBetaBeforeAfter %>%
  add_column(disper_p_adjusted = NA,.after = "disper_p_Opening")

colnames(tableBetaBeforeAfter) = colnames(tableBetaAfter)

tableBeta <- rbind(tableBetaBeforeAfter, tableBetaAfter)

tableBeta <- tableBeta %>%
  gt(rowname_col = "Row.names") %>%
  fmt_scientific(columns = F.Model, rows = F.Model < 0.1) %>%
  fmt_number(columns = F.Model, rows = F.Model > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = R2, rows = R2 < 0.1) %>%
  fmt_number(columns = R2, rows = R2 > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Pr(>F)`, rows = `Pr(>F)` < 0.1) %>%
  fmt_number(columns = `Pr(>F)`, rows = `Pr(>F)` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = disper_f, rows = disper_f < 0.1) %>%
  fmt_number(columns = disper_f, rows = disper_f > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = disper_p, rows = disper_p < 0.1) %>%
  fmt_number(columns = disper_p, rows = disper_p > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = disper_p_adjusted, rows = disper_p_adjusted < 0.1) %>%
  fmt_number(columns = disper_p_adjusted, rows = disper_p_adjusted > 0.1, n_sigfig = 3) %>%
  sub_missing(missing_text = "-") %>%
  tab_header(title = md("**Table 2.** Comparisons of centroids in principal coordinate analysis (PCoA)."),
             subtitle = NA) %>%
  tab_spanner(label = md("**PERMANOVA**"), columns = c(F.Model, R2, `Pr(>F)`)) %>%
  tab_spanner(label = md("**PERMDISP**"),columns = c(disper_f, disper_p, disper_p_adjusted)) %>%
  tab_stubhead(label = md("**Grouping**")) %>%
  cols_label(Df = "Degrees of Freedom", F.Model = "F value", R2 = "R2", `Pr(>F)` = "p-value", disper_f = "F value", disper_p = "p-value", disper_p_adjusted = "p-adjusted") %>%
  tab_row_group(label = "A) PERMANOVA test on centroids and PERMDISP test on dispersions of PCoA in NICU patient rooms before and after opening.", rows = 1) %>%
  tab_row_group(label = "B) PERMANOVA tests on centroids and PERMDISP test on dispersions of PCoA in different surface categories, area types, and days after opening in all door handle, keyboard, and office electronic samples taken after the hospital opened for inpatient care (n = 120).", rows = 2:4) %>%
  row_group_order(groups = c("A) PERMANOVA test on centroids and PERMDISP test on dispersions of PCoA in NICU patient rooms before and after opening.", "B) PERMANOVA tests on centroids and PERMDISP test on dispersions of PCoA in different surface categories, area types, and days after opening in all door handle, keyboard, and office electronic samples taken after the hospital opened for inpatient care (n = 120).")) %>%
  tab_style(style = cell_text(font = "Times New Roman", align = "center", size = 12), locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title(), cells_stubhead(), cells_stub(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(align = "left"), locations =  list(cells_title(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(style = "italic", weight = "bold"), locations =  cells_stub(rows = is.na(Df))) %>%
  tab_style(style = cell_borders(sides = c("top","bottom")), locations =  list(cells_body(rows = is.na(Df)), cells_stub(rows = is.na(Df)))) %>%
  tab_options(column_labels.border.bottom.color = "black", table.border.top.color = "transparent", table.border.bottom.width = 3, table.border.bottom.color = "black", table_body.hlines.color = "transparent", stub.border.color = "black", heading.border.bottom.width = 3, heading.border.bottom.color = "black", row_group.border.top.color = "black", row_group.border.bottom.color = "black", table_body.border.bottom.color = "black") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Pr(>F)`,rows = `Pr(>F)` <= 0.05)) %>%
  tab_footnote(footnote = "ASV relative abundances were square-root transformed prior to calculating the dissimilarity.", locations = cells_title()) %>%
  tab_footnote(footnote = "Bonferroni correction was used to adjust p-values, and significant p-values are bolded (α = 0.05).", locations = cells_row_groups(groups = 2))


#Pairwise Supplemntary
tableBetaSupp <- rbind(pw.permSurface[,c(1,2,4:7)], pw.permArea[,c(1,2,4:7)])
tableBetaSupp <- tableBetaSupp %>%
  add_row(pairs = "Surface Category",.before = 1) %>%
  add_row(pairs = "Area Type",.before = 5) 

colnames(tableBetaSupp) <- c("Row.names", "Df", "F.Model", "R2", "Pr(>F)", "perm_p_adjusted")

tableBetaSupp <- tableBetaSupp %>%
  gt(rowname_col = "Row.names") %>%
  fmt_scientific(columns = F.Model, rows = F.Model < 0.1) %>%
  fmt_number(columns = F.Model, rows = F.Model > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = R2, rows = R2 < 0.1) %>%
  fmt_number(columns = R2, rows = R2 > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = `Pr(>F)`, rows = `Pr(>F)` < 0.1) %>%
  fmt_number(columns = `Pr(>F)`, rows = `Pr(>F)` > 0.1, n_sigfig = 3) %>%
  fmt_scientific(columns = perm_p_adjusted, rows = perm_p_adjusted < 0.1) %>%
  fmt_number(columns = perm_p_adjusted, rows = perm_p_adjusted > 0.1, n_sigfig = 3) %>%
  sub_missing(missing_text = "-") %>%
  sub_missing(row = c(1,5), missing_text = "") %>%
  tab_header(title = md("**Table S7.** Pairwise PERMANOVA tests on centroids of PCoA on the various area types in different surface categories, area types, and days after opening in all door handle, keyboard, and office electronic samples taken after the hospital opened for inpatient care (n = 120)."),
             subtitle = NA) %>%
  tab_spanner(label = md("**PERMANOVA**"), columns = c(F.Model, R2, `Pr(>F)`,perm_p_adjusted)) %>%
  tab_stubhead(label = md("**Grouping**")) %>%
  cols_label(Df = "Degrees of Freedom", F.Model = "F value", R2 = "R2", `Pr(>F)` = "p-value", perm_p_adjusted = "p-adjusted") %>%
  tab_style(style = cell_text(font = "Times New Roman", align = "center", size = 12), locations = list(cells_body(), cells_column_labels(), cells_column_spanners(), cells_title(), cells_stubhead(), cells_stub(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(align = "left"), locations =  list(cells_title(), cells_row_groups(), cells_footnotes())) %>%
  tab_style(style = cell_text(style = "italic", weight = "bold"), locations =  cells_stub(rows = is.na(Df))) %>%
  tab_style(style = cell_borders(sides = c("top","bottom")), locations =  list(cells_body(rows = is.na(Df)), cells_stub(rows = is.na(Df)))) %>%
  tab_options(column_labels.border.bottom.color = "black", table.border.top.color = "transparent", table.border.bottom.width = 3, table.border.bottom.color = "black", table_body.hlines.color = "transparent", stub.border.color = "black", heading.border.bottom.width = 3, heading.border.bottom.color = "black", row_group.border.top.color = "black", row_group.border.bottom.color = "black", table_body.border.bottom.color = "black") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = `Pr(>F)`,rows = `Pr(>F)` <= 0.05)) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = perm_p_adjusted,rows = perm_p_adjusted <= 0.05)) %>%
  tab_footnote(footnote = "ASV relative abundances were square-root transformed prior to calculating the dissimilarity.", locations = cells_title()) %>%
  tab_footnote(footnote = "Bonferroni correction was used to adjust p-values, and significant p-values are bolded (α = 0.05).", locations = cells_title())
