denoising.stats <- read.delim("./Data/denoising-stats.tsv", comment.char="#")

tableDenoise <- denoising.stats %>%
  mutate(percentage.of.input.denoised = denoised/input*100, .after = denoised) %>%
  select(sample.id, input, percentage.of.input.passed.filter, percentage.of.input.denoised, percentage.of.input.merged, percentage.of.input.non.chimeric)

tableDenoise <- tableDenoise %>%
  gt(rowname_col = "sample.id") %>%
  fmt_percent(columns = c(percentage.of.input.passed.filter, percentage.of.input.denoised, percentage.of.input.merged, percentage.of.input.non.chimeric), scale_values = FALSE) %>%
  tab_header(
    title = md(
      "**Table S7.** Denoising Statistics for Each Sample."
    ),
    subtitle = NA
  ) %>%
  tab_stubhead(label = md("**Sample ID**")) %>%
  cols_label(
    sample.id = "Sample ID",
    input = "Number of Unfiltered Reads",
    percentage.of.input.passed.filter = "Filtered",
    percentage.of.input.denoised = "Denoised",
    percentage.of.input.merged = "Merged",
    percentage.of.input.non.chimeric = "Non-chimeric"
  ) %>%
  tab_spanner(label = md("**Percentage of Reads Remaining After Each Quality Step**"), columns = c(percentage.of.input.passed.filter, percentage.of.input.denoised, percentage.of.input.merged, percentage.of.input.non.chimeric)) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", align = "center", size = 12),
    locations = list(
      cells_body(),
      cells_column_labels(),
      cells_title(),
      cells_stub(),
      cells_stubhead(),
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

#Averages  
tableDenoiseAvg <- denoising.stats %>%
  mutate(percentage.of.input.denoised = denoised/input*100, .after = denoised) %>%
  select(input, percentage.of.input.passed.filter, percentage.of.input.denoised, percentage.of.input.merged, percentage.of.input.non.chimeric) %>%
  summarise(input.avg = mean(input), 
            input.sd = sd(input),
            percentage.of.input.passed.filter.avg = mean(percentage.of.input.passed.filter),
            filter.sd = sd(percentage.of.input.passed.filter),
            percentage.of.input.denoised.avg = mean(percentage.of.input.denoised), 
            denoise.sd = sd(percentage.of.input.denoised),
            percentage.of.input.merged.avg = mean(percentage.of.input.merged),
            merged.sd = sd(percentage.of.input.merged),
            percentage.of.input.non.chimeric.avg = mean(percentage.of.input.non.chimeric),
            non.chimeric.sd = sd(percentage.of.input.non.chimeric))

tableDenoiseAvg <- tableDenoiseAvg %>%
  gt() %>%
  fmt_percent(columns = 3:10, scale_values = FALSE) %>%
  fmt_number(columns = 1:2, decimals = 0) %>%
  tab_header(
    title = md(
      "**Table S7.** Average Denoising Statistics."
    ),
    subtitle = NA
  ) %>%
  tab_stubhead(label = md("**Number of Unfiltered Reads**")) %>%
  cols_label(
    input.avg = "Average",
    input.sd = "SD",
    percentage.of.input.passed.filter.avg = "Average",
    filter.sd = "SD",
    percentage.of.input.denoised.avg = "Average",
    denoise.sd = "SD",
    percentage.of.input.merged.avg = "Average",
    merged.sd = "SD",
    percentage.of.input.non.chimeric.avg = "Average",
    non.chimeric.sd = "SD"
  ) %>%
  tab_spanner(label = md("**Number of Unfiltered Reads**"), columns = c(input.avg, input.sd)) %>%
  tab_spanner(label = md("Filtered"), columns = c(percentage.of.input.passed.filter.avg, filter.sd)) %>%
  tab_spanner(label = md("Denoised"), columns = c(percentage.of.input.denoised.avg, denoise.sd)) %>%
  tab_spanner(label = md("Merged"), columns = c(percentage.of.input.merged.avg, merged.sd)) %>%
  tab_spanner(label = md("Non-chimeric"), columns = c(percentage.of.input.non.chimeric.avg, non.chimeric.sd)) %>%
  tab_spanner(label = md("**Percentage of Reads Remaining After Each Quality Step**"), columns = 3:10) %>%
  tab_style(
    style = cell_text(font = "Times New Roman", align = "center", size = 12),
    locations = list(
      cells_body(),
      cells_column_labels(),
      cells_title(),
      cells_stub(),
      cells_stubhead(),
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
