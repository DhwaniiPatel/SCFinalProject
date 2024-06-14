get_table <- function(gene_cell) {
  table <- gene_cell |>
    distinct(cell_line, treatment, name) |>
    arrange(desc(treatment),desc(cell_line)) |>
    gt() |>
    fmt_number(col = 2)
  gtsave(table, "tabs/gene_cell.html")
  return(table)
}

# growth rates
gene_name <- function(gene,name1,cell_line1,treatment1) {
  gene_name <- subset(gene,
                      name == name1)
  return(gene_name)
}

growth_rate <- function(gene_cell) {
  gene_cell_growth = gene_cell |> arrange(conc) |>
    mutate(Diff_conc = conc - lag(conc),  # Difference in concentration
           Diff_growth = gene_expression - lag(gene_expression), # Difference in gene expression
           Rate = (Diff_growth / Diff_conc)) # growth rate in percent
  return(gene_cell_growth)
}

get_table_growth <- function(gene) {

  Cdz = growth_rate(gene_name(gene,'Gl-Cdz'))$Rate[2:11]
  Xib = growth_rate(gene_name(gene,'Gl-Xib'))$Rate[2:11]
  Gl_Rjs = growth_rate(gene_name(gene,'Gl-Rjs'))$Rate[2:11]
  Gl_Xik = growth_rate(gene_name(gene,'Gl-Xik'))$Rate[2:11]

  Cwn = growth_rate(gene_name(gene,'Gl-Cwn'))$Rate[2:11]
  Kyh = growth_rate(gene_name(gene,'Gl-Kyh'))$Rate[2:11]
  Gl_Mfa = growth_rate(gene_name(gene,'Gl-Mfa'))$Rate[2:11]
  Gl_Zhw = growth_rate(gene_name(gene,'Gl-Zhw'))$Rate[2:11]

  # combine rates
  Concentration = growth_rate(gene_name(gene,'Gl-Rjs'))$conc[2:11]
  df <- data.frame(Concentration, Gl_Mfa, Gl_Rjs, Gl_Xik, Gl_Zhw)
  df |> gt()
  #gtsave(df, "tabs/growth_rate.html")
  #write.csv(df, "data/2024-06-12-growth_rate.csv", row.names = FALSE)
}

model_results <- function(model_output) {
  results <- tab_model(model_output, file = "data/model_output.html")
  return(results)
}

# pacman::p_load(tidyverse, targets, gt)
# tar_load(movies_clean)
# get_table(movies_clean)
