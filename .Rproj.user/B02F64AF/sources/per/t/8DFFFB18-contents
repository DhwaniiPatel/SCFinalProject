fit_model <- function(gene) {

  options(scipen = 999)

  gene_mixed = lmer(gene_expression ~ conc+treatment+cell_line
                     + (1 | name), REML = FALSE, data = gene)

  #summary(gene_mixed)$coeff
  #anova(gene_mixed)
  #confint(gene_mixed)
  #sjPlot::tab_model(gene_mixed2)
  #report::report(gene_mixed2)

  return(gene_mixed)
}
# pacman::p_load(tidyverse, targets)
# tar_load(mpg_clean)
# fit_model(mpg_clean)
