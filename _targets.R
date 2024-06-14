#install.packages("sjPlot")
#install.packages("reshape")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("emmeans")
#install.packages("ggpubr")
#install.packages("ggrepel")
#install.packages("stringr")
#install.packages("targets")
#install.packages("tarchetypes")
library(sjPlot)
library(reshape)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)
library(ggrepel)
library(stringr)
library(targets)
library(tarchetypes)
pacman::p_load(tidyverse,gt)
# Set target options:
tar_option_set(
  packages = c("tidyverse", "gt"),
  format = "rds"
)

tar_source()

list(
  tar_file(gene_data, "raw-data/2024-03-19-WIF-tis4d.csv"),
  # clean data
  tar_target(gene_data_clean, clean_data(gene_data)),
  tar_target(gene_cell1, clean_data2(gene_data_clean, 'Wild-Type')),
  tar_target(gene_cell2, clean_data2(gene_data_clean, 'Cell-Type 101')),
  # table 1: groups of cell lines and treatments
  tar_target(categories, get_table(gene_data_clean)),
  # plot 1: relationships between cell concentration and gene expression
  tar_target(gene_plot, plot_scatter(gene_cell1, gene_cell2)),
  # table 2: growth rates of gene expression
  tar_target(growth, get_table_growth(gene_data_clean)),
  # model: mixed effects with random intercept
  tar_target(model_output, fit_model(gene_data_clean)),
  # output: results of the model
  tar_target(results, model_results(model_output)),
  # plot 2: residual of predicted
  tar_target(residual_plot, plot2(model_output, gene_data_clean)),
  # readme file
  tar_quarto(readme, "readme.qmd")
)
