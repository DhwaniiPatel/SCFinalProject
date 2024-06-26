clean_data <- function(gene) {
  gene_df <- read_csv(gene)
  # clean cell line, treatment and name fields
  gene_df$cell_line<-str_to_title(gene_df$cell_line)
  gene_df$treatment<-str_to_title(gene_df$treatment)
  gene_df$name<-str_to_title(tolower(gene_df$name))

  #write.csv(gene_df, "data/2024-06-12-clean-gene-data.csv", row.names = FALSE)
  return(gene_df)
}

clean_data2 <- function(gene, cell_line_req) {
  # create a new table for each cell line
  gene_cell <- subset(gene, cell_line == cell_line_req)
  return(gene_cell)
}


# movies_table4 |> filter(genres == "Drama")
# pacman::p_load(tidyverse, targets)
# tar_load(movies_file)
# clean_movies(movies_file)
