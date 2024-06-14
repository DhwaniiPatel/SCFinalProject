plot_scatter <- function(gene_cell1,gene_cell2) {

  col_pal <- c("deepskyblue3", "burlywood3", "deepskyblue3","burlywood3")
  col_pal2 <- c( "burlywood3", "deepskyblue3","burlywood3","deepskyblue3")
  col_pal3 <- c("burlywood3","burlywood3","deepskyblue3","deepskyblue3")

  plot1 <- gene_cell1 |> ggplot(aes(x = conc, y = gene_expression, group = name, col = treatment, label=name))+
    geom_point(size = 2.5) +
    geom_label_repel(data=gene_cell1 |> filter(conc == 10), aes(label=name), size=4,colour = 'black', box.padding = 0.5, fill = col_pal2, family = 'Times')+
    theme(text = element_text(family = "times", size = 12))+
    theme(
      panel.background = element_rect(fill = "white", colour = "black",
                                      size = 0.5, linetype = 'solid'),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "lightgray"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "lightgray")
    )+
    ggtitle("Wild Type") +
    xlab(expression(paste(mu,"g/ml"))) +
    ylab("Gene expression") +
    scale_x_continuous(breaks = seq(0, 10, by = 1)) +
    scale_y_continuous(breaks = seq(0, 50, by = 10)) +
    scale_color_manual(name = "Treatment",labels = c("Activating Factor 42"
                                                     ,"Placebo"), values = col_pal) +
    theme(legend.position="top", plot.title = element_text(hjust = 0, vjust=2.12), legend.key=element_blank()) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size = 10))

  plot2 <- gene_cell2 |> ggplot(aes(x = conc, y = gene_expression, group = name, col = treatment))+
    geom_point(size = 2.5) +
    geom_label_repel(data=gene_cell2 |> filter(conc == 10), aes(label=name), size=4,colour = 'black', box.padding = 0.5,fill = col_pal3, family = 'Times')+
    theme(text = element_text(family = "times", size = 12))+
    theme(
      panel.background = element_rect(fill = "white", colour = "black",
                                      size = 0.5, linetype = 'solid'),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "lightgray"),
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "lightgray")
    )+
    ggtitle("Cell Type 101") +
    xlab(expression(paste(mu,"g/ml"))) +
    ylab("Gene expression") +
    scale_x_continuous(breaks = seq(0, 10, by = 1)) +
    scale_y_continuous(breaks = seq(0, 50, by = 10)) +
    scale_color_manual(name = "Treatment", labels = c("Activating Factor 42"
                                                      ,"Placebo"), values = col_pal) +
    theme(legend.position="top", plot.title = element_text(hjust = 0, vjust=2.12), legend.key=element_blank()) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size = 10))

  plot <- ggarrange(plot1, plot2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom",labels = c("A", "B")) +
    theme(plot.margin = margin(1,0.5,2,0.5, "cm"))
   #ggsave('figs/gene_plot.tiff', plot, device = "tiff", dpi = 500, bg = 'white',width = 9, height = 6)

}

plot2 <- function(gene_mixed,gene) {
  ## Plotting our own residual ~ fitted
  lmer_fitted <- predict(gene_mixed, newdata = gene, re.form = ~(1 |name))
  lmer_resid <- gene$gene_expression - lmer_fitted

  gene_data <- data.frame(lmer_fitted, lmer_resid)

  plot <- gene_data |> ggplot(aes(x = lmer_fitted, y = lmer_resid))+
    geom_point() +
    ggtitle("Residual") +
    xlab("Concentration") +
    ylab("Residuals") +
    geom_hline(yintercept=mean(lmer_resid),color="red") +
    theme(legend.position="top", legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size = 9))

  #ggsave('figs/Residual.tiff', plot, device = "tiff", dpi = 500, bg = 'white',width = 9, height = 6)
}




# pacman::p_load(tidyverse, targets)
# tar_load(movies_clean)
# plot_scatter(movies_clean)
