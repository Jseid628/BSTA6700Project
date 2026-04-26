
create_figure <- function() {
  cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  bias_fig_name <- 'figure/ortho_bias_fig.pdf'
  
  all_bias_sim_long <- readRDS("results/sim_biasOrtho_4-26.rds")
  
  all_bias_sim_long$setting <- factor(all_bias_sim_long$setting, levels = c("T_0=10,K=4", "T_0=10,K=10", "T_0=40,K=4", "T_0=40,K=10"))
  
  pdf(bias_fig_name)
  print(
    all_bias_sim_long %>%
      ggplot(aes(x=method, y=bias, fill=method)) +
      geom_boxplot(notch=FALSE,outlier.shape=NA) +
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="black", fill="black") +
      facet_wrap(~setting, scales = "fixed", nrow = 2) + # Set scales as fixed
      geom_hline(yintercept = 0, color = "black") + # Add a horizontal line at 0 to all facets
      ylim(-0.25,2) + 
      labs(y = "Bias", x = "SC Method") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
            axis.text.y = element_text(size=12),
            axis.title.x = element_text(size=14, face="bold"),
            axis.title.y = element_text(size=14, face="bold"),
            plot.title = element_text(size=16, face="bold", hjust=0.5),
            plot.margin = unit(c(0,0,0,0), "lines"),
            aspect.ratio = 3/4) +
      scale_fill_manual(values=cbp1)
  )
  dev.off()
}
