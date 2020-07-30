######------------------ Bootstrap results plot (using ggplot)


if (return_plots) {
  
  # Prepare the data
  df <- data.frame(differences=boot_diff)
  
  boot_diff_sorted <- sort(boot_diff, decreasing = FALSE)
  xmin <- quantile(boot_diff_sorted,alpha)
  xmax <- quantile(boot_diff_sorted,1-alpha)
  
  # Density plot with vertical line (initial mean difference)
  boot_plot <- ggplot(df, aes(x=differences)) + geom_density(alpha=.2, fill="grey")+ 
    geom_vline(aes(xintercept=xmin),color="red", linetype="dashed", size=0.5)+geom_vline(aes(xintercept=xmax),color="green", linetype="dashed", size=0.5)+
    geom_vline(aes(xintercept=delta_ratio_init),color="blue", linetype="solid", size=1)+
    labs(title=title_bootstrap,x=paste0(ratio_name," differences"), y = "Density")
  
  # Fill in differently the areas on both sides of the vertical line and add the real histogram in the background
  dpb <- ggplot_build(boot_plot)
  x1 <- max(which(dpb$data[[1]]$x <=xmin))
  x2 <- max(which(dpb$data[[1]]$x <=xmax))
  
  
  # Make it interactive
  final_plot <- ggplotly(boot_plot +
                           geom_area(data=data.frame(x=dpb$data[[1]]$x[0:x1],
                                                     y=dpb$data[[1]]$y[0:x1]),
                                     aes(x=x, y=y), fill="red",alpha=0.3)+
                           
                           geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:x2],
                                                     y=dpb$data[[1]]$y[x1:x2]),
                                     aes(x=x, y=y), fill="grey",alpha=0.3) +
                           
                           geom_area(data=data.frame(x=dpb$data[[1]]$x[x2:length(dpb$data[[1]]$x)],
                                                     y=dpb$data[[1]]$y[x2:length(dpb$data[[1]]$x)]),
                                     aes(x=x, y=y), fill="green",alpha=0.3) + 
                           geom_histogram(aes(y=..density..), colour="black", fill="white",alpha=0.2))
}