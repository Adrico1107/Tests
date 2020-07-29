#' Bootstrap approach to test significance
#' This function performs a bayesian test on a data source
#'
#' @param num Numerator column
#' @param denom Denominator column
#' @param metric Impact metric column
#' @param source Source dataframe, with StartEnd column
#'
#' @return The function returns a list with the test results
#' @import dplyr
#' @import ggplot2
#' @import plotly

######------------------ Bootstrap test auxiliary function

boot_sample_diff <- function (Total_sample,nS,nE){
  
  n<-nS+nE
  idx_S <- sample(1:n,nS,replace=T) #nS random indexes in [1,n] WITHOUT REPLACEMENT
  idx_E <- sample(1:n,nE,replace=T) #the rest of the indexes in [1,n]
  ratio_diff <- mean(Total_sample[idx_E])-mean(Total_sample[idx_S])
  return(ratio_diff)
}


impact_bootstrap <- function(num,denom,metric,source,error_target,impact_dim,start_start,start_end,start,end_start,end_end,end,return_plots){
  
  num <- "Purchase"
  denom <- "Sessions"
  
  start_start <- "A"
  end_start <- "B"
  
  return_plots <- TRUE
  
  
  ######------------------ Inititialise variables
  
  #start <- source %>% filter(StartEnd == "Start")
  #end <- source %>% filter(StartEnd == "End")
  
  sum_start_num <- 10
  sum_start_denom <- 100000
  sum_end_num <- 11
  sum_end_denom <- 100000
  
  sum_start_metric <- 20
  sum_end_metric <- 25
  
  ratio_start <- sum_start_num/sum_start_denom
  ratio_end <- sum_end_num/sum_end_denom
  delta_ratio_init<-ratio_end-ratio_start
  
  ratio_change <- ratio_end/ratio_start-1 # What if ratio_start is 0 ?
  abs_numerator_change <- sum(sum_start_denom,sum_end_denom)*sum_end_num/sum_end_denom-sum(sum_start_denom,sum_end_denom)*sum_start_num/sum_start_denom
  impact_on_numerator <- sum(sum_start_metric,sum_end_metric)/sum(sum_start_num,sum_end_num) # What if start and end numerators are both equal to 0
  metric_change <- abs_numerator_change*impact_on_numerator
  
  error_target <- 0.95
  alpha <- 1-error_target
  
  
  ######------------------ Bootstrap test 
  
  Start_sample <- c(rep(1,sum_start_num),rep(0,sum_start_denom-sum_start_num))
  End_sample <- c(rep(1,sum_end_num),rep(0,sum_end_denom-sum_end_num))
  Total_sample <- c(Start_sample,End_sample)
  
  R<-100
  boot_diff <- rep(0,(5*R))
  PVALSUP <- rep(0,5)
  PVALINF <- rep(0,5)
  Total_sample <- append(Start_sample,End_sample)
  
  
  for (j in 1:5){
    boot_diff_j <- rep(0,R)
  for (i in 1:R){
    boot_diff[i]<-boot_sample_diff(Total_sample,sum_start_denom,sum_end_denom)
  }
  boot_diff <- append(boot_diff,boot_diff_j)
  pval_boot_s <- round(mean(boot_diff_j>=delta_ratio_init),5)
  PVALSUP[j] <- pval_boot_s
  pval_boot_i <- mean(boot_diff_j<=delta_ratio_init)
  PVALINF[j] <- pval_boot_i
  }
  
  print(PVALSUP)
  pval_boot_sup <- round(mean(PVALSUP),3)
  pval_boot_inf <- mean(PVALINF)
  
  if (pval_boot_sup<=alpha){
    assessed_output <- 1
    significant <- TRUE
    pval_boot <- pval_boot_sup
  }else if (pval_boot_inf<=alpha){
    assessed_output <- -1
    significant <- TRUE
    pval_boot <- pval_boot_inf
  }else{
    assessed_output <- 0
    significant <- FALSE
    pval_boot <- min(pval_boot_sup,pval_boot_inf)
  }
  
  print(pval_boot_sup)
  print(pval_boot_inf)
  print(assessed_output)
  
  
  
  ######------------------ Text labels
  
  ratio_name <- ifelse(denom=="NULLdenom", num, paste0(num,"/",denom))
  print(ratio_name)
  title_bootstrap <- paste0("Density distribution for the mean ", ratio_name," differences between pages A and B")
  print(title_bootstrap)
  
  StartEnd_Type <- ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",source[[impact_dim]][1]), "DateRange",
                          ifelse(is.numeric(source[[impact_dim]][1]), "NumRange", "AnyOtherType"))
  StartEnd_Type <- "AnyOtherType"
  
  range_start <- paste0(start_start, ifelse(StartEnd_Type=="DateRange", paste0(" to ",start_end), ""))
  range_end <- paste0(end_start, ifelse(StartEnd_Type=="DateRange", paste0(" to ",end_end), ""))
  
  lab_start <- paste0("(", ratio_name, ") for ", range_start)
  lab_end <- paste0("(", ratio_name,") for ", range_end)
  
  value_start <- paste0(round(100*ratio_start,2),"%")
  value_end <- paste0(round(100*ratio_end,2),"%")
  
  label_start <- paste0("Mean ", lab_start, ": ", value_start)
  label_end <- paste0("Mean ", lab_end, ": ", value_end)
  
  align_start <- ifelse(ratio_end < ratio_start, "left", "right") #WTFFFFF
  align_end <- ifelse(align_start=="right", "left", "right")
  
  
  ######------------------ Bootstrap results plot
  
  
  if (return_plots) {
    
    # Prepare the data
    df <- data.frame(differences=boot_diff)
    
    # Density plot with vertical line (initial mean difference)
    boot_plot <- ggplot(df, aes(x=differences)) + geom_density(alpha=.2, fill="red")+ 
      geom_vline(aes(xintercept=delta_ratio_init),color="blue", linetype="dashed", size=1)+labs(title=title_bootstrap,x=paste0(ratio_name,"differences"), y = "Density")
    
    # Fill in differently the areas on both sides of the vertical line and add the real histogram in the background
    dpb <- ggplot_build(boot_plot)
    x1 <- min(which(dpb$data[[1]]$x >=delta_ratio_init))
    
    
    # Make it interactive
    final_plot <- ggplotly(boot_plot +
                             geom_area(data=data.frame(x=dpb$data[[1]]$x[x1:length(dpb$data[[1]]$x)],
                                                       y=dpb$data[[1]]$y[x1:length(dpb$data[[1]]$y)]),
                                       aes(x=x, y=y), fill="green",alpha=0.3) + geom_histogram(aes(y=..density..), colour="black", fill="white",alpha=0.2))
    
  }
  
  final_plot
  
  
  ######------------------ Bootstrap results table
  
  col1 <- data.frame(c(num,denom,"Overall ratio"))
  Start <- c(10, 1000, paste0(round(11/1000,3)*100,"%"))
  #print(paste0(round(10/1000,2)*100,"%"))
  End   <- c(12,   1000, paste0(round(17/1000,3)*100,"%")) #Alternative : label_percent()(x)
  Evolution <- c((12/10-1),
                 (1000/1000-1),
                 ((ratio_end/ratio_start)-1)) #Needs to have the same nb of chiffres significatifs as other ratios
  Evolution <- paste0(round(Evolution,2)*100,"%")
  
  output_table <- data.frame(col1,Start,End,Evolution)
  names(output_table) <- c("Parameter","Start","End","Evolution")
  
  print(output_table)
  
  ######------------------ Return the output
  
  text_sign <- paste0("The difference in mean (", ratio_name, ") is ",
                      ifelse(significant, "", "not "), "significant at the ", 100*(1-error_target),"% level.")
  text_result <- paste(paste0("A Bootstrap permutation test was used to evaluate the significance of the difference in means between the two periods:"),
                       label_start, label_end, text_sign, sep="\n")
  
  print(text_sign)
  print(text_result)
  
  out$metric_name  <- "p-value"
  out$metric_value <- pval_boot
  out$ratio_change <- ratio_change
  out$metric_change <- round(metric_change,2)
  if (return_plots) out$graph <- final_plot
  out$table <- output_table
  out$text_result <- text_result
  out$significant  <- ifelse(significant, "Yes", "No")
  out$significant_yn  <- ifelse(significant, "Yes", "No")
  out$xmore  <- NULL
  
  out$help$title <- "Bootstrap permutation test result"
  out$help$content <- "The vertical line shows the observed difference for the initial A and B samples"
  
  
  return(out)  
  
}

