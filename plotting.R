if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
if (!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

createBarPlot <- function(df, outcome, arm = NULL) {
  # Default arm to "all" if not specified
  if (is.null(arm)) {
    condition_label <- "music (all groups)" # Default condition label
    arm <- "all"               # Default arm value
  } else {
    condition_label <- arm     # Use specified arm as condition label
  }
  
  # Calculate summary statistics
  summary_stats <- calculateStats(df, outcome, arm)
  
  # Filter dataset based on specified arm if it's not "all"
  if (arm != "all") {
    df <- df %>% filter(arm == !!arm)
  }
  
  # Prepare data for plotting
  plot_data <- summary_stats %>%
    mutate(time_point = ifelse(grepl("_v2$", arm), "Pre", "Post")) %>%
    arrange(time_point)
  
  # Prepare jitter data for individual data points
  jitter_data <- df %>%
    mutate(time_point = factor(ifelse(visit == 2, "Pre", "Post"), levels = c("Pre", "Post")))
  
  # Select relevant columns before performing t-test
  t_test_data <- df %>%
    select(src_subject_id, visit, !!sym(outcome)) %>%
    spread(visit, !!sym(outcome)) %>%
    drop_na()
  
  n_value <- nrow(t_test_data)
  if (n_value < 2) {
    stop("Not enough observations for paired t-test.")
  }
  
  t_test_result <- t.test(t_test_data$`2`, t_test_data$`7`, paired = TRUE)
  
  # Get the sample size (n value)
  n_value <- nrow(t_test_data)
  
  # Create legend text with statistical information
  legend_text <- sprintf(
    "Mean\nPre: %.4f\nPost: %.4f\n\nn = %d\n\nt-test\nt = %.4f\ndf = %d\np = %.4f",
    plot_data$mean[plot_data$time_point == "Pre"],
    plot_data$mean[plot_data$time_point == "Post"],
    n_value,
    t_test_result$statistic,
    t_test_result$parameter,
    t_test_result$p.value
  )
  
  # Check if plot_data has valid means for y-axis limit calculation
  max_mean <- ifelse(any(!is.na(plot_data$mean)), max(plot_data$mean, na.rm = TRUE), 0)
  
  # Create the plot
  plot <- ggplot(plot_data, aes(x = time_point, y = mean, fill = time_point)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                  width = 0.2, colour = "black", alpha = 0.9, size = 1) +
    geom_point(data = jitter_data, aes(x = time_point, y = !!sym(outcome)), 
               alpha = 0.3, position = position_jitter(width = 0.2)) +
    annotate("text", x = Inf, y = Inf, label = legend_text, 
             hjust = 1, vjust = 1, size = 5, fontface = "bold") +
    annotate("text", x = 0.5, y = max(max_mean * 1.2, max(df[[outcome]], na.rm = TRUE)), 
             label = condition_label, hjust = 0, vjust = 1, size = 6, fontface = "bold") +
    labs(y = paste("Mean", outcome)) +
    scale_fill_manual(values = c("Pre" = "skyblue", "Post" = "lightgreen")) +
    scale_x_discrete(limits = c("Pre", "Post")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white")) +
    coord_cartesian(ylim = c(0, max(max_mean * 1.5, max(df[[outcome]], na.rm = TRUE))))
  
  return(plot)
}



createScatterplot <- function(data, x_var, y_var, x_label = NULL, y_label = NULL, group = "all") {

  # Create proper labels if not provided
  if (is.null(x_label)) x_label <- gsub("_", " ", x_var)
  if (is.null(y_label)) y_label <- gsub("_", " ", y_var)
  
  # Calculate correlation and p-value
  cor_test <- cor.test(data[[x_var]], data[[y_var]])
  r <- cor_test$estimate
  p <- cor_test$p.value
  n <- length(data[[x_var]])
  
  # Format correlation and p-value
  cor_label <- sprintf("r = %.2f, p = %.3f", r, p)
  
  # Create group label
  group_label <- if (group == "all") "Music (All Groups)" else paste0("Arm: ", group)
  
  # Create the plot
  scatterplot <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(color = 'royalblue4') +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x.bottom = element_line(linewidth = .6),
          axis.line.y.left = element_line(linewidth = .6),
          axis.title.x = element_text(size = 20, face = "bold"),
          axis.title.y = element_text(size = 20, face = "bold"),
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5)) +
    geom_smooth(method = 'lm', formula = y ~ x, color = "black") +
    labs(x = paste0("∆ ", x_label),
         y = paste0("∆ ", y_label),
         title = group_label)
  
  # Add correlation information to the plot
  scatterplot <- scatterplot + annotate("text", x = min(data[[x_var]]), y = max(data[[y_var]]),
                    label = cor_label, hjust = 0, vjust = 1)
  
  # Add sample size to the plot
  scatterplot <- scatterplot + annotate("text", x = max(data[[x_var]]), y = min(data[[y_var]]),
                    label = paste("n =", n), hjust = 1, vjust = 0)
  
  # Save the plot
  plot_name <- paste0("plots/correlations/", x_var, "_vs_", y_var, "_", group, "_scatter.png")
  ggsave(plot_name, plot = scatterplot, width = 10, height = 8, dpi = 300)
  
  # Return the plot object
  return(scatterplot)
}


# 
# createScatterplot <- function(data, x_var, y_var, x_label = NULL, y_label = NULL, group = "all") {
#   library(dplyr)
#   library(ggplot2)
#   
#   # Create proper labels if not provided
#   if (is.null(x_label)) x_label <- gsub("_", " ", x_var)
#   if (is.null(y_label)) y_label <- gsub("_", " ", y_var)
#   
#   # Calculate correlation and p-value
#   cor_test <- cor.test(data[[x_var]], data[[y_var]])
#   r <- cor_test$estimate
#   p <- cor_test$p.value
#   n <- length(data[[x_var]])
#   
#   # Format correlation and p-value
#   cor_label <- sprintf("r = %.2f, p = %.3f", r, p)
#   
#   # Create group label
#   group_label <- if (group == "all") "Music (All Groups)" else paste0("Music (", group, ")")
#   
#   # Identify outliers (more than 2 standard deviations from the mean)
#   outliers <- data %>%
#     mutate(
#       x_outlier = abs(scale(!!sym(x_var))) > 2,
#       y_outlier = abs(scale(!!sym(y_var))) > 2,
#       is_outlier = x_outlier | y_outlier
#     )
#   
#   # Print outliers
#   outlier_data <- outliers %>% filter(is_outlier)
#   cat("\nOutliers (more than 2 standard deviations from the mean):\n")
#   if (nrow(outlier_data) > 0) {
#     print(outlier_data %>% select(all_of(c(x_var, y_var, "x_outlier", "y_outlier"))))
#     cat(sprintf("\nTotal number of outliers: %d\n\n", nrow(outlier_data)))
#   } else {
#     cat("No outliers detected.\n\n")
#   }
#   
#   # Automatically filter out outliers
#   data_filtered <- outliers %>% filter(!is_outlier)
#   cat("Outliers removed. Creating plot with filtered data.\n")
#   
#   # Create the plot with filtered data
#   scatterplot <- ggplot(data_filtered, aes(x = .data[[x_var]], y = .data[[y_var]])) +
#     geom_point(color = "royalblue4") +
#     theme_minimal() +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.line.x.bottom = element_line(linewidth = .6),
#           axis.line.y.left = element_line(linewidth = .6),
#           axis.title.x = element_text(size = 20, face = "bold"),
#           axis.title.y = element_text(size = 20, face = "bold"),
#           plot.title = element_text(size = 22, face = "bold", hjust = 0.5)) +
#     geom_smooth(method = 'lm', formula = y ~ x, color = "black") +
#     labs(x = paste0("∆ ", x_label),
#          y = paste0("∆ ", y_label),
#          title = group_label)
#   
#   # Add correlation information to the plot
#   scatterplot <- scatterplot + annotate("text", x = min(data_filtered[[x_var]]), y = max(data_filtered[[y_var]]),
#                                         label = cor_label, hjust = 0, vjust = 1)
#   
#   # Add sample size to the plot
#   scatterplot <- scatterplot + annotate("text", x = max(data_filtered[[x_var]]), y = min(data_filtered[[y_var]]),
#                                         label = paste("n =", nrow(data_filtered)), hjust = 1, vjust = 0)
#   
#   # Save the plot
#   plot_name <- paste0("plots/correlations/", x_var, "_vs_", y_var, "_", group, "_scatter.png")
#   ggsave(plot_name, plot = scatterplot, width = 10, height = 8, dpi = 300)
#   
#   # Return the plot object
#   return(scatterplot)
# }