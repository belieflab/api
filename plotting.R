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
  
  # Perform paired t-test on pre- and post-intervention data
  t_test_data <- df %>%
    spread(visit, !!sym(outcome)) %>%
    select(`2`, `7`) %>%
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
             hjust = 1, vjust = 1, size = 3, fontface = "bold") +
    annotate("text", x = 0.5, y = max(max_mean * 1.2, max(df[[outcome]], na.rm = TRUE)), 
             label = condition_label, hjust = 0, vjust = 1, size = 5, fontface = "bold") +
    labs(y = paste("Mean", outcome)) +
    scale_fill_manual(values = c("Pre" = "skyblue", "Post" = "lightgreen")) +
    scale_x_discrete(limits = c("Pre", "Post")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x = element_blank()) +
    coord_cartesian(ylim = c(0, max(max_mean * 1.2, max(df[[outcome]], na.rm = TRUE))))
  
  return(plot)
}
