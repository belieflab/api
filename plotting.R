# if (!require("ggplot2")) {install.packages("ggplot2")}
# 
# create_bar_plot <- function(stats, data, measure) {
#   measure_sym <- rlang::sym(measure)
#   
#   # Extract the dataframe name without underscores
#   data_name <- deparse(substitute(data))
#   data_name <- gsub("_", " ", data_name)
#   
#   # Determine if "Pre" and "Post" are present in the dataframe name
#   if (grepl("pre", tolower(data_name)) && grepl("post", tolower(data_name))) {
#     condition <- "Pre vs Post"
#   } else {
#     condition <- "Condition"
#   }
#   
#   # Extract phenotypes from the dataframe if possible
#   phenotypes <- NULL
#   if ("phenotype" %in% colnames(data)) {
#     phenotypes <- unique(data$phenotype)
#   }
#   
#   # Create the plot
#   plot <- ggplot(data = stats, aes(x = visit, y = mean, fill = visit)) +
#     geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
#     geom_errorbar(aes(x = visit, ymin = mean - se, ymax = mean + se),
#                   width = 0.2, colour = "black", alpha = 0.9, size = 1, position = position_dodge(0.5)) +
#     geom_point(data = data, aes(x = visit, y = !!measure_sym), alpha = 0.3, position = position_jitter()) +
#     labs(x = condition, y = data_name, title = paste("Mean", data_name, "for Visits")) +
#     scale_x_discrete(labels = c('pre', 'post')) +
#     scale_fill_manual(name = "", values = c("#32CD32", "#9370DB")) +
#     theme_minimal() +
#     theme(legend.position = "top")
#   
#   # Generate a filename based on the current timestamp
#   timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
#   filename <- paste0("plot_", gsub(" ", "_", data_name), "_", timestamp, ".png")
#   
#   # Save the plot in the "plots" directory
#   plot_dir <- "plots"
#   if (!dir.exists(plot_dir)) {
#     dir.create(plot_dir)
#     cat("Created directory:", plot_dir, "\n")
#   }
#   
#   ggsave(file.path(plot_dir, filename), plot = plot, width = 8, height = 6, dpi = 300)
#   
#   # Print filename for reference
#   cat("Plot saved as:", filename, "\n")
#   
#   return(plot)
# }
# 
# # Example usage:
# lshsrScore <- create_bar_plot(lshsrStats, lshsr_compact, "lshsr_total")
# print(lshsrScore)

createBarPlot <- function(stats, data, measure) {
  measure_sym <- rlang::sym(measure)
  
  plot <- ggplot(data = stats, aes(x = visit, y = mean, fill = visit)) +
    geom_bar(stat = "identity", width = 0.5) +
    geom_errorbar(aes(x = visit, ymin = mean - se, ymax = mean + se),
                  width = 0.2, colour = "black", alpha = 0.9, size = 1, position = position_dodge(0.5)) +
    geom_point(data = data, aes(x = visit, y = !!measure_sym), alpha = 0.3, position = position_jitter()) +
    labs(x = "Music", y = "Hallucinations") +
    scale_x_discrete(labels = c('pre', 'post')) +
    scale_fill_manual(name = "", values = c("#32CD32", "#9370DB"))
  
  return(plot)
}

# Example usage:
# lshsrScore <- createBarPlot(lshsrStats, lshsr_compact, "lshsr_total")
# print(lshsrScore)


plot_mean <- function(data, title) {
  # Load ggplot2
  if (!require("ggplot2")) {install.packages("ggplot2")}; library(ggplot2)
  # Create the directory in which mean plots will be saved if it doesn't exist
  if (!dir.exists("figures/means")) {
    dir.create("figures/means", recursive = TRUE)
  }
  
  # Plotting code
  p <- ggplot(data, aes(x = condition, y = mean_value, fill = phenotype)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
                  position = position_dodge(width = 0.7), width = 0.2, size = 0.7) +
    labs(title = title, x = "Condition", y = "Score") +
    theme_minimal() +
    theme(legend.position = "top")
  
  # Save plot to "figures/means" directory
  plot_path <- file.path("figures/means", paste0(tolower(gsub(" ", "_", title)), ".png"))
  ggsave(filename = plot_path, plot = p)
  
  return(p)
}


# Example usage
plot_mean(hits_combined, "Mean Hits Pre vs Post for Phenotypes HC and SZ")
