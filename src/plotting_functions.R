### Collaborators (auto-populated):
###
### mkpappu
if (!require("ggplot2")) {install.packages("ggplot2")}; library(ggplot2)


# General plotting function with automatic labels and title
plot_means_auto <- function(data) {
  # Extract dataframe name without underscores
  data_name <- deparse(substitute(data))
  data_name <- gsub("_", " ", data_name)
  
  # Determine if "Pre" and "Post" are present in the dataframe name
  if (grepl("pre", tolower(data_name)) && grepl("post", tolower(data_name))) {
    condition <- "Pre vs Post"
  } else {
    condition <- "Condition"
  }
  
  # Extract phenotypes from the dataframe if possible
  phenotypes <- NULL
  if ("phenotype" %in% colnames(data)) {
    phenotypes <- unique(data$phenotype)
  }
  
  # Convert necessary columns to numeric if they are not already
  if (!is.numeric(data$value)) {
    data$value <- as.numeric(data$value)
  }
  
  # Remove rows with NA values in the value column
  data <- na.omit(data)
  
  # Calculate means
  mean_data <- aggregate(value ~ condition + phenotype, data = data, FUN = mean, na.rm = TRUE)
  
  # Define colors for fill variable if needed
  my_colors <- c("#1B9E77", "#D95F02")  # Example colors
  
  # Create the plot
  p <- ggplot(mean_data, aes(x = condition, y = value, fill = phenotype)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.8) +
    labs(x = "Condition", y = data_name, title = paste("Mean", data_name, "for Phenotypes")) +
    scale_fill_manual(values = my_colors, labels = phenotypes) +
    theme_minimal() +
    theme(legend.position = "top")  # Position the legend at the top
  
  # Generate a filename based on current timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("plot_", gsub(" ", "_", data_name), "_", timestamp, ".png")
  
  # Save the plot in "plots" directory
  plot_dir <- "plots"
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir)
    cat("Created directory:", plot_dir, "\n")
  }
  
  ggsave(file.path(plot_dir, filename), plot = p, width = 8, height = 6, dpi = 300)
  
  # Print filename for reference
  cat("Plot saved as:", filename, "\n")
}

# Example usage:
plot_means_auto(data = mean_correct_rejection_combined)




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

# plot scatter

# plot_model()
