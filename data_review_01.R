### INSTRUCTIONS -------------------------------------------------------------
# export the following collections from zotero: 
  # full text review - COVIDENCE as "fairness_review_zotero_YYYY-MM-DD.csv"
  # Snowball as "fairness_review_zotero_snowball_YYYY-MM-DD.csv"
# place them in the same directory as this script
# for the METHOD PANEL section to work: also place the following script in the same directory
  # "data_review_panels_METHOD.R"

### TABLE OF CONTENTS
  ## OVERALL PIE CHARTS (FOR ALL TAG CATEGORIES)
    # will produce pie graphs of all tags, sorted by category and save it as a png
  #
  ## PIPELINE STAGE PLOT
    # produces a separate single pie chart for the pipeline stage category (incl export of png)
  #
  ## PANEL PLOTS
    # reproduces the large plot assembly from above BUT for each pipeline stage
    # this is an overwhelming and messy plot but might provide some insight still
  #
  ## METHOD PANEL
    # simply calls a related script ("data_review_panels_METHOD.R")
    # this script produces pipeline stage pie charts per METHOD tag (overall, prevention, detection, mitigation)
    # it re-defines some of the previously used variables, so keeping it separate is easier
    # that script can also be run independantly


## SET UP ----------------------------------------------------------------------
# install.packages("pacman")
library("pacman")
# # install and load relevant packages
p_load(dplyr, stringr, ggplot2, tidyr, viridis, RColorBrewer, patchwork, wesanderson)

rm(list=ls())
today <- Sys.Date()

## DATA PREP --------------------------------------------------------------------

#import data
data <- read.csv(paste0("fairness_review_zotero_", Sys.Date(), ".csv"))
data <- rbind(data, read.csv(paste0("fairness_review_zotero_snowball_", Sys.Date(), ".csv")))


# category map does not need to be looped over
category_map <- c("COVIDENCE" = "source", "AD-HOC" = "source", "SNOWBALL" = "source", "WORD-OF-MOUTH" = "source",
                  
                  "EVIDENCE" = "type", "METHOD: DETECTION" = "type", "METHOD: MITIGATION" = "type", 
                  "METHOD: PREVENTION" = "type", "RECOMMENDATION" = "type", "OPPORTUNITY" = "type", 
                  "CONTEXT" = "type", "PERSPECTIVE" = "type", "APPLICATION" = "type",
                  
                  "PROBLEM SELECTION" = "pipeline stage", "DATA AVAILABILITY" = "pipeline stage",
                  "PREPROCESSING" = "pipeline stage", "EDA" = "pipeline stage", "MODELLING" = "pipeline stage",
                  "VALIDATION" = "pipeline stage", "AUDIT" = "pipeline stage", "KNOWLEDGE GENERATION" = "pipeline stage",
                  "INPROCESSING" = "pipeline stage", "POSTPROCESSING" = "pipeline stage",
                  
                  "PRS" = "use-case", "GxE" = "use-case", "PATHWAYS" = "use-case", "FINE-MAPPING" = "use-case", 
                  "DIAGNOSTICS: RARE DISEASE" = "use-case", "DIAGNOSTICS: CANCER" = "use-case", "DIAGNOSTICS: OTHER" = "use-case", 
                  "TREATMENT" = "use-case", "PHARMACOGENOMICS" = "use-case", "GWAS" = "use-case", 
                  "OTHER: NONE" = "use-case", "NEXT-GEN SEQUENCING" = "use-case", "REFERENCE PANEL" = "use-case",
                  "OMICS" = "use-case", "POPULATION HEALTH" = "use-case", "USE-CASE: OTHER" = "use-case",
                  "CANCER: NON-DIAGNOSTIC" = "use-case",
                  
                  "BRIEUC" = "reviewer", "LEANDRA" = "reviewer", "FABIAN" = "reviewer",
                  
                  "UNREAD" = "status", "SKIMMED" = "status", "DONE" = "status", "TIMEOUT-30" = "status", "PAYWALL" = "status", "REVISIT?" = "status",
                  
                  "KEY PAPER" = "relevance", "RELEVANT" = "relevance", "PERIPHERAL" = "relevance", "IRRELEVANT" = "relevance", "DUPLICATE" = "relevance"
)





  # Count occurrences of the individual tags (ignoring empty spaces before the strings)
  count_data <- data %>%
    separate_rows(Manual.Tags, sep = ";") %>%
    mutate(Manual.Tags = str_trim(Manual.Tags)) %>%
    count(Manual.Tags)
  #swap the cols for readability
  temp <- count_data$Manual.Tags
  count_data$Manual.Tags <- count_data$n
  count_data$n <- temp
  count_data <- rename(count_data, count = Manual.Tags, tags = n)
  
  # assign categories
  cat_tags <- count_data %>% mutate(category = category_map[tags])
  cat_tags$category <- replace_na(cat_tags$category, "other")
  count_data <- cat_tags # this produces an error I don't understand, but it still does what I want it to do
  
  # Order the Manual.Tags within each category (not sure that's needed)
  count_data <- count_data[order(count_data$category, count_data$tags), ]
  
  color_palette <- wes_palette("Moonrise3", n_distinct(count_data$tags), type = "continuous")
  names(color_palette) <- sample(unique(count_data$tags))
  
  
  
  ## OVERALL PIE CHARTS (FOR ALL TAG CATEGORIES) ----------------------------------
  
  
  # Create a pie chart for each category
  pie_charts2 <- lapply(unique(count_data$category), function(cat) {
    data <- count_data[count_data$category == cat, ]
    
    # Create custom legend labels with counts
    legend_labels <- paste(data$tags, " (", data$count, ")", sep = "")
    
    pie_chart <- ggplot(data, aes(x = "", y = count, fill = tags, pattern = tags)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = color_palette, labels = legend_labels) +
      labs(x = "", y = "", fill = cat, pattern = "tags", title = cat) +
      theme_minimal() +
      theme(legend.position = "left")
      # geom_text(aes(label = count), position = position_stack(vjust = 0.5)) # this would place the count into the pie chart

  })
  
  
  # Arrange pie charts and legends side by side
  combined_plot <- wrap_plots(pie_charts2, nrow = 3)
  # combined_plot <- combined_plot + plot_layout(guides = "collect", legend.position = "left")
  
  print(combined_plot)
  
  ## this plot is obviously very messy but can provide some added insight
  
  # save as png
  ggsave( paste0("all_cats_",  Sys.Date(),".png"), plot = combined_plot, width = 25, height = 20)
  
  
  ## PIPELINE STAGE PLOT -------------------------------------------------------
  pipe_data <- count_data[count_data$category == "pipeline stage", ]
  
  # Create custom legend labels with counts
  legend_labels <- paste(pipe_data$tags, " (", pipe_data$count, ")", sep = "")
  
  pipe_chart <- ggplot(pipe_data, aes(x = "", y = count, fill = tags, pattern = tags)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y", start = 0) +
    labs(x = "", y = "") +
    ggtitle("pipeline stage") +
    theme_minimal() +
    theme(legend.position = "right") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
      scale_fill_manual(
        values = color_palette,
        labels = legend_labels)


# pipe_chart
ggsave( paste0("all_cats_pipe",  Sys.Date(),".png"), plot = pipe_chart, width = 16, height = 16)



## PANEL PLOTS ------------------------------------------------------------------
# now we will reproduce the large plot assembly from above for each pipeline stage
# this is an overwhelming and messy plot but might provide some insight still

# initialise the panel plot with the non-split combined plot
panel_plot <- wrap_plots(pie_charts2, nrow = 1)  + plot_annotation("Overall")

for (i in count_data$tags[count_data$category == "pipeline stage"]) {
  # can't just repeat the above code bc it will just count all tags rather than the overlap
  # create subcount data corresponding to the pipeline stage of the iteration
  # select rows in which Manual.Tags includes i
  sub_data <- filter(data, grepl(i , Manual.Tags))
  
  # run the above code over just that
  
  # Count occurrences of the individual tags (ignoring empty spaces before the strings)
  # data <- sub_data
  sub_count_data <- sub_data %>%
    separate_rows(Manual.Tags, sep = ";") %>%
    mutate(Manual.Tags = str_trim(Manual.Tags)) %>%
    count(Manual.Tags)
  #swap the cols for readability
  temp <- sub_count_data$Manual.Tags
  sub_count_data$Manual.Tags <- sub_count_data$n
  sub_count_data$n <- temp
  sub_count_data <- rename(sub_count_data, count = Manual.Tags, tags = n)
  
  cat_tags <- sub_count_data %>% mutate(category = category_map[tags])
  
  # check whether all tags have been categorise
  # if not, ammend the above code with the correct categorisation
  # Print rows containing NA
  na_rows <- cat_tags[!complete.cases(cat_tags), ]
  cat_tags$category <- replace_na(cat_tags$category, "other")
  
  # count_data_no_cats <- count_data
  sub_count_data <- cat_tags # this produces an error I don't understand, but it still does what I want it to do
  
  # Order the Manual.Tags within each category (not sure that's needed)
  sub_count_data <- sub_count_data[order(sub_count_data$category, sub_count_data$tags), ]
  
  
  
  
  ### PLOTS --------------------------------------------------------------------
  
  # color_palette <- wes_palette("Moonrise3", n_distinct(sub_count_data$tags), type = "continuous")
  # names(color_palette) <- sample(unique(sub_count_data$tags))
  # 
  
  
  # Create a pie chart for each category
  pie_charts2 <- lapply(unique(sub_count_data$category), function(cat) {
    sub_data <- sub_count_data[sub_count_data$category == cat, ]
    
    # Create custom legend labels with counts
    legend_labels <- paste(sub_data$tags, " (", sub_data$count, ")", sep = "")
    
    pie_chart <- ggplot(sub_data, aes(x = "", y = count, fill = tags, pattern = tags)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = color_palette, labels = legend_labels) +
      labs(x = "", y = "", fill = cat, pattern = "tags", title = paste0(i, "_", cat)) +
      theme_minimal() +
      geom_text(aes(label = count), position = position_stack(vjust = 0.5)) + # adding the count in the pie
      # theme(legend.position = "left")
      theme(legend.position = "none") # removing the legend so the panel_plot won't be overwhelmed
    
    
  })
  
  
  # Arrange pie charts, adding main title (doesn't work)
  combined_plot <- wrap_plots(pie_charts2, nrow = 1) + plot_annotation(i)
  
  
  # # save as png
  # ggsave( paste0("all_cats_",  i, "_", Sys.Date(),".png"), plot = combined_plot, width = 25, height = 25)
  
  panel_plot <- panel_plot / combined_plot  + plot_layout(guides = "collect")
  
  
  # save for this i specifically
  assign(paste0("sub_data_", i), sub_data)
  assign(paste0("count_data_", i), sub_count_data)
  assign(paste0("combined_plot_", i), combined_plot)
  
}

# print(panel_plot)
ggsave( paste0("panel_all_cats_",  Sys.Date(),".png"), plot = panel_plot, width = 25, height = 30)




## METHOD PANEL ---------------------------------------------------------------
# this script produces pipeline stage pie charts per METHOD tag (overall, prevention, detection, mitigation)
# it re-defines some of the previously used variables, so keeping it separate is easier
# it can also be run independantly

source("data_review_panels_METHOD.R")
