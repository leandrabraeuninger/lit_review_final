### INSTRUCTIONS -------------------------------------------------------------
# export the following collections from zotero: 
# full text review - COVIDENCE as "fairness_review_zotero_YYYY-MM-DD.csv"
# Snowball as "fairness_review_zotero_snowball_YYYY-MM-DD.csv"
# place them in the same directory as this script
# for full graphs, also run the script "data_review_01.R"

# install.packages("pacman")
library("pacman")
# # install and load relevant packages
p_load(dplyr, stringr, ggplot2, tidyr, viridis, RColorBrewer, patchwork, wesanderson)

rm(list=ls())
today <- Sys.Date()

#import data
data <- read.csv(paste0("fairness_review_zotero_", Sys.Date(), ".csv"))
data <- rbind(data, read.csv(paste0("fairness_review_zotero_snowball_", Sys.Date(), ".csv")))


## filtering to only include methods papers
# # is done in the loop below
# data <- data %>%
#   filter(grepl("METHOD: MITIGATION", Manual.Tags))


# category map does not need to be looped over
category_map <- c("COVIDENCE" = "source", "AD-HOC" = "source", "SNOWBALL" = "source", "WORD-OF-MOUTH" = "source",
                  
                  "EVIDENCE" = "type", "METHOD: DETECTION" = "type", "METHOD: MITIGATION" = "type", 
                  "METHOD: PREVENTION" = "type", "RECOMMENDATION" = "type", "OPPORTUNITY" = "type", 
                  "CONTEXT" = "type", "PERSPECTIVE" = "type", "APPLICATION" = "type",
                  
                  "PROBLEM SELECTION" = "pipeline stage", "DATA AVAILABILITY" = "pipeline stage",
                  "PREPROCESSING" = "pipeline stage", "EDA" = "pipeline stage", "MODELLING" = "pipeline stage",
                  "VALIDATION" = "pipeline stage", "AUDIT" = "pipeline stage", "KNOWLEDGE GENERATION" = "pipeline stage",
                  # "INPROCESSING" = "pipeline stage", "POSTPROCESSING" = "pipeline stage",
                  
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

# create the colour palette 
stages <- c(
  "PROBLEM SELECTION", "DATA AVAILABILITY",
  "PREPROCESSING", "EDA", "MODELLING",
  "VALIDATION" , "AUDIT" , "KNOWLEDGE GENERATION"
  # "INPROCESSING" , "POSTPROCESSING"
)

color_palette <- wes_palette("Moonrise3", length(stages), type = "continuous")
names(color_palette) <- stages

a <- c("METHOD", "METHOD: MITIGATION", "METHOD: DETECTION", "METHOD: PREVENTION")

for (i in a){
  ## filtering to only include methods papers
  method_data <- data %>%
    filter(grepl(i, Manual.Tags))
  
  # Count occurrences of the individual tags (ignoring empty spaces before the strings)
  count_data <- method_data %>%
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
  

  ## PIPELINE STAGE PLOT
  pipe_data <- count_data[count_data$category == "pipeline stage", ]
                            # count_data$tags != "INPROCESSING" &
                            # count_data$tags != "POSTPROCESSING", ]
  
  pipe_chart <- ggplot(pipe_data, aes(x = "", y = count, fill = tags, pattern = tags)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y", start = 0) +
    labs(x = "", y = "") +
    ggtitle(i) +
    theme_minimal() +
    theme(legend.position = "right") +
    geom_text(aes(label = count), position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = color_palette)
    
  # Create custom legend labels with counts
  # legend_labels <- paste(pipe_data$tags, " (", pipe_data$count, ")", sep = "")

  # # Modify the legend using scale_fill_manual() to add the nice colours (ensures the tags have the same colours across all plots)
  # pipe_chart <- pipe_chart +
  #   scale_fill_manual(
  #     values = color_palette
  #     # labels = legend_labels,
  #     # guide = guide_legend(
  #     #   label.position = "right",
  #     #   label.hjust = 0,
  #     #   title.position = "top"
  #     # )
  #   )
  
  # save  with correct name
  assign(paste0("pipe_", i), pipe_chart)
}

# panel_pipe <- pipe_METHOD +
#   (`pipe_METHOD: PREVENTION` /
#   `pipe_METHOD: DETECTION` /
#   `pipe_METHOD: MITIGATION`)

panel_pipe <- pipe_METHOD +
  (`pipe_METHOD: PREVENTION` + theme(legend.position="none")) +
     (`pipe_METHOD: DETECTION` + theme(legend.position="none"))  +
     (`pipe_METHOD: MITIGATION` + theme(legend.position="none")) 

# panel_pipe
ggsave( paste0("panel_pipe",  Sys.Date(),".png"), plot = panel_pipe, width = 8, height = 8)
