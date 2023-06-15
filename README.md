# lit_review_final
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

# if all worked, there should be 4 pngs saved
