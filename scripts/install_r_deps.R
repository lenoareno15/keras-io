pkgs <- c(
  "shiny", "shinydashboard", "dplyr", "ggplot2", "plotly", "tidyr", "DT",
  "janitor", "stringr", "scales", "rmarkdown", "broom", "caret", "shinycssloaders",
  "shinyjs", "shinyWidgets", "thematic", "ranger", "tinytex", "rlang"
)

installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)

if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

message("All packages installed or already present.")