# dependencies.R
required_packages <- c("shiny", "tidyverse", "tidytext", "DT", "wordcloud", "RColorBrewer")

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Load the packages
invisible(lapply(required_packages, library, character.only = TRUE))
