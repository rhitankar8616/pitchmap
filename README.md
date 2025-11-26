git clone https://github.com/yourusername/pitchmap.git
cd pitchmap
# (if using Git LFS)
git lfs install
git lfs pull
# In R:
install.packages(c("shiny", "dplyr", "tidyr", "ggplot2", "readr", "scales", "gridExtra"))
shiny::runApp(".")
