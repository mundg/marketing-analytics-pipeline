
all_packages <- c('yaml', 'dplyr', 'tidyr', 'here', 'jsonlite', 'lubridate', 'hrbrthemes', 'scales')
install.packages(setdiff(all_packages, installed.packages()[, "Package"]))