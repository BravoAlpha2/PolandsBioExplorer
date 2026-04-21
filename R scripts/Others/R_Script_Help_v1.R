# ============================================================
# Author: Nuno Garcia
# Date: 20/04/2026
# LinkedIn: https://www.linkedin.com/in/nuno-garcia-97b780158/
# ORCID: https://orcid.org/0000-0001-7917-3286
#
# Script purpose:
# Read large biodiversity CSV files efficiently and convert them to
# .rds format for faster loading in a Shiny dashboard focused on
# biodiversity observations in Poland.
# ============================================================


# ============================================================
# 1) PACKAGE required
# ============================================================

library(data.table)


# ============================================================
# 2) INPUT FILES
# ============================================================

path_occ <- "C:/Users/nunog/OneDrive/Desktop/Shiny - Appsilon Interview/Data/biodiversity-data/occurence.csv"
path_mult <- "C:/Users/nunog/OneDrive/Desktop/Shiny - Appsilon Interview/Data/biodiversity-data/multimedia.csv"

# ============================================================
# 3) READ CSV FILES (occurences and multimedia)
# ============================================================

occ <- fread(path_occ)
mult <- fread(path_mult)

# ============================================================
# 4) SAVE AS RDS
# ============================================================

# Save occurences in .rds format
saveRDS(occ,"C:/Users/nunog/OneDrive/Desktop/Shiny - Appsilon Interview/Data/biodiversity-data/occurrence.rds")

# Save multimedia in .rds format
saveRDS(mult,"C:/Users/nunog/OneDrive/Desktop/Shiny - Appsilon Interview/Data/biodiversity-data/multimedia.rds")

# ============================================================
# 5) MESSAGE (It´s optinional but I like to put)
# ============================================================

cat("Conversion completed successfully: CSV files were read and saved as RDS files.\n")