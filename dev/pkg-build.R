####################################################################################################
# pkg-build.R
# Using the devtools ecosystem to run the different stages of the package build cycle
#
####################################################################################################
# .rs.restartR() 
library(devtools)
library(usethis)
library(testthat)
library(checkmate)
library(covr)
rm(list = ls()); gc()

#----- (Optional) Different ways to clean if we're worried about conflicts
# devtools::unload() # Unload from R session (usually sufficient if we're having problems)
# devtools::uninstall(unload = T) # Combines unloading and removal (dev-friendly version of remove.packages)
# remove.packages("rfg")

#----- Development cycle
# usethis::use_version("dev")
# usethis::use_version("patch")
# usethis::use_version("minor")
# usethis::use_version("major")

devtools::load_all() # Load the current version
devtools::document() # Update documentation via Roxygen
devtools::test()     # Run testthat tests

# pkg_rep <- covr::package_coverage(type = "all")
# covr::report(pkg_rep)
covr::package_coverage() # Testing coverage report
covr::report() # More detailed testing coverage report

# #----- Build cycle
# roxygen2::roxygenise(clean = TRUE)
# devtools::check() # Catch errors before build
# devtools::build()
# devtools::install() # If we want to test an actual installation

