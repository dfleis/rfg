####################################################################################################
# pkg-init-config.R
# Initial R package setup
#
####################################################################################################
library(devtools)
library(usethis)
library(testthat)
# library(desc)

#--------------------------------------------------
#----- INITIAL CONFIG, FILE INITIALIZATION
#--------------------------------------------------
#----- Create package project skeleton
# usethis::create_package("../rfg")

#----- Add entries to .Rbuildignore
# Add files to .Rbuildignore
# usethis::use_build_ignore(c("LICENSE.md", ".Rhistory", ".Rproj.user"))
# To add an entire directory and all its contents (recursively) to .Rbuildignore
# include the line ^path/to/dir/ (or possibly ^path/to/dir/.*$ might be safer?).
# usethis::edit_r_buildignore() # add lines ^.*\.Rproj$ ^ignore/ and ^dev/
usethis::use_build_ignore("man-roxygen")

#----- Create skeleton {packageName}-package.R file
# usethis::use_package_doc()

#----- Create DESCRIPTION file
per1 <- utils::person("David", "Fleischer", email = "david.p.fleischer@gmail.com", role = c("cre", "aut"))
auth <- c(per1)
maint <- per1

usethis::use_description(fields = list(
  Title = "Random Function Generator",
  Description = "Implements the random function generator design used by Friedman (2001) 'Greedy function approximation: A gradient boosting machine'. Generates complex multivariate functions useful for synthetic evaluations of models in high-dimensional spaces.",
  `Authors@R` = c(per1),
  `Maintainer@R` = per1
))

usethis::use_mit_license()

#----- Setup testthat file structure
# usethis::use_testthat()

#----- Set some package dependencies
usethis::use_import_from("stats", c("rnorm", "rexp", "runif"))
# usethis::use_import_from("rlang", "is_integerish")
# usethis::use_package("httr2", type = "Imports")
# usethis::use_import_from("httr2", fun = "%>%")
# usethis::use_package("R6", type = "Imports")
# usethis::use_package("rlang", type = "Imports")
# usethis::use_package("checkmate", type = "Imports")
# usethis::use_import_from("utils", fun = "modifyList")

# rlang_funs <- c("abort", "caller_arg", "caller_env")
# usethis::use_import_from("rlang", fun = rlang_funs)









