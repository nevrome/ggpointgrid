# download/update dependencies: system("cd src && futhark pkg sync")
system("cd src && futhark c --library entry.fut")
#system("cd src && futhark multicore --library arrange.fut") # does not work on windows, unfortunately
devtools::document()
Rcpp::compileAttributes()
# build with ctrl+shift+b # devtools::install()
