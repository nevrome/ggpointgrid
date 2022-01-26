# download/update dependecies: futhark pkg sync
system("cd src && futhark c --library arrange.fut")
devtools::document()
Rcpp::compileAttributes()
devtools::install()
