packages <- c("devtools", "ggplot2", "dplyr") 
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
