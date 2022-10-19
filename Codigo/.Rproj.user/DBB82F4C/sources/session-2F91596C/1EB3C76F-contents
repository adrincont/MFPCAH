# =============================================================================#
# [Libraries                                                                 ] #
# [Comments] -----------------------------------------------------------------.#
# ============================================================================.#
# {Libraries} =============================================================.####
rm(list = ls())
install_load = function(package1, ...)  {   
  # convert arguments to vector
  packages = c(package1, ...)
  # start loop to determine if each package is installed
  for (package in packages) {
    # if package is installed locally, load
    if (package %in% rownames(installed.packages()))
      do.call('library', list(package))
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  } 
}
list_lib = c(
  # Operaciones con datos funcionales
  "fda.usc",
  "fda",
  "MFPCA",
  "FactoMineR",
  # Graficas
  "ggplot2",
  "dplyr",
  "GGally",
  "patchwork",
  # Tratamiento de datos
  "R.matlab",
  "gridExtra",
  "readxl"
  )
# Load libraries and install if they do not exist
install_load(list_lib)
theme_set(theme_bw())
tab_lib = installed.packages()
tab_lib = tab_lib[rownames(tab_lib) %in% c("comoOdeCpp",list_lib),]
write.csv(tab_lib,file = "info_library.csv")
# {End of code} ===========================================================.####