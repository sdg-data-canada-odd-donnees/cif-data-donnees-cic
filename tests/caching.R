

dependencies <- c("dplyr", "cansim", "dotenv")
print(dependencies)

installed_packages <- installed.packages()[,"Package"]
print(installed_packages)

new_packages <- dependencies[!(dependencies %in% installed_packages)]
print(new_packages)

if (length(new_packages)) install.packages(new_packages)
