## R package to use the Python environment in Rmarkdown which is equivalant to Jupyter notebook -- 

#library(reticulate)

# which python version do I have? python path?
Sys.which("python")
py_discover_config()
py_config()
py_available()
conda_list()

# install_miniconda()
# miniconda_path()


#virtualenv_remove("test")

# create a new environment 
conda_create("test")
# install SciPy
conda_install(envname = "r-reticulate", packages = c("scipy", "numpy",
                                             "pandas",
                                             "seaborn",
                                             "matplotlib") )

# conda_install(envname = "test", packages = c("re", "calendar")) --have trouble finding these packages

scipy <- import("scipy")
dt <- import("datetime")
pd <- import("pandas")
np <- import("numpy")
