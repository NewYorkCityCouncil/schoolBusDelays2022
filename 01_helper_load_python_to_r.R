## R package to use the Python environment in Rmarkdown which is equivalant to Jupyter notebook -- 

library(reticulate)

# which python version do I have? python path?
Sys.which("python")
py_discover_config()
py_config()
py_available()
conda_list()

install_miniconda()
miniconda_path()

## "/Users/romartinez/Library/r-miniconda/envs/r-reticulate/bin/python"
## 3.6.15

# create environment
virtualenv_create("test")
virtualenv_list()
use_virtualenv("test")

#virtualenv_remove("test")

# install python packages
virtualenv_install(envname = "test", ignore_installed = FALSE,
                   packages = c("requests",
                                "pandas",
                                "plotly",
                                "datetime",
                                "siuba",
                                "kaleido",
                                "folium",
                                "Psycopg") )
