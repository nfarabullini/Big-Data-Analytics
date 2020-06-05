# Big Data Analytics
Project Group 23: Corey Bothwell, Nicoletta Farabullini, Jacob Gelling, Andris Prokofjevs, Qasim Warraich <br>


## The Woof Factor in Zürich
--- 
Through use of data freely available from Stadt Zürich Open Data, we analyse dogs registered in Zürich and combine this data with other information available about the districts (kreis) that make up Zürich. We perform exploratory analysis using a number of visualisations and a model is developed in which we predict dog breed given related data.

### Notebook:
The included jupyter notebook is an interactive  way to view our entire data pipeline from data acquisition through EDA and Model generation to our final takeaways

### Configuration of Jupyter Notebook:

#### R Kernel <br>
This Jupyter notebook requires use of the R Kernel, which can be installed with a very simple 2 step installation process.

The detailed instructions can be found here: https://irkernel.github.io/installation/

#### Browser <br>
We **reccomend a chromium based browser** to view this notebook. We have discovered an **issue** rendering one of the visualisations **in firefox**.
One way to achieve this is to launch Jupyter notebook with a browser flag like for example `jupyter-notebook --browser=chrome` . Then the `localhost` url generated in the terminal may be copied into your chromium based browser and the notebook should launch. 

#### Libraries <br>
The required packages must first be installed. On Linux, this requires the system to have Curl (for communicating with the Yandex Translate API) and GDAL (for generating the map vizualisations) installed. Potential issues with GDAL and or RGDAL installation may stem from a lack of PROJ > 6.00 when using GDAL >=3.0.<br> We refer you to the CRAN page for more information: <br> 
https://cran.r-project.org/web/packages/rgdal/index.html

#

