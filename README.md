# SDWorkshop
Supporting resources for System Dynamics Society (2023) and NATCOR (2024) workshops

There are five objectives of the initial workshop, which are to:

* Understand data frames and plot simulation data using ggplot2
* Analyse sensitivity runs (based on output from SD simulation tools) with dplyr
* Build a model using deSolve
* Run a sensitivity sweep with purrr, deSolve and dplyr
* Create interactive simulation app using Shiny and/or explore an SIR model with hospitalisations.


Objective five has been updated (for NATCOR) to include a more detailed exploration of an SIR and SIRH model.

The R code examples are in the directory [R code](https://github.com/JimDuggan/SDWorkshop/tree/main/R%20code) 

To run the code, it is recommended to setup an account on posit ([posit.cloud (formerly known as RStudio Cloud)](https://posit.cloud/)), and then use the option **New Project from Git Repository**, and enter https://github.com/JimDuggan/SDWorkshop.

Once the project is created, open the file `install_packages.R` in the `setup` folder, and click on the button called "Source", as this will run the file.

Any of the R files should then run ok.

For a detailed example of using R to explore system dynamics model for pandemic preparedness, see [Jair Andrade's GitHub repository](https://github.com/jandraor/preparedness), which contains all the examples in the 2024 paper published in the System Dynamics Review.

Jair Andrade, Berend Beishuizen, Mart Stein, Máire Connolly and Jim Duggan. [*Preparing for pandemic response in the context of limited resources.*](https://onlinelibrary.wiley.com/doi/10.1002/sdr.1775).


