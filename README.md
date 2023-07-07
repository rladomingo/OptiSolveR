# OptiSolveR
OptiSolveR is a collection of generic solvers for quadratic spline interpolation and the simplex method. The tool's implementation of the simplex method is designed to assist with optimization problems such as minimizing shipping costs.

## Installation
Follow these steps to install and run the OptiSolveR:
1. Install R. If you haven't already, download and install R from the [R Project website](https://www.r-project.org/).
2. Install the required packages. Open R or RStudio and run the following command to install the necessary packages:

   ```R
   install.packages(c("shiny", "shinythemes", "shinyMatrix"))
   ```
3. Clone the repository or download it as a ZIP file and extract it to your desired location.
    ```
    git clone https://github.com/rladomingo/OptiSolveR.git
    ```
4. Run OptiSolveR. Open R or RStudio and set the working directory to the location where you cloned or extracted the repository. Then, run the following command to launch the app:
    ```R
   shiny::runApp()
   ```
5. Once the app is running, you will see a URL displayed in the R console (e.g., Listening on https​://127.0.0.1:xxxx). Open a web browser and enter the URL to use OptiSolveR.

## Usage
For detailed instructions on how to use OptiSolveR and explore its features, please refer to the [User Manual](./user-manual.pdf) located in the root directory of this project. The user manual provides comprehensive documentation, including step-by-step guides, examples, and explanations of each functionality.
