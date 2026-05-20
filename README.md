
# MycoTools

[![Build Status](https://travis-ci.org/ANYthingsDATA/MycoTools.svg?branch=master)](https://travis-ci.org/ANYthingsDATA/MycoTools)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/MycoTools)](https://cran.r-project.org/package=MycoTools)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/MycoTools)](https://cran.r-project.org/package=MycoTools)

## Overview

This package contains tools for processing and analyzing data from climate sensor and generating MycoIndex scores.

## Prerequisites
To run MycoTools you must first make sure you have the following software installed. 
- R  >= 4.3
  - https://cran.r-project.org/bin/windows/base/ 
- RStudio desktop 
  - https://posit.co/download/rstudio-desktop/
- Rtools must be installed
  - https://cran.r-project.org/bin/windows/Rtools/ 

### Installation of required packages
Once R and RStudio is up and running, you should install the following packages:
- devtools
- usethis

The commands below will check if the packages are already installed, and installs them if they are not installed.
```R
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
# Install usethis if not already installed
if (!requireNamespace("usethis", quietly = TRUE)) {
  install.packages("usethis")
}
```

## Connecting RStudio to Github
To install the package, you must create a Github.com user account and be granted access to the ANYthingsDATA/MycoTools repository. Once you have a github user and access to the repository
```R
# Create GitHub token
usethis::create_github_token()
```

- This will take you out of RStudio to Github for help in token creation.
- Specify the permissions given to R studio in the token name.
- Specify for how long the token is valid.
  - When your token expire, you will have to create a new token. This can be done as many times as you need. 
- Make sure the token is copied as it will never be seen again.
  - Save a copy of your token in a word or notepad document.
  - Example token: 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
- Your token will be used to access Github APIs and install Mycotools from the private Github repository. 

To add your newly created token to RStudio, run the following command and follow the instruction in the console window.
```R
# Set up GitHub token for package installation
gitcreds::gitcreds_set()
```

- If you have a token that now has expired, select "Replace these credentials" and add your newly created token.


## Installing/Updating the MycoTools package

To install the latest development version of MycoTools from GitHub, use the `devtools` package:
```R
# Install MycoTools package from GitHub
devtools::install_github("ANYthingsDATA/MycoTools", auth_token = gh::gh_token())
```

If your Github token is expired this installation will cause an error (401) or unauthorized access. If so, re-run the steps described in "Connecting RStudio to Github" to create a new token before installing/updating MycoTools.


## Usage

Provide examples and code snippets to demonstrate how to use your package. Include both basic and advanced usage examples.
```R
# Load the package
library(MycoTools)

# Example usage
data <- your_function(input_data)
```

## Documentation

Link to the complete package documentation or vignettes if available. Provide information on where users can find detailed documentation for your functions and classes.

## Contributing

If you want to contribute to the development of this package, please follow the guidelines in the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Issues

If you encounter any issues with the package or have suggestions for improvement, please open an issue on the [GitHub Issues page](https://github.com/yourusername/yourpackagename/issues).

## License

This package is proprietary software. Copyright © 2024–2026 ANYthings v/ Anders B. Nygaard and Mycoteam AS. All rights reserved. See the [LICENSE](LICENSE) file for details.

## Acknowledgments

If your package depends on other packages or resources, acknowledge and link to them here.

## Author

- Anders Nygaard — [ANYthings](https://anythings.no) (sole proprietorship)
- anders [at] anythings.no


## TODO:



sjekk import data
sjekk parsedate
sjekk complete date
vurder
combi mixmold/mixwood og mixtemp mold x temp og wodd x temp
