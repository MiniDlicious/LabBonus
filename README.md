# Project Status
[![Build Status](https://travis-ci.org/MiniDlicious/LabBonus.svg?branch=master)](https://travis-ci.org/MiniDlicious/LabBonus)

# Lab Bonus: Another R Package

Machine learning 

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

The package require a basic installation of R. In addition devtools and testthat packages.

```
install.packages("devtools")
install.packages("testthat")
```

### Installing

Run the following in R:

```
devtools::install_github("MiniDlicious/LabBonus", subdir="awesomebonus", build_vignette=TRUE)
```

## Running the tests

```
devtools::test()
```

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

## Built With

* [Travis CI](https://travis-ci.org)


## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Friends and family for making this possible.