# groundcontrol: R package

Roll your own on-time flight dataset, like [nycflights13](https://github.com/hadley/nycflights13), but using the airports and years of your choosing.


## Install

The **groundcontrol** package is not on CRAN, so intall it using **devtools**,
```r
devtools::install_github("jrnold/groundcontrol")
```


## Usage

This package only has two functions, `create_flights` which creates an R data-only package,
and `download_flightdata` which only downloads the data without creating a package.
For example, the following creates an R package named **seaflights15**
with all flights departing or arriving at SEA (Seattle-Tacoma airport) in 2015,
```
groundcontrol::create_flights("seaflights15", "SEA", 2015,
                              origin = TRUE, dest = TRUE)
```
