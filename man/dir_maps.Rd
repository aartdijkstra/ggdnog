% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dir_maps.R
\name{dir_maps}
\alias{dir_maps}
\title{Find directory with map data}
\usage{
dir_maps()
}
\value{
The directory containing the shapefiles, or NA if not found.
}
\description{
This function scans the directory tree relative from the working directory to find a specific directory in Sharepoint.
Note that it searches for specific keywords unique to GGD NOG, so running it outside this environment will yield unexpected results.
}
\examples{
paste0(dir_maps(), "/PC4.shp")
}
