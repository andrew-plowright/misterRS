% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pseudo_img.R
\name{pseudo_img}
\alias{pseudo_img}
\title{Pseudo image}
\usage{
pseudo_img(inputs, out_rts, ...)
}
\description{
Create a 3-band pseudo image using bands from several different images or surfaces. These can (and should) be rescaled to an 8-bit image (values from 0 to 255)
}
\details{
The \code{inputs} argument should be a list with up to three elements, each of which correspond to a band in the output image.
Each element should be in a named list, for which the elements are:
\describe{
\item{\code{rts}}{Raster tileset}
\item{\code{band}}{The band number from the RS Dataset}
\item{\code{range}}{A numeric vector with two elements indicating the minimum and maximum range of the input RS Dataset band that will be rescaled}
}
}
