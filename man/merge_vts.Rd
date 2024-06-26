% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge.R
\name{merge_vts}
\alias{merge_vts}
\title{Merge multiple VTS into a single one according to zones}
\usage{
merge_vts(in_vts_list, out_vts, zones, zone_field, ...)
}
\arguments{
\item{in_vts_list}{named list of vts. The names should correspond to either 1) "\if{html}{\out{<none>}}" or 2) values in the \code{zone_field} of the \code{zones}}

\item{zones}{a sf class MultiPolygon (or a file path). Should contain an attribute named \code{zone_field}}

\item{zone_field}{character. Name of attribute which identifies the name of each zone}
}
\description{
The geographical bounds of zones are defined in \code{zone}. The name of each zone is identified in the \code{zone_field}. The contents of the
\code{in_vts_list} are merged according to their intersection with the zones. If a vts is identified as "\if{html}{\out{<none>}}", it will be selected where
no zones are present.
}
