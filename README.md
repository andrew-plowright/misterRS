# Mister Remote Sensing <img src="man/figures/logo.png" align="right" width ="100"/>

## Overview

A framework and suite of functions for processing and analyzing large, tiled remote sensing datasets.

This package is built around the `rsds` (Remote Sensing Dataset) object class, which links to raw tiled datasets in either raster or vector format, and the `tileScheme` object class (imported from the [TileManager](https://github.com/andrew-plowright/TileManager) library), which contains geometric information about the tileset such as tile extents and buffers.

The functions are built to allow efficient parallel processing of these tiled datasets. Current capabilities include:

- Generating surfaces (`surface_dem`, `surface_dsm`)
- Detecting individual trees from canopy height models (`detect_trees`)
- Segmenting images and surfaces (`segment_mss` and `segment_watershed`)
- Generating spectral, textural, and LAS metrics for segments
- Classifying segments

_**Developer's note:** This library is developed mainly for my personal workflow and so its structure is still very much in flux. As such, it's unlikely to be of immediate use to anyone else at this point, but please give me a shout if you find anything of interest. -- Andrew_
