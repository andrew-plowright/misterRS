import argparse
import qgis.core as qc
from gdal import PushErrorHandler
from PyQt5.QtGui import QColor, QPainter
from argfunc import *
import sys, os

parser = argparse.ArgumentParser()
parser.add_argument('-qgs_file',    type=niceStr)
parser.add_argument('-RSDS_folder', type=niceStr)
parser.add_argument('-file_ext',    type=niceStr)
parser.add_argument('-group_name',  type=niceStr)
parser.add_argument('-style_file',  type=niceStr)

a = parser.parse_args()

from argparse import Namespace

a = Namespace(
    qgs_file = "D:/Lb/Maps/Lb_2019.qgz",
    RSDS_folder = "D:/Lb/DerivedData/CHM_2011",
    file_ext = "tif",
    group_name = "CHM 2011",
    style_file = "D:/Lb/Maps/QGIS_styles/CHM.qml"

)


# Get list of files
in_files = [f for f in os.listdir(a.RSDS_folder) if f.endswith(a.file_ext)]

if len(in_files) > 0:

    # Get full file paths
    in_paths = [os.path.join(a.RSDS_folder, f) for f in in_files]

    # Get names of layers
    in_names = [os.path.splitext(f)[0] for f in in_files]

    # Launch QGIS application
    qgs = qc.QgsApplication([], True)
    qgs.initQgis()

    # WARNING: This is to silence the ol 'Sum of Photometric type-related... (etc)' error
    # Should be included with caution, since it can silence other errors
    PushErrorHandler('CPLQuietErrorHandler')

    # Create new project
    project = qc.QgsProject()

    # Read map
    project.read(a.qgs_file)

    # To access layers and groups at root
    root = project.layerTreeRoot()

    # Get group (add if needed)
    group = root.findGroup(a.group_name)
    if group is None:
        group = root.addGroup(a.group_name)


    # Add layer and groups
    for i in range(len(in_names)):

        # Get layer source, name, type and parent name
        lpath = in_paths[i]
        lname = in_names[i]

        # Try and get layer if it already exists
        lyr = None
        for treelyr in group.findLayers():
            if treelyr.name() == lname:
                lyr = treelyr.layer()

        # Add layer if it doesn't already exist
        if lyr is None:

            lyr = qc.QgsRasterLayer(lpath, lname)
            project.addMapLayer(lyr, False)
            group.addLayer(lyr)

        # Symbolize layer
        lyr.loadNamedStyle(a.style_file)

    # Save map
    success = project.write(a.qgs_file)

else:
    sys.stdout.write("ERROR: No files found")