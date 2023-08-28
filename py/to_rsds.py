from qgis.PyQt.QtCore import QCoreApplication
from os import path
from qgis.core import (QgsProcessing,
                       QgsProcessingAlgorithm,
                       QgsProcessingException,
                       QgsProcessingOutputNumber,
                       QgsProcessingParameterDistance,
                       QgsProcessingParameterFeatureSource,
                       QgsProcessingParameterRasterLayer,
                       QgsProcessingParameterFolderDestination,
                       QgsProcessingParameterString,
                       QgsProcessingParameterCrs,
                       QgsProcessingFeedback)
from qgis import processing


class ToRSDS(QgsProcessingAlgorithm):

    def tr(self, string):
        # Returns a translatable string with the self.tr() function.
        return QCoreApplication.translate('Processing', string)

    def createInstance(self):
        # Must return a new copy of your algorithm.
        return ToRSDS()

    def name(self):
        # Returns the unique algorithm name.
        return 'to_rsds'

    def displayName(self):
        # Returns the translated algorithm name.
        return self.tr('Convert raster to Remote Sensing Dataset (RSDS)')

    def group(self):
        # Returns the name of the group this algorithm belongs to.
        return self.tr('misterRS')

    def groupId(self):
        # Returns the unique ID of the group this algorithm belongs to.
        return 'misterRS'

    def shortHelpString(self):
        # Returns a localised short help string for the algorithm.
        return self.tr('Convert raster to Remote Sensing Dataset (RSDS)')

    def initAlgorithm(self, config=None):
        
        # PARAMETERS
        self.addParameter(
            QgsProcessingParameterFeatureSource(
                'INPUT_TILES',
                self.tr('Input vector layer'),
                defaultValue = 'tiles_main',
                types=[QgsProcessing.TypeVectorPolygon]
            )
        )
        self.addParameter(
            QgsProcessingParameterRasterLayer(
                'INPUT_RASTER',
                self.tr('Input raster layer')
            )
        )
        self.addParameter(
            QgsProcessingParameterDistance(
                'CELLSIZE',
                self.tr('Cell Size'),
                defaultValue = 0.25,
                parentParameterName='INPUT'
            )
        )
        self.addParameter(
            QgsProcessingParameterFolderDestination(
                'OUTPUT_FOLDER',
                self.tr('Output folder')
            )
        )
        self.addParameter(
            QgsProcessingParameterCrs(
                'CRS',
                self.tr('Coordinate Reference System'),
                optional=True
            )
        )
        self.addParameter(
            QgsProcessingParameterString(
                'SELECTED_TILE',
                self.tr('Selected Tile'),
                optional=True
            )
        )
        
        # OUTPUT
        self.addOutput(
            QgsProcessingOutputNumber(
                'NUMBEROFFEATURES',
                self.tr('Number of features processed')
            )
        )

    def processAlgorithm(self, parameters, context, feedback):

        # Get parameters
        in_tiles = self.parameterAsSource(parameters,'INPUT_TILES',context)
        in_raster = self.parameterAsRasterLayer(parameters,'INPUT_RASTER',context)
        rastercellsize = self.parameterAsDouble(parameters, 'CELLSIZE',context)
        out_folder = self.parameterAsFile(parameters, 'OUTPUT_FOLDER',context)
        crs = self.parameterAsCrs(parameters, 'CRS', context)
        if not crs.isValid():
            crs = None
        selected_tile = self.parameterAsString(parameters, 'SELECTED_TILE', context)
        
        # Counter for tiles processed
        processed_tiles = 0 
        
        features = in_tiles.getFeatures()
        for feature in features:

            # Tile type and name
            type = feature['type']
            tile_name = feature['tileName']
            
            # Check if this tile should be processed
            process_this = selected_tile == '' or selected_tile == tile_name
            
            if type == 'buff' and process_this:
                
                feedback.pushInfo(self.tr(tile_name))
                
                # Output file
                out_file = out_folder + "/" + tile_name + ".tif"
            
                if not path.isfile(out_file):
                
                    # Get bounding box
                    bbox = feature.geometry().boundingBox()
                    
                    # Process
                    processing.run("gdal:warpreproject", 
                    {'INPUT':in_raster , 
                    'SOURCE_CRS':None,
                    'TARGET_CRS':crs,
                    'RESAMPLING':2,
                    'NODATA':None,
                    'TARGET_RESOLUTION': rastercellsize,
                    'DATA_TYPE': None,
                    'OPTIONS':'COMPRESS=LZW',
                    'TARGET_EXTENT':bbox,
                    'TARGET_EXTENT_CRS':crs,
                    'MULTITHREADING':False,
                    'EXTRA':'',
                    'OUTPUT': out_file}
                    )
                
                    processed_tiles = processed_tiles + 1
                   
        return {'TILESPROCESSED': processed_tiles}