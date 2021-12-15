<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis styleCategories="AllStyleCategories" maxScale="0" minScale="1e+08" version="3.16.2-Hannover" hasScaleBasedVisibilityFlag="0">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
  </flags>
  <temporal mode="0" fetchMode="0" enabled="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <property key="WMSBackgroundLayer" value="false"/>
    <property key="WMSPublishDataSourceUrl" value="false"/>
    <property key="embeddedWidgets/count" value="0"/>
    <property key="identify/format" value="Value"/>
  </customproperties>
  <pipe>
    <provider>
      <resampling zoomedInResamplingMethod="nearestNeighbour" enabled="false" zoomedOutResamplingMethod="nearestNeighbour" maxOversampling="2"/>
    </provider>
    <rasterrenderer nodataColor="" alphaBand="-1" band="1" opacity="0.5" type="paletted">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <colorPalette>
        <paletteEntry label="C - Coniferous" color="#0ecbdc" alpha="255" value="1"/>
        <paletteEntry label="R - Hedgerow" color="#0019fb" alpha="255" value="2"/>
        <paletteEntry label="D - Deciduous" color="#01ff16" alpha="255" value="3"/>
        <paletteEntry label="H - Cherry" color="#de12bc" alpha="255" value="4"/>
        <paletteEntry label="B - Building" color="#ff0501" alpha="255" value="5"/>
        <paletteEntry label="TM - Temporal mismatch" color="#ffc700" alpha="255" value="6"/>
        <paletteEntry label="E - Dead" color="#000000" alpha="255" value="7"/>
      </colorPalette>
      <colorramp name="[source]" type="randomcolors"/>
    </rasterrenderer>
    <brightnesscontrast brightness="0" gamma="1" contrast="0"/>
    <huesaturation colorizeGreen="128" colorizeRed="255" grayscaleMode="0" colorizeBlue="128" saturation="0" colorizeOn="0" colorizeStrength="100"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>8</blendMode>
</qgis>
