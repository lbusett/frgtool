# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# FRG_Burnt_Areas_Processing.py
# Created on: 2014-01-16 11:32:33.00000
#   (generated by ArcGIS/ModelBuilder)
# Description:
# ---------------------------------------------------------------------------

def main(Burned_Areas_Full_shp, Intermediate_Folder):

    # Define useful variables
    Temp_Out_Folder = Intermediate_Folder+'\\Burnt_Area_Overlap_Analysis\\'
    Burned_Areas_Intersects_shp =Temp_Out_Folder+"Burnt_Areas_Intersects.shp"                   # Accessory shape - contains all overlapping BAs, replicated
    Burned_Areas_Single_shp = Intermediate_Folder+'\\'+os.path.splitext(os.path.basename(Burned_Areas_Full_shp))[0]+'_Single_Fires.shp'          # Will contain shapefile of areas burned only once
    Burned_Areas_Multiple_shp = Intermediate_Folder+'\\'+os.path.splitext(os.path.basename(Burned_Areas_Full_shp))[0]+'_Multiple_Fires.shp'       # Will contain shpefile of areas burned multiple times

##    Burned_Areas_Single_shp_summary = Results_Summary_Folder+'\\Shapefiles\\'+os.path.splitext(os.path.basename(Burned_Areas_Full_shp))[0]+'_Single_Fires.shp'          # Will contain shapefile of areas burned only once
##    Burned_Areas_Multiple_shp_summary = Results_Summary_Folder+'\\Shapefiles\\'+os.path.splitext(os.path.basename(Burned_Areas_Full_shp))[0]+'_Multiple_Fires.shp'       # Will contain shpefile of areas burned multiple times


    Intersect_LUT_CSV = Intermediate_Folder+'\\'+os.path.splitext(os.path.basename(Burned_Areas_Full_shp))[0]+'_Intersect_LUT_csv.csv'    # Will contain Look Up Table linking the ID of the overlapping area (OVERLAP_ID) with OBJECTIDs of the
                                                                                                # "original BAs
    # Create temp folder if needed

    if not os.path.exists(Temp_Out_Folder):
        os.makedirs(Temp_Out_Folder)

    # Process: Intersect - Create new shapefile containing only the intersected areas. Will generate multiple polygons for each intersection
    # i.e. in an area burnt twice, two identical polys are generated, each inheriting the attributes from one of the "original" intersecting
    # burnt areas
    print('--- Create Shapefile of Intersected burnt areas ---')
    arcpy.Intersect_analysis(Burned_Areas_Full_shp, Burned_Areas_Intersects_shp, "ALL", "", "INPUT")

    # Process: Erase - Create new shapefile containing only the Non intersected areas by erasing the intersects from the full shapefile
    print('--- Create Shapefile of areas burned once ---')
    arcpy.Erase_analysis(Burned_Areas_Full_shp, Burned_Areas_Intersects_shp, Burned_Areas_Single_shp, "")

    # Compute the areas of the intersects and add it to the intersects shapefile
    arcpy.AddField_management(Burned_Areas_Intersects_shp, "Area_Int", "DOUBLE")
    exp = "!SHAPE.AREA@HECTARES!"
    arcpy.CalculateField_management(Burned_Areas_Intersects_shp, "Area_Int", exp, "PYTHON_9.3")

    # Add a new field (OVERLAP_ID) to the intersection shapefile that will have the same value for all the identical polygons created by the intersection
    # To do that, we check if the area of a polygon with a given FID is identical to that of the polygon with FID = FID-1. If so, the OVERLAP_FID is set equal
    # to that of the preious poly, otherwise a new value is assigned

    arcpy.AddField_management(Burned_Areas_Intersects_shp, "OVERLAP_ID", "SHORT")
    polys = arcpy.UpdateCursor(Burned_Areas_Intersects_shp)

    for poly in polys:
        id_current = poly.getValue("FID")
        if id_current == 0:
           poly.setValue("OVERLAP_ID", 0)                  # On the first record, set OVERLAP_ID to 0, and Area_Previous to the area of the first record
           Area_previous = poly.getValue ("Area_Int")
           Overfid_previous = 0
        else:
           Area_current = poly.getValue ("Area_Int")       # On other records, get the Area of the current record and compare it with that of the previous one
           if Area_current == Area_previous:               # If areas are equal, set the OVERLAP_ID to the same value as before
              poly.setValue("OVERLAP_ID", Overfid_previous)
           else:                                            # If areas are different, increment the OVERLAP_ID counter. Set overlap_id of the poly to the new value
              Overfid_new = Overfid_previous+1
              poly.setValue("OVERLAP_ID", Overfid_new)
              Overfid_previous =Overfid_new
           Area_previous = Area_current                     # Update the "Area_previous" variable
        polys.updateRow(poly)

    del [poly,polys]

    # Save interesting fields to a csv file to be used as a look up table between the different OBJECTIDs and the corresponding intersecting burnt
    # Areas

    fieldnames = ['OBJECTID','FireDate','Place_Name','YearSeason','OVERLAP_ID']
    data_set = Burned_Areas_Intersects_shp
    output = Intersect_LUT_CSV
    with open(output,'wb') as out_file:
      out_writer = csv.writer(out_file, delimiter=";")
      out_writer.writerow(fieldnames)
      with arcpy.da.SearchCursor(data_set, fieldnames) as cursor:
         for row in cursor:
             row2 = row[:2] + (row[2].encode('UTF-8'),) + row[3:]                     # Convert placename to UTF-8 to avoid problems with special characters
             out_writer.writerow(row2)
      del cursor

    # Create a new shapefile, containing only the overlpping polygons, their area and the OVERLAP ID
    fieldnames = [f.name for f in arcpy.ListFields(Burned_Areas_Intersects_shp) if f.type not in ["Geometry", "Raster", "Blob"]]
    dropFields = fieldnames[2:len(fieldnames)-2]

    # Execute CopyFeatures to make a new copy of the feature class
    arcpy.CopyFeatures_management(Burned_Areas_Intersects_shp, Burned_Areas_Multiple_shp)

    # Execute DeleteField to remove unneeded fields
    arcpy.DeleteField_management(Burned_Areas_Multiple_shp, dropFields)

    # Cycle through rows, and keep only one record for each intersecting area
    polys = arcpy.UpdateCursor(Burned_Areas_Multiple_shp)
    count = 0
    for poly in polys:
        id_current = poly.getValue("OVERLAP_ID")
        if count == 0:                               # On the first record, set OVERLAP_ID to 0, and Area_Previous to the area of the first record
           Overfid_previous = poly.getValue ("OVERLAP_ID")
        else:
           Overfid_current = poly.getValue ("OVERLAP_ID")
           if Overfid_current == Overfid_previous:
              polys.deleteRow(poly)
           Overfid_previous = Overfid_current
        count = count +1
    del [poly,polys]
##    print('--- Copying Shapes and files ---')
##    arcpy.CopyFeatures_management(Burned_Areas_Single_shp, Burned_Areas_Single_shp_summary)
##    arcpy.CopyFeatures_management(Burned_Areas_Multiple_shp, Burned_Areas_Multiple_shp_summary)
##    shutil.copy(Intersect_LUT_CSV, Results_Summary_Folder)
    print('--- DONE ---')
    return('ciao')



# Import modules
import argparse
import arcpy
import os
import csv
import shutil

# set overwrite to true
arcpy.env.overwriteOutput = True  # Set overwrite to TRUE

parser = argparse.ArgumentParser()
parser.add_argument('-BFull','--Burned_Areas_Full_shp',help='Input Shapefile name', required=True)
parser.add_argument('-Inter','--Intermediate_Folder',help='OutputMain Folder', required=True)
## parser.add_argument('-Summary','--Results_Summary_Folder',help='Results_Summary_Folder', required=True)


args = parser.parse_args()
print(args)
main(args.Burned_Areas_Full_shp, args.Intermediate_Folder)



#TO test:
##Burned_Areas_Full_shp =  "H:\\FIRE_REGENERATION_TOOL_ROBERTO\\Fire_Regeneration_Tool\\Ancillary_Data\\ARCGIS_Shape\\Burned_Areas_00_12.shp"
##Intermediate_Folder = "H:\\FIRE_REGENERATION_TOOL_ROBERTO\\Data\\Outputs_new\\Intermediate_Processing\\Shapefiles"
##Results_Summary_Folder = "H:\\FIRE_REGENERATION_TOOL_ROBERTO\\Data\\Outputs_new\\Intermediate_Processing\\Shapefiles"

##Burned_Areas_Full_shp =  "F:\\Fire_Regeneration\\Ancillary_Data\\ARCGIS_Shape\\Burned_Areas_00_12.shp"
##Intermediate_Folder = "F:\\Fire_Regeneration\\Outputs_2000_2012\\Results_2000_2012\\Intermediate_Processing\\"
##Results_Summary_Folder = "F:\\Fire_Regeneration\\Outputs_2000_2012/Results_2000_2012/Summaries_for_EFFIS"

##main(Burned_Areas_Full_shp,Intermediate_Folder,Results_Summary_Folder)


