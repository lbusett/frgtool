library(hash)
MOD_Dir       =  "/Documents/GDrive/FRG/"
Shape_File    =  "/Documents/GDrive/FRG/Input_Shapefiles/Burned_Areas_00_15.shp"
CLC_File_00   =  "/Documents/GDrive/FRG/Ancillary_Data/CLC_00/CLC_00_250_ENVI"
ENV_Zones_File = "/Documents/GDrive/FRG//Ancillary_Data/ENV_Zones/ENV_Zones.tif"
Out_Folder    =  "/Documents/GDrive/FRG/"
Start_Year    = 2000
End_Year      = 2015
Method        = 2
SNDVI         = 1
ReProc        = 2
ReDown        = 2
ReProcIm      = 2
erode         = 1
min_pix       = 10
MedWdt        = 3
NKer          = 200
sig_level     = 0.05
sub_zones     = 0
perc_diffs    = hash( c(NDVI = 9.5, RDVI = 11.5))
a =  FRG_Full_Processing(MOD_Dir= MOD_Dir, Shape_File = Shape_File, CLC_File_00 = CLC_File_00, ENV_Zones_File = ENV_Zones_File,
                               Out_Folder = Out_Folder, Start_Year = Start_Year, End_Year = End_Year,
                               Method = Method, SRDVI = SRDVI, SNDVI = SNDVI ,ReProc = ReProc, ReDown = ReDown, ReProcIm = ReProcIm,
                               erode = erode, min_pix = min_pix, MedWdt = MedWdt, NKer = NKer , sig_level = sig_level,
                               sub_zones = sub_zones,  perc_diffs = perc_diffs)