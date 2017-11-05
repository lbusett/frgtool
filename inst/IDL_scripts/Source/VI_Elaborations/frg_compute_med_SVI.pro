;+
;:Name:
;frg_compute_med_SVI
;
;:Description:
; Function used to compute the Scaled Vegetation indexes starting from original NDVI or RDVI data
;
;:Details This function is used to call the IDL function used for computation of SDVI and SNDVI yearly images.\cr 
; Also calls the IDL routines needed to create the ENVI burnt areas ROI, and the ENVI burnt areas MASKS (both total and eroded!) \cr
;  yearly SRDVI (or SNDVI) images are computed starting from the corresponding yearly Average mosaics. Computation of the scaled indexes for each pixel is done as follows:\cr
;  The pixels belonging to the same class of the target included in a window of width specified by the user centred on the target are identified on the basis of the CLC00 recoded map  
;  Pixels showing NODATA values in the input VI image, or corresponding to areas classified as "burned" on the input ROI files are removed from the above selection
;  Remaining pixels (i.e., same class, unburned and with good data) are used to compute the MOMENTS of the distribution of VI values of the target CLC class in the window centred on the target pixel
;  Computed MOMENTS are use to determine an ESTIMATE of the 5th, 50th (median) 95th percentile of the distribution, using the Cornish-Fisher fourth-order Expansion method (http://www.nematrian.com/R.aspx?p=CornishFisherDerivation))
;  Scaled indexes for the target pixel is computed exploiting this method:
;        'Percentage Deviation to Kernel Median': For each pixel, the algorithm identifies all pixels of the image corresponding to the same CLC class contained in a window of width NKer
;        It then computes the median of the distribution of values of the analyzed Vegetation index in the selecteds pixels. Scaled index for the pixel are then computed as: 
;        sVI = 100 * (VI_pix - VI_median)/(VI_median). 
; 
;:Params:
;
;    in_file = Original NDVI or RDVI file
;    sVI_File = ENVI meta file of Scaled VI time series to be analyzed
;    CLC_file_00 = CLC 2000 file
;    firemask_file = ENVI MAsk of burnt areas (Computed automatically in previous steps)
;    ENV_Zones_File = Ecoregions file
;    out_file = Output file name for scaled VI
;    no_data_out = no_data_out, $
;    year = year of analysis
;
;:Returns:
;
; "DONE" + creates ENVI file containing rescaled VI for the selected year
;
;:Author
;
;Lorenzo Busetto, phD
;EC - Joint Research Centre, Institute for Environment and Sustainability,
;Land Management & Natural Hazards Unit - FOREST (TP 261)
;Via Fermi s/n, Ispra (Va), I-21027, Italy
;Phone: 39 0332 783689
;email: lorenzo.busetto@jrc.ec.europa.eu
;http://forest.jrc.ec.europa.eu
;http://effis.jrc.ec.europa.eu
;
;-

function frg_compute_med_SVI,  $
    CLC_file_00 = CLC_file_00, $
    in_file = in_file, $
    firemask_file = firemask_file, $
    out_file = out_file, $
    nodata_out = nodata_out, $
    n_ker = n_ker, $
    index = index, $
    year = year
    
  compile_opt IDL2
    
  ; If parameters not passed, use interactive input for files and set to defaults for processing parameters
  If (N_ELEMENTS(in_file) EQ 0)    then  in_files =   DIALOG_PICKFILE(TITLE='Select Input DVI File', /MULTIPLE_FILES)
  If (N_ELEMENTS(CLC_file_00) EQ 0) then  CLC_file_00 = DIALOG_PICKFILE(TITLE='Select Input RECODED CLC2000 File ', /MULTIPLE_FILES)
  If (N_ELEMENTS(out_file) EQ 0)    then  out_file =    DIALOG_PICKFILE(TITLE='Select root name for output files', /MULTIPLE_FILES)
  If (N_ELEMENTS(n_ker) EQ 0)       then  n_ker = 801
  If (N_ELEMENTS(index) EQ 0)   then  index = 'NDVI'
  
  nodata_out = fix(nodata_out)
  nodata_in = nodata_out
  
  ; Check if n_ker is even or odd
  if  (floor(n_ker/2)-float(n_ker)/2) EQ 0 then begin
    n_ker = n_ker+1
    print, 'n_ker was an even number. It was resetted to n_ker +1'
  endif
  
  ; Compute the values corresponding to the selected percentiles on a Normal Standardized distribution (needed later for Cornish-Fisher expansion)
  med_thresh = GAUSS_CVF(1-0.5)
  
  ; Open recoded CLC00 file, get info on the file
  envi_open_file, CLC_file_00, r_fid = CLC00_fid                       ; Open the CLC00 input file and get dimensions
  envi_file_query, CLC00_fid, wl=wl, pos=pos, nb = nb, bnames = bnames, ns = ns_clc, nl = nl_clc, dims = dims
  
  ; Open the ENVI Fire Mask
  envi_open_file, firemask_file, r_fid = Fire_fid
  
  ; Initialize parameters for tiled processing (needed to increase speed)
  lines_in_tile = 1000          ; Number of lines for each tile
  n_tiles = nl_clc/lines_in_tile    ; Number of tiles
  n_pix_ker = long(n_ker)^2         ; Number of pixels in kernel
  
  ; - ------------------------------------------------ -
  ; - Start cycling on input files (One file per year) -
  ; - ------------------------------------------------ -
  
  n_files = N_ELEMENTS(in_files)
  
  ; Open files and get info
  envi_open_file, in_file, r_fid = In_fid                    ; Open the DVI input file and get dimensions
  envi_file_query, In_fid, wl=wl, pos=pos, nb = nb, bnames = bnames, ns = ns, nl = nl, dims = dims
  inherit = envi_set_inheritance(In_fid, dims, pos, /spatial)    ; Retrieve spatial information using inheritance
  if  (ns ne ns_clc) or (nl ne nl_clc) then begin                 ; Check for dimensions of VegCover and CLC06 files: should be equal !
    err_msg = dialog_message ( 'Input DVI File and CORINE rasters has different dimensions ! Returning to Main ', /Information)
    return,'cancel'
  endif
  
  ; Start Cycling on recoded CORINE classes. Considered classes are (See FRG_clc_recode):
  ; Broadleaved Forests = 2
  ; Coniferous Forests = 3
  ; Mixed Forests = 4
  ; Schlerophyllus Vegetation = 5
  ; Transitional Vegetation = 6
  ; OtherNatural Land = 7
  ; NOTE: Agricultural areas (Recoded Corine Class 1) are not considered due to the strong variability of summer DVI of different crop types, which lead to meaningless
  ; sDVI values.
  
  ; Initialize the progressbar
  progressbar = obj_new('progressbar', Color='red', Text = 'Analyzing File: '+ FILE_BASENAME(in_file), title = 'Computing sDVI',$
    percent = 10, xsize = 200, ysize = 40 , /fast_loop)
  progressbar -> Start
  
  ; CleanUp on Cancel event
  if progressbar->CheckCancel() then begin
    ok = dialog_message('User cancelled operation - Starting clean Up')        ; Other cleanup, etc. here.
    ; Destroy the progress bar.
    envi_file_mng, id=DVI_fid, /remove
    envi_file_mng, id=CLC00_fid, /remove
    progressbar -> Destroy
    envi_delete_rois, roi_ids
    return, 'Canceled'
  endif
  
  ; Allocate output matrix
  SDVI_Out_med = INTARR(ns,nl)
  
  ; - ------------------------------------------------ -
  ; Start cycling on tiles. Tiled processing is used to increase speed and avoid memory allocation problems !
  ; Tiles are created "larger" than needed to avoid problems on image borders when passing the moving window
  ; on the image
  ; - ------------------------------------------------ -
  for tile  = 0L, n_tiles-1 do begin
    ;    for tile  = 8L, 9-1 do begin
    print, 'Tile '+strtrim((tile+1),2)+' of: '+strtrim (n_tiles,2)
    
    ; If user didn't cancel, update the progress bar.
    progressbar -> Update,  (100*(tile+1)/float(n_tiles)),  Text = 'Analyzing File: '+ FILE_BASENAME(in_file)
    
    ; set the dims variable according to the tile. dims is used to define the subscripts
    ; corresponding to the different tiles
    
    case (tile) of
      0:          dims = [-1, 0 , ns -1, 0, (long(lines_in_tile))+(n_ker/2)-1]
      n_tiles-1:  dims = [-1, 0 , ns -1, long(lines_in_tile)*tile-(n_ker/2), nl-1]
      else:       dims = [-1, 0 , ns -1, long(lines_in_tile)*tile-(n_ker/2), long(lines_in_tile)*(tile+1)+(n_ker/2)-1]
    endcase
    
    ; Get corine data and Original VI data for selected tile
    
    CLC00_tile = ENVI_GET_DATA(dims = dims, fid = CLC00_fid, pos = [0])
    
    case 1  of
      (index EQ 'NDVI') OR (index EQ 'DVI'): begin
        ;        DVI_tile= (double(ENVI_GET_DATA(dims = dims, fid = In_Fid, pos = [0])))/10000 ; Get DVI data for pixels of the class
        ;        NoData_Mask = (DVI_tile NE (nodata_in/10000))
        DVI_tile= (float(ENVI_GET_DATA(dims = dims, fid = In_Fid, pos = [0]))) ; Get DVI data for pixels of the class
        NoData_Mask = (DVI_tile NE (nodata_in))
        DVI_tile = TEMPORARY(DVI_tile)/10000
      end
      (index EQ 'RDVI'): begin
        DVI_tile = (float(ENVI_GET_DATA(dims = dims, fid = In_Fid, pos = [0]))) ; Get DVI data for pixels of the class
        NoData_Mask = (DVI_tile NE nodata_in)
      end
      else: begin
      end
    endcase
    
    Fire_Tile = ENVI_GET_DATA(dims = dims, fid = Fire_fid, pos = [0])
    Fire_Mask = (Fire_Tile EQ 1)   ;& Rely_Mask = (RELY_Tile LE 1)
    undefine, Fire_Tile
    heap_gc
    
    GoodData_Mask = (NoData_Mask*Fire_Mask)  ; Create mask of good data, excluding nodata, burned areas and low reliability from DVI (Consider modifying !)
    
    ; Initialize Output sDVI tile
    sDVI_tile_med = fix(0*CLC00_tile)
    
    ; - ------------------------------------------------ -
    ; - Start cycling on classes
    ; - ------------------------------------------------ -
    for mask_code = 0L,10 do begin
    
      case (1) of
        (mask_code GE 2 AND mask_code LE 7) : begin
        
          print, 'Class '+strtrim(mask_code,2)
          
          clc_mask = (CLC00_tile EQ mask_code)             ; Create mask for pixels belonging to selected class
          clc_gooddata_mask = clc_mask*GoodData_Mask       ; Create mask for pixels belonging to selected class and NOT belonging to NODATA, burned areas and low reliability pixels
          
          DVI_masked_data = (clc_gooddata_mask)*DVI_tile   ; Mask DVI data on selected class and remove NODATA, burned areas and low reliability pixels (These are the DVI of the
          ; pixels used to compute percentile values on the kernel !)
          
          SDVI_comp_data = clc_mask*DVI_tile  ; retrieve DVI data used for SDVI computation: these are ALL DVI data of pixels of the tile belonging to the class
          ;They include "burned pixels", and also NODATA and low reliability. Results obtaine for NODATA and low reliability are removed afterwards
          
          ; For each pixel, count the number of neighbors in a N_ke0r*n_ker window belonging to the selected class,
          ; then mask the result on pixels belonging to the selected class. Result is a raster image reporting the total number of
          ; neighbors belonging to the class for pixels belonging to it, and 0 (zero) for other pixels
          
          n_pix_class  = (clc_mask)* smooth(float(clc_gooddata_mask),n_ker, /EDGE_TRUNCATE)*long(n_ker)^2
          
          ; For each pixel, compute the total DVI, mean DVI and mean (DVI^2) of pixels belonging to the selected class in a n_ker*n_ker window
          ; then mask the result on pixels belonging to the selected class. Result is a raster image reporting the total DVI of
          ; neighbors belonging to the class for pixels belonging to the class, and 0 (zero) for other pixels
          
          sum_DVI = (clc_mask)*(n_pix_ker)*(smooth(DVI_masked_data,n_ker, /EDGE_TRUNCATE))     ; sum(DVI) =  n_pix_ker*mean(DVI)
          mean_DVI = (sum_DVI)/(n_pix_class)>0                                                   ; This is the "true" Mean DVI: corrects for the proportion between total and "used" pixels
          mean_DVI_squared  = (((clc_mask)*(n_pix_ker)*(smooth((DVI_masked_data^2),n_ker, /EDGE_TRUNCATE)))/(n_pix_class))>0   ; Computes mean(DVI^2)in the moving windows and mask it
          
          ; Compute variance and standard deviation. Variance computation exploits the formula
          ; Var(X) = (1/(N*(N-1))*(mean(xi^2)-(mean(xi)^2))
          ; where N is the number of samples and xi are the single "valid" pixels in X
          
          stdev_DVI = sqrt((mean_DVI_squared - (mean_DVI^2))*(n_pix_class/(n_pix_class-1)))
          undefine, sum_DVI,clc_gooddata_mask                    ; Mem clean up
          heap_gc
          
          ; Compute third central moment and skewness
          mean_DVI_cube  = ((clc_mask)*(n_pix_ker)*(smooth((DVI_masked_data^3),n_ker))/(n_pix_class))>0 ;  Computes mean(DVI^3)in the moving windows and masks it (equals RAW moment)
          third_moment = mean_DVI_cube - $
            3*(mean_DVI)*(mean_DVI_squared)+ $
            2*(mean_DVI^3)
          ; Compute skewness using the relationship with the third moment (http://en.wikipedia.org/wiki/Central_moment)
          skew = third_moment/(stdev_DVI^3)
          where_finite = Where(finite(skew) EQ 0)
          skew[where_finite]=0
          undefine, third_moment, where_finite          ; Mem clean up
          heap_gc
          ; Compute fourth central moment and (exceed) kurtosis
          mean_DVI_fourth  = ((clc_mask)*(n_pix_ker)*(smooth((DVI_masked_data^4),n_ker))/(n_pix_class))>0 ;  Computes sum(DVI^4)in the moving windows and masks it (equals RAW moment)
          fourth_moment = mean_DVI_fourth - $                     ; Compute fourth moment
            4*(mean_DVI)*(mean_DVI_cube)+ $
            6*(mean_DVI^2)*(mean_DVI_squared)- $
            3*(mean_DVI^4)
          kurt = (fourth_moment/(stdev_DVI^4))-3                  ; compute (exceed) kurtosis
          where_finite = Where(finite(kurt) EQ 0)
          kurt[where_finite]=0
          undefine, fourth_moment, mean_DVI_fourth, mean_DVI_cube,mean_DVI_squared, DVI_masked_data, n_pix_class, where_finite ; Mem clean up
          heap_gc
          ; Estimate the low_perc and high_perc percentiles of the distribution using Cornish-Fisher fourth-order Expansion
          ; (http://www.nematrian.com/R.aspx?p=CornishFisherDerivation)
          
          DVI_CornFish_med = (mean_DVI + stdev_DVI*((med_thresh+skew*(med_thresh^2-1)/6.)+ $
            ((3*kurt*(med_thresh^3-3*med_thresh)- $
            2*skew^2*(2*med_thresh^3-5*med_thresh))/72.)))>0
            
          ; Compute sDVI (sDVI = (DVIpix - DVISoil)/(DVI_fullveg - DVISoil))
            
          sDVI_diff_med_temp = fix (100*(SDVI_comp_data - DVI_CornFish_med)/((DVI_CornFish_med)))
          
          ;          if max(SDVI_comp_data) GT 0 then stop
          
          ; add the "results" for the current class to the "full" results of the tile (Since classes are "exclusive", doesn't affect results
          ; on the previous calsses).
          
          sDVI_tile_med = temporary(sDVI_tile_med)+ clc_mask*sDVI_diff_med_temp
          
          undefine, DVI_CornFish_low, DVI_CornFish_high,sDVI_diff_med_temp,DVI_CornFish_med,mean_DVI, stdev_DVI, skew, kurt,clc_mask                  ; Mem clean up
          heap_gc
        end
        else: begin
        
          clc_mask = (CLC00_tile EQ mask_code)             ; Create mask for pixels belonging to selected class
          sDVI_tile_med = temporary(sDVI_tile_med)+ clc_mask*nodata_out ; Apply the mask to the scaled index tile --> assign nodata to "other" pixels
        end
      endcase
      
    endfor ; End cycle on classes
    
    sDVI_tile_med [where((NoData_Mask) EQ 0)] = nodata_out  ;  put to nodata NODATAs of the original VI file
    
    ; Put the info of the actual tile in the correct position of the output file
    case (tile) of
      0:          SDVI_Out_med [*,0:(lines_in_tile-1)] = sDVI_tile_med[*,0:(lines_in_tile-1)]
      n_tiles-1:  SDVI_Out_med [*,(tile*lines_in_tile):*] = sDVI_tile_med [*,(n_ker/2):*]
      else:       SDVI_Out_med [*,(tile*lines_in_tile):((tile+1)*lines_in_tile-1)] = sDVI_tile_med [*,(n_ker/2):(lines_in_tile+n_ker/2)-1]
    endcase
    
    undefine, sDVI_tile, sDVI_temp,kurt, skew, mean_DVI, stdev_DVI,n_pix_class, DVI_tile, CLC00_tile, sDVI_tile_med
    heap_gc
    
  endfor   ; End cycle on tiles
  
  print, 'Writing Output Files'
  
  ; CleanUp
  envi_file_mng, id=clc00_fid, /remove
  envi_file_mng, id=Fire_fid, /remove
  
  ;Write output file
  bnames = "Med_Perc" + index
  ENVI_WRITE_ENVI_FILE, SDVI_Out_med, out_name=out_file, inherit = inherit, bnames = bnames, wl = year, /NO_COPY, /NO_OPEN
  
  ; CleanUp
  envi_file_mng, id=In_Fid, /remove
  progressbar -> Destroy
  return, 'DONE'
  
end
