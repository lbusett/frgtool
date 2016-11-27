;+
;:Name:
;FRG_ROI_STAT_ERODE
;
;:Calling Sequence:
;
;:Description:
; Function used to extract the Scaled indexes time series for the different pixels of the different burnt areas, and save them in 
; a CSV file later used to analysize statistical significance of post-fire variations
;
;:Params:
;
;    ROI_File = ROI file of burnt areas (automatically created in previous steps)
;    Erode_File =  ENVI mask of eroded ROIS (automatically created in previous steps)
;    sVI_File = ENVI meta file of Scaled VI time series to be analyzed
;    CLC_File_00 = CLC 2000 file
;    ENV_Zones_File = Ecoregions file
;    Out_File = Output CSV file name
;    no_data_in = no_data_in, $
;    Start_Year = Start_Year, $
;    End_Year = End_Year
;
;
;
;:Returns:
;
; "DONE" + creates CSV file containing scaled vis time series for each burnt pixel
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
function FRG_ROI_STAT_ERODE,ROI_File = ROI_File, $
    Erode_File =  Erode_File, $
    sVI_File = sVI_File,$
    CLC_File_00 = CLC_File_00,  $
    ENV_Zones_File = ENV_Zones_File, $
    Out_File = Out_File, $
    no_data_in = no_data_in, $
    Start_Year = Start_Year, $
    End_Year = End_Year
    
  compile_opt IDL2
  
  End_Year = fix(End_Year)      &    Start_Year = fix(Start_Year)
  no_data_out = no_data_in
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Open Files and get useful Info
  ; -------------------------------------------------------------------------------------------------------------------- -
  
  envi_open_file, sVI_File, r_fid = in_fid,/no_realize                           ; Open the VegCover input file and get dimensions
  envi_file_query, in_fid, wl=wl, pos=pos, nb = nb, bnames = bnames, ns = ns, nl = nl
  
  envi_open_file, CLC_File_00, r_fid = clc_fid,/no_realize                       ; Open Corine File and get useful Info
  envi_file_query, clc_fid, ns = ns_clc, nl = nl_clc
  
  envi_open_file, Erode_File, r_fid = erode_fid,/no_realize                      ; Open Corine File and get useful Info
  envi_file_query, erode_fid, ns = ns_clc, nl = nl_clc
  
  envi_open_file, ENV_Zones_File, r_fid = env_fid,/no_realize                    ; Open ENV_ZONES File and get useful Info
  envi_file_query, env_fid, ns = ns_env, nl = nl_env
  
  ;   envi_open_file, CLC_File_00_06, r_fid = clc_fid_06,/no_realize                               ; Open Corine File and get useful Info
  ;  envi_file_query, clc_fid_06, ns = ns_clc_06, nl = nl_clc_06
  
  if  (in_fid eq -1) or (clc_fid eq -1) then begin                                            ; Check for correct file opening
    err_msg = dialog_message ( 'Input File Error ! Returning to Main !', /Information)
    return,'cancel'
  end
  
  if  (ns ne ns_env) or (nl ne nl_env) then begin
    err_msg = dialog_message ( 'Input VegCover File and Ecoregions rasters have different dimensions ! Returning to Main ', /Information)
    return,'cancel'
  end
  
  if  (ns ne ns_clc) or (nl ne nl_clc) then begin
    err_msg = dialog_message ( 'Input VegCover File and CORINE rasters have different dimensions ! Returning to Main ', /Information)
    return,'cancel'
  end
  print, 'Start'
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Restore the ROIs
  ; -------------------------------------------------------------------------------------------------------------------- -
  
  progressbar = obj_new('progressbar', Color='red', Text='Restoring ROIs - Wait', title = 'Computing ROIs Statistics', $
    percent = 10, xsize = 200, ysize = 40 , /fast_loop)
  progressbar -> Start
  
  envi_restore_rois, roi_file
  roi_ids = envi_get_roi_ids(roi_names=roi_names,/short_name)
  envi_get_roi_information, ROI_IDS, NPTS=roi_sizes,nl=nl_roi, ns = ns_roi
  nl_roi = nl_roi[0]        &         ns_roi = ns_roi[0]
  ;- Check ROI File
  if roi_ids [0] eq -1 then begin
    err_msg = dialog_message ( 'Invalid ROI File ! Returning to Main', /Information)
    return, 'Canceled'
  endif
  
  ;- Check ROI File Dimensions
  if (ns_roi ne ns or nl_roi ne nl)  then begin
    err_msg = dialog_message ( 'Image dimensions not compatible with selected ROI ! Returning !', /Information)
    return, 'Canceled'
  endif
  
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Initialize variables
  ; -------------------------------------------------------------------------------------------------------------------- -
  
  pos = indgen (nb)			                                            ; Bands positions of input VegCover files (= to years)
  n_rois= n_elements(roi_ids)				                                ;N� of ROIS
  n_years = nb                                                      ; Get Number of Years. Equal to number of bands in the input sVI META file
  n_rows = total(roi_sizes) + n_elements(where(roi_sizes EQ 0)) 
  roi_data_out = strarr(2*nb+4,n_rows)                    ; Initialize output matrix (It's better to create a big matrix at the start than concatenating !)
  start_index = 0                                                   ; Initialize starting index (updates in the cycle on the basis of the number of pixels in each ROI)
  
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Start Cycle on ROIS
  ; -------------------------------------------------------------------------------------------------------------------- -

  for roi=0, n_rois-1 do begin
  
    ; Progress bar initialization and update
    if progressbar->CheckCancel() then begin
      ok = dialog_message('User cancelled operation - Starting clean Up')        ; Other cleanup, etc. here.
      ; Destroy the progress bar.
      envi_file_mng, id=in_fid, /remove
      envi_file_mng, id=clc_fid, /remove
      progressbar -> Destroy
      envi_delete_rois, roi_ids
      
      return, 'Canceled'
    endif
    ; If user didn't cancel, update the progress bar.
    progressbar -> Update,  ((roi/float(n_rois))*100), Text='Analyzing roi :   '+ strtrim(roi+1,1) + '   of:   ' + strtrim(n_rois, 1)
    print, roi
    ; -------------------------------------------------------------------------------------------------------------------- -
    ; Extract values from ROI and update the output matrix
    ; -------------------------------------------------------------------------------------------------------------------- -
    roi_size = roi_sizes[roi]                                                   ; Get ROI size
 
    
    if roi_size ne 0 then begin
       end_index = start_index + (roi_size-1)                                      ; Update end_index on the basis of number of pixels in the ROI
      roi_data_temp = (float(envi_get_roi_data(roi_ids[roi], fid=in_fid,pos=pos)))         ; Get ROI data
      roi_data_temp[where(roi_data_temp eq no_data_in)] = no_data_out                      ; set no_data to new value
      
      roi_data_temp_eroded = roi_data_temp                                                            
      erode_data_temp = (float(envi_get_roi_data(roi_ids[roi], fid=erode_fid,pos=0)))      ; Get ROI data for the eroded mask
      roi_data_temp_eroded[*,where(erode_data_temp eq 0)] = no_data_out                    ; set "eroded" roi pixels to NODATA !
      
      roi_clc_temp = float(envi_get_roi_data(roi_ids[roi], fid=clc_fid,pos = 0 ))          ; Get CLC data
      roi_env_temp = float(envi_get_roi_data(roi_ids[roi], fid=env_fid,pos = 0 ))          ; Get ENV data
      ;      roi_clc_temp_06 = float(envi_get_roi_data(roi_ids[roi], fid=clc_fid_06,pos = 0 ))                  ; Get CLC_06 data
      
      roi_name = reform(replicate(ROI_NAMES[roi],roi_size),1,roi_size)                     ; Get ROI names
      roi_pixnum = reform(indgen(roi_size),1,roi_size)                                     ; Get number of pixels in ROI
      roi_data_full = [string(roi_name),string(roi_pixnum), $                              ; Create output Matrix
        string(reform(roi_clc_temp,1,n_elements(roi_clc_temp))), $
        string(reform(roi_env_temp,1,n_elements(roi_env_temp))), $
        string(roi_data_temp),string(roi_data_temp_eroded) ]
;       roi_data_full = strarr(36, roi_size)
      roi_data_out[*,start_index:end_index] = roi_data_full                                ; Put output in full out matrix
    endif else begin
      
      roi_name = ROI_NAMES[roi]                                                                                               ; if 0 element ROI put no_data line in out_matrix
      roi_data_full = [ROI_NAMES[roi],'0',string(no_data_out),string(no_data_out),string(replicate(no_data_out,nb)),string(replicate(no_data_out,nb))]
      roi_data_out[*,start_index] = roi_data_full
      end_index = start_index   
    endelse
    start_index = end_index+1              ; Update start_index on the basis of number of pixels in the last processed ROI
   ; if (start_index EQ total(roi_sizes)) then break  ; workaround: exit loop if last rois have 0 pixels: prevents prgram to stop unexpectedly
  endfor    ; End Cycle on ROIs
  
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Clean up
  ; -------------------------------------------------------------------------------------------------------------------- -
  roi_data_out = temporary(roi_data_out[*,0:end_index])
  envi_file_mng, id=in_fid, /remove
  envi_file_mng, id=clc_fid, /remove
  envi_file_mng, id=erode_fid, /remove
  
  progressbar -> Update,  ((roi/float(n_rois))*100), Text='Removing ROIs - Please Wait'
  envi_delete_rois, roi_ids
  
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Save output matrix as CSV file
  ; -------------------------------------------------------------------------------------------------------------------- -
  Years = string(strtrim(indgen(End_Year - Start_Year + 1) + Start_Year,2))   ; Create array of year names
  col_head = ['NAME','N_PIX','CLC_Class','ENV_ZONE',Years,Years]
  write_csv_data,roi_data_out,col_head, filename =Out_File, width = 50000
  
  progressbar -> Destroy
  print, 'End'
  return, Out_File
  
end

