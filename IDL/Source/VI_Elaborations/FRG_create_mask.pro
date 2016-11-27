;:Name:
; FRG_Create_Mask
;
;:Calling Sequence:
;
;:Description:
;
; Function to create a mask of Burnt areas starting from EFFIS shapefile (Mask = 1 correspond to unburnt !)
;
;:Params:
;    ROI_File = ROI file of burnt areas
;    clc_File = CLC_2000 file
;    Mask_File = Output ENVI Mask File
;
;:Returns
; "DONE" + creates ENVI mask file of burnt areas
;
;:Author
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


function FRG_Create_Mask,ROI_File = ROI_File, CLC_00_File = CLC_00_File, Mask_File = Mask_File

  compile_opt IDL2
  
  ; Open the input file associated with
  ; the ROIs
  
  envi_open_file, CLC_00_File, r_fid=fid
  
  if (fid eq -1) then begin
    envi_batch_exit
    return, 'Canceled'
  endif
  
  ; Restore the ROI file and get all
  ; the available ROI ids.
  
  print, '--- Restoring ROIS ---'
  envi_restore_rois, ROI_File
  roi_ids = envi_get_roi_ids()
  class_values = intarr(n_elements(roi_ids))+1
  if (roi_ids[0] eq -1) then return, 'Canceled'
  
  ; Convert rois to class image
  envi_doit, 'envi_roi_to_image_doit', $
    fid=fid, roi_ids=roi_ids, $
    class_values=class_values, r_fid = out_fid,/in_memory, /NO_REALIZE
  ENVI_FILE_QUERY,fid, dims=dims
  
  ; Convert class image to Mask ( 1 in unburnt areas)
  print, 'Start Creating Mask file'
  pos = [0]
  exp = '(b1 EQ 0)'
  envi_doit, 'math_doit',fid=out_fid, pos=pos, dims=dims,exp=exp, $
    r_fid=mask_fid, OUT_NAME=Mask_File, /NO_REALIZE
    
  return, 'DONE'
  
end
