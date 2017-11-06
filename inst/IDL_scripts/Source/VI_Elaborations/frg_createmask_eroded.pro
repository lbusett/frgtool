;:Name:
; frg_createmask_eroded
;
;:Calling Sequence:
;
;:Description:
;
; Function to create a mask of eroded Burnt areas ROIS
;
;:Params:
;    mask_file  = ENVI Mask file of burnt areas (Automatically created in previous steps)
;    eroded_mask_file = Output Mask of eroded ROIS
;
;:Returns
; "DONE" + creates ENVI mask file of eroded ROIS
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

function frg_createmask_eroded, mask_file  = mask_file, eroded_mask_file = eroded_mask_file

  compile_opt IDL2
  
  ; Open the input mask file associated with the ROIs
  
  envi_open_file, mask_file, r_fid=fid
  if (fid eq -1) then begin
    envi_batch_exit
    return, 'Canceled'
  endif
  ENVI_FILE_QUERY,fid, dims=dims
  
  pos = [0]
  exp = '(b1 EQ 0)'
  
  ; Invert the original mask
  envi_doit, 'math_doit',fid=fid, pos=pos, dims=dims,exp=exp, $
    r_fid=mask_fid,/IN_MEMORY
  ENVI_FILE_QUERY,mask_fid, dims=dims
  
  ; Apply erosion filter and create eroded mask
  kernel = fltarr(3,3) +1
  value = kernel
  ENVI_DOIT, 'MORPH_DOIT', CYCLES=1, DIMS=dims, FID=mask_fid, KERNEL=kernel, /gray,$
    METHOD=0, OUT_NAME=eroded_mask_file, POS=0, R_FID=out_fid
    
  return, 'Done'
  
end
