;:Name:
; FRG_Create_Mask_Eroded
;
;:Calling Sequence:
;
;:Description:
;
; Function to create a mask of eroded Burnt areas ROIS
;
;:Params:
;    Mask_File  = ENVI Mask file of burnt areas (Automatically created in previous steps)
;    Eroded_Mask_File = Output Mask of eroded ROIS
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

function FRG_Create_Mask_Eroded, Mask_File  = Mask_File, Eroded_Mask_File = Eroded_Mask_File

  compile_opt IDL2
  
  ; Open the input mask file associated with the ROIs
  
  envi_open_file, Mask_File, r_fid=fid
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
    METHOD=0, OUT_NAME=Eroded_Mask_File, POS=0, R_FID=out_fid
    
  return, 'Done'
  
end
