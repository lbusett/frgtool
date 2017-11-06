;+
;:Name:
; FRG_COMPUTE_RDVI
;
;:Calling Sequence:
;
;:Description:
;
; Function to compute the Relative Difference Vegetation Index (RDVI) starting from COUPLES
; of files containing NIR and RED reflectances.
;
;:Params:
;    NIR_file = NIR_file
;    RED_File = RED_File
;    Out_File = Output RDVI File
;    nodata_in = nodata_in
;    nodata_out = nodata_out
;
;:Author:
;
;:Returns:
;
; "DONE" + creates ENVI file containing RDVI for the selected year
;:
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

function LB_NaN_to_noData,Data, nodata_in, nodata_out

  for nd = 0, N_ELEMENTS(nodata_in)-1 do begin
  
    WhereNaN = FINITE(Data,/NAN)
    Data[WhereNaN]= nodata_out
    
  endfor
  
  return, Data
  
end

function FRG_COMPUTE_RDVI, $
    NIR_file = NIR_file, $
    RED_File = RED_File, $
    Out_File = Out_File, $
    nodata_in = nodata_in, $
    nodata_out = nodata_out
    
  compile_opt IDL2
  
  ;ENVI
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Open Files and get useful Info
  ; -------------------------------------------------------------------------------------------------------------------- -
  
  print, 'Building RDVI file ' + strtrim (out_file,2)
  envi_open_file, NIR_File, r_fid = NIR_fid                       ; Open the NIR MODIS input file and get dimensions
  envi_file_query, NIR_fid, wl=wl, pos=pos, nb = nb, bnames = bnames, ns = nsn, nl = nln, dims = dims
  
  envi_open_file, RED_File , r_fid = RED_fid,/no_realize                           ; Open the RED MODIS input file and get dimensions
  envi_file_query, RED_fid, wl=wl, pos=pos, nb = nb, bnames = bnames, ns = nsr, nl = nlr
  
  ; Check for congruence on files dimensions
  if  (NIR_fid eq -1) or (RED_fid eq -1) then begin                                            ; Check for correct file opening
    err_msg = dialog_message ( 'Input File Error ! Returning to Main !', /Information)
    return,'FAILED'
  end
  
  if  (nsn ne nsr) or (nln ne nlr) then begin                                            ; Check for correct file opening
    err_msg = dialog_message ( 'Dimensions of RED and NIR file differ ! Returning to Main !', /Information)
    return,'FAILED'
  end
  
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Compute RDVI File from RED and NIR reflectance.
  ; -------------------------------------------------------------------------------------------------------------------- -
  
  t_fid = [NIR_fid,RED_fid]
  pos = [0,0]
  exp = '(((b1-b2)/sqrt(b1+b2))*(b1 NE '+strtrim(nodata_in,2)+' AND b2 NE'+strtrim(nodata_in,2)+')+(' $
    +strtrim(nodata_out,2)+'*(b1 EQ '+strtrim(nodata_in,2)+' OR b2 EQ'+strtrim(nodata_in,2)+')))'
    
  outdir = FILE_DIRNAME(out_file, /MARK_DIRECTORY)
  tmp_file = outdir +'tmp_file'
  
  ; Compute and save in a temporary file
  envi_doit, 'math_doit',fid=t_fid, pos=pos, dims=dims,exp=exp, $
    r_fid=DVI_fid, out_name=tmp_file,  /no_realize
    
  ; Convert to TIFF and save to final file
  envi_output_to_external_format, /tif, pos = 0, dims = dims, out_name=out_file, fid = DVI_fid
  
  print, 'DVI File Built !'
  envi_file_mng, id = NIR_fid,/REMOVE
  envi_file_mng, id = RED_fid,/REMOVE
  envi_file_mng, id = DVI_fid,/REMOVE
  envi_file_mng, id = t_fid,/REMOVE
  
  ; Delete temporary File
  FILE_DELETE,tmp_file
  
  return,'DONE'
  
end

