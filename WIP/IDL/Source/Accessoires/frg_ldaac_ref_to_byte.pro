

function FRG_LDAAC_REF_TO_BYTE,File_List = File_List, scale_factor = scale_factor

  compile_opt IDL2
  
  ;ENVI
  Main_Dir = ProgramRootDir(/ONEUP)
  if (N_ELEMENTS(File_List))EQ 0 then File_List = DIALOG_PICKFILE(TITLE='Select Input Files', /MULTIPLE_FILES)
  if (N_ELEMENTS(scale_factor))EQ 0 then begin
    print, 'Please Specify the scale factor to be used'
    return, 'FAILED'
  endif
  
  for file = 0L, N_ELEMENTS(File_List)-1 do begin
  
    in_file = FSC_BASE_FILENAME(File_List[file], DIRECTORY=in_dir, extension = extension)
    out_file = in_dir+in_file+'_byte'+'.'+extension
    envi_open_file, File_List[file], r_fid = in_fid                       ; Open the DVI input file and get dimensions
    envi_file_query, in_fid, wl=wl, pos=pos, nb = nb, bnames = bnames, ns = ns, nl = nl, dims = dims
    pos = [0]
    exp = 'byte(b1*'+strtrim(scale_factor,2)+')'
    envi_doit, 'math_doit',fid=in_fid, pos=pos, dims=dims,exp=exp, out_name=out_file, $
      r_fid=out_fid
  endfor
  return,'DONE'
  
end

