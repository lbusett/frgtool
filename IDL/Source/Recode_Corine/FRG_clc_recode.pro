function FRG_CLC_RECODE

  ; ,ROI_File = ROI_File, $
  ;                                         VegCover_File = VegCover_File,$
  ;                                         CLC_File_00 = CLC_File_00, $
  ;                                         CLC_File_06 = CLC_File_06, $
  ;                                         Out_File = Out_File, $
  ;                                         no_data_in = no_data_in, $
  ;                                         no_data_out = no_data_out
  compile_opt IDL2
  
;    envi, /restore_base_save_files
      ;ENVI
;    !error_state.code = 0
    
      Main_Dir = ProgramRootDir(/ONEUP)
      Data_dir = 'E:\Projects_Data\Fire_Regeneration_Data\input\'       &    Res_Dir  = 'E:\Projects_Data\Fire_Regeneration\Results\'   
      Prev_Dir  = Main_Dir + 'Previous\' 
    
    info_Recode_Files = {CLC_File :Data_dir + 'Accessory\CLC_00_250\CLC_00_250_ENVI_cropped',$
                         out_file :Data_dir + 'Accessory\CLC_00_250\CLC_00_250_ENVI_recoded' }
  
      envi_open_file, info_Recode_Files.CLC_File, r_fid = clc_fid,/no_realize                               ; Open Corine File and get useful Info
      envi_file_query, clc_fid, ns = ns_clc, nl = nl_clc,dims = dims
      
      
  ; -------------------------------------------------------------------------------------------------------------------- -
  ; Recode the CLC classes according to the scheme shown in "Legend.txt" and the following LUT
  ; Artificial Surfaces = 0
  ; Agricultural Areas = 1
  ; Broadleaved Forests = 2
  ; Coniferous Forests = 3
  ; Mixed Forests = 4
  ; Schlerophyllus Vegetation = 5
  ; Transitional Vegetation = 6
  ; OtherNatural Land = 7
  ; Wetlands = 8
  ; Water Bodies = 9
  ; Other = 10
  ; -------------------------------------------------------------------------------------------------------------------- -
  print, 'Start Building DVI files'
  t_fid = [CLC_fid]
  pos = [0]
  exp = 'byte((b1 LE 11)*0+(b1 GE 12 AND b1 LT 23)*1+(b1 EQ 23)*2+(b1 EQ 24 )*3+(b1 EQ 25 )*4+(b1 EQ 28 )*5+(b1 EQ 29 )*6+(b1 GE 26 AND b1 LE 27 )*7+(b1 GE 30 AND b1 LE 34 )*7+(b1 GE 35 AND b1 LE 39 )*8+(b1 GE 40 AND b1 LE 44 )*9+(b1 GE 45 )*10)'
  out_name =  info_Recode_Files.out_file 
  envi_doit, 'math_doit',fid=t_fid, pos=pos, dims=dims,exp=exp, out_name=out_name, $
   r_fid=DVI_fid
  print, 'CLC File Built !'
  
  return, 'Ok'
end
