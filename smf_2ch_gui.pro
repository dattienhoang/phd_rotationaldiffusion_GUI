;smf_2ch_gui.pro -- 
;Routine to calculate the autocorrelation function for indidivual intensity
;traces as well as the quasi-ensemble autocorrelation. 
;
;Dat Tien Hoang, Last major edit: 2013-10-01
;
; This is the main program for processing our single molecule fluorescence
; rotational dynamics data.  It is designed to help locate bright spots in
; both polarization channels, then find their relative intensities. From this
; intensity data it constructs autocorrelation functions, thereby giving us
; access to information about the rotational diffusion of the molecules.
; 
; The colors used in this program are almost all greyscale, except
; for the three colors at the top of the color table, which are
; assigned to red, green and blue respectively.
;
; This program was originally written by Tobias K. Herman (TKH) [DATE], and 
; updated June 2013 by Dat Tien Hoang (DTH)
;
; This requires a *.bin movie file, but can convert a TIFF stack to a *bin
; file format.
;
;Caveats: 
;1.) Everytime this GUI is to be used on a new computer, you need to change the
;    info.Temp_FilePath s.t. it contains the directory containing the recovery
;    *.sav file! I may get rid of this in the future...
;******************************************************************************
PRO SMF_2CH_GUI_TLB_Events, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END

;******************************************************************************
;Allow user to load a *.bin file or a *.tif stack (which would then be
;converted to a *.bin file
PRO SMF_2CH_GUI_Load_Movie, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  Widget_Control, /Hourglass

  print, 'Loading in a movie file...'
  Widget_Control, event.id, Get_UValue=buttonValue
  CASE StrUpCase(buttonvalue) OF
    '.BIN': BEGIN
      bin_filepath = Dialog_pickfile(Title='Choose a *.bin Movie File', $
                     Filter='*.bin', path='/home/administrator/')
      IF bin_filepath NE '' THEN BEGIN
        ;Get information about image size in pixels (in the filename!)
        splt_name = StrSplit(bin_filepath, '-', /extract)
        Base_Filepath = splt_name[0]
        Image_Size = Long(StrSplit((StrSplit(splt_name[1],'.', /extract))[0], $
                   'x', /extract))
      ENDIF
    ENDCASE

    '.DAT': BEGIN
      dat_filepath = Dialog_pickfile(Title='Choose a *.dat Movie File', $
                     Filter='*.dat', path=filepath(*info.Associated_Name))
      IF dat_filepath NE '' THEN BEGIN
        print, '...convert *.dat to *.bin filetype:', dat_filepath
        Wait, 0.05

        OpenR, lun, dat_filepath, /get_lun
        BinRead, lun, image
        Close, lun
        Free_lun, lun

        ;Save image as a .bin file
        Image_Size = Size(image)
        Image_Size = Image_Size[1:*]
        print, '...*.dat file read!'
        Bin_Filepath = StrSplit(dat_filepath, '.', /extract)
        Base_Filepath = Strjoin(Bin_Filepath[0:N_Elements(Bin_Filepath)-2])
        Bin_Filepath = StrCompress(Base_Filepath + '-' + String(Image_Size[1]) + $
                    'x' + string(Image_Size[2]) + '.bin', /remove_all)

        Print, '...outputting *.bin file:', Bin_Filepath
        OpenW, lun, Bin_Filepath, /get_lun
        Writeu, lun, image
        Close, lun
        Free_lun, lun
      ENDIF ELSE bin_filepath = ''
    ENDCASE

    '.TIF':BEGIN
      file = Dialog_Pickfile(Title = 'Choose a TIFF Stack Movie File', $
            Filter = '*.tif', path = filepath(*info.Associated_Name))
       IF file NE '' THEN BEGIN
        print, '...convert *.tif to *.bin filetype:', file
        Tif_to_bin, file = file, Image_Size = Image_Size,$
        bin_file_out = bin_filepath, bin_file_base = base_filepath
        print, '...outting *.bin file:', bin_file_out
       ENDIF ELSE bin_filepath = ''
    ENDCASE
  ENDCASE

  ; If file loaded, display filename and load the first frame into the
  ; 'summed image' and 'BPass image' variables in case the user wishes
  ; to bypass those procedures. Store image size in the info structure
  ; for future reference
  print, bin_filepath

  IF bin_filepath NE '' THEN BEGIN
    Bin_info = File_Info(bin_filepath) & help, bin_info.size
    info.S_Image = [Image_Size[0],Image_Size[1], $
      long64((Bin_Info.size)/(Image_Size[0]*Image_Size[1]))]
      help, (info.s_image)[2] & print, info.s_image
    *info.associated_Name  = Bin_Filepath
    info.Base_Filepath     = Base_Filepath

    info = SMF_2CH_New_Movie_Initialize_20130731(info)
    Widget_Control, info.widgetIDs.ProcessMenu, sensitive=1

    ;Prompt user for the frame rate/time of the new movie; update display
    FrameTime = SMF_GUI_Frametime(value=info.analysis.Frametime, $
                Group_leader=event.top, label='Frametime', $
                Title=StrCompress('Enter Frametime for:' + $
                *info.associated_Name), Cancel=cancelled)
    IF cancelled EQ 0 THEN info.analysis.Frametime = FrameTime
    info.MovieInfo[6] = StrCompress('Frame Time (Seconds per Frame): ' + $
                        String(info.analysis.FrameTime))
    Widget_Control, info.WidgetIDs.MInfoText, set_Value=info.MovieInfo
  ENDIF ELSE dummy = Dialog_Message('No file was loaded!')

  IF *info.associated_Name NE '' THEN $
    Widget_Control, info.WidgetIDs.DriftButton, sensitive = 1 ELSE $
    Widget_Control, info.WidgetIDs.DriftButton, sensitive = 0
    
    Widget_Control, event.top, Set_UValue=info, /No_Copy 
END; of SMF_GUI_Load_Movie

;******************************************************************************
;Remove drift from movie using the method of "Residual Overlaps". The output is
;a new *.bin file that contains a dedrifted movie, but also retains original
;data in a movie whose filename was appended with "_unshifted". If the original
;data is desired, remove the appended tag for compatibility purposes with file
;naming conventions

PRO SMF_2CH_GUI_Drift, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy

  SMF_Correct_Sample_Drift, info, Group_Leader = event.top
  
  ;Get image size info (in pixels and frames) of the dedrifted movie
  split_name = StrSplit(*info.Associated_Name, '-', /extract)
  Image_Size = Long(StrSplit((StrSplit(split_name[1], '.', /extract))[0], 'x', $
               /extract))
  Bin_info = File_Info(*info.Associated_Name)
  info.S_Image = [Image_Size[0], Image_Size[1], $
                 Fix((Bin_Info.size)/(Image_Size[0]*Image_Size[1]))]

  ;Update the info structure to be consistent with new (trimmed) image
  info = SMF_2CH_New_Movie_Initialize_20130731(info)
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Drift

;******************************************************************************
;This routine is designed to be run after a program crash.  It will bring
;the user back to the point in the analyis that they were at before the
;crash.  It is not for restoring saved analysis files, just for crash
;recovery
PRO SMF_2CH_GUI_Recover, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  info = SMF_2CH_Recovery_20130731(info)
  ;Recenter dislay if it has been resized
  CenterTLB, event.top, 0.5, 0.5
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Recover

;******************************************************************************
;Allow user to specify the physical separation between Channels on CCD chip and
;also visualize the accuracy of this setting
PRO SMF_2CH_GUI_Channel_Separation, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  info.ChannelSeparation = SMF_2CH_Set_Channel_Separation_20130731(*info.summedImage, $
                           Group_Leader=event.top, $
                           Features=info.Features, $
                           ChannelSeparation=info.ChannelSeparation)
  ;Update movie info display
  info.MovieInfo[8] = StrCompress('Channel Offset (pixels): ' + $
                      String(info.ChannelSeparation[0]) + ',' + $
                      String(info.ChannelSeparation[1]))
  Widget_Control, info.WidgetIDs.MInfoText, Set_Value=info.MovieInfo
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Channel_Separation

;******************************************************************************
;Sum frames to create an apparant increase in SNR for finding features
PRO SMF_2CH_GUI_Sum_Frames, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  WSET, info.widgetIDs.D2_WID        ;Set window to middle draw widget
  *info.summedImage = SMF_Sum_Frames(*info.Associated_Name, info.S_Image, $
      InitialSum=*info.summedimage, Frame_Format= *info.Image_Template, $
      Group_Leader=event.top)
  *info.BPImage = *info.summedImage ; update the BPImage in case user wants to go
                    ; directly to 'Find Features'
  info.BPFiltered = 0B         ; Set filtered flag to 'NO'
  ;Display final summed image
  Erase
  tvscl, *info.summedimage, top=info.top
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Sum_Frames

;******************************************************************************
;Set the parameters for noise filtering by convolution (a la Crocker and Weeks)
PRO SMF_2CH_GUI_BPass, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  WSet, info.widgetIDs.D1_WID

  IF info.BPFiltered THEN BEGIN
    BPResult = SMF_BPass(*info.SummedImage, BPParam=info.BPParam, $
            PreviousTry=*info.BPImage, Group_Leader = event.top)
  ENDIF ELSE BPResult = SMF_BPass(*info.SummedImage, BPParam=info.BPParam, $
                             Group_Leader=event.top)
  IF BPResult.Cancelled NE 1 THEN BEGIN
    info.BPFiltered = 1
    ;Update Analysis info display
    info.AnalysisInfo[2] = StrCompress('BPass Parameters: ' + $
                           String(BPResult.PSize, Format='(I3)') + ',' + $
                           String(BPResult.NSize, Format='(F4.1)'))
    Widget_Control, info.WidgetIDs.AInfoText, Set_Value=info.analysisInfo
  ENDIF

  *info.BPImage = BPResult.BPImage
  info.BPParam.PSize = BPResult.PSize
  info.BPParam.NSize = BPResult.NSize

  WSet, info.widgetIDs.D2_WID        ;Display final result in middle draw widget
  Erase
  TVSCL, BPResult.BPImage

  Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; of SMF_GUI_BPass

;******************************************************************************
;Set the parameters to use with the dilation operators to identify features 
;(a la Crocker and Weeks)
PRO SMF_2CH_GUI_Feature, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy

  ;Check to see if user has applied BPass.  If not, confirm whether they
  ;really want to use the feature routine on unfiltered data
  IF ~info.BPFiltered THEN BEGIN
  DialogTemp = Dialog_Message('Really find features in an unfiltered image?', $
               /Question, /Default_No)
    IF StrUpCase(DialogTemp) EQ 'NO' THEN BEGIN
      Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
    ENDIF
  ENDIF

  WSet, info.widgetIDs.D1_WID
  FeatureInfo=SMF_Feature(*info.BPImage, ParticleSize=info.FeatureParam[0], $
                   Masscut=info.FeatureParam[1], Group_Leader=event.top, $
                   color=info.top)
  IF N_Elements(FeatureInfo.Features) LT 6 THEN BEGIN
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    RETURN
  ENDIF ; of case where Cancel button was pressed

  ;Remove all the features which are too close to the edge (defined arbitrarily
  ;as 5 pixels for now)
  XValues = FeatureInfo.Features[0,*]
  YValues = FeatureInfo.Features[1,*]
  InRange = Where(((XValues GE 5) and (Xvalues LE (info.S_Image[0] - 5))) AND $
            ((YValues GE 5) and (Yvalues LE (info.S_Image[1] - 5))), count)

  IF count GT 0 THEN BEGIN ; Are there any usable features?
    Features = FeatureInfo.Features[0:2,InRange]
    *info.Features = Features
    info.FeatureParam = [FeatureInfo.ParticleSize, FeatureInfo.Masscut]
    ENDIF ELSE BEGIN ;If not, then get out of this routine
    Widget_Control, event.top, Set_UValue=info, /No_Copy
    RETURN
  ENDELSE

  ;Update analysis info display
  info.AnalysisInfo[3] = StrCompress('Feature Parameters: ' + $
                         String(FeatureInfo.ParticleSize) + ',' + $
                         String(FeatureInfo.Masscut))
  Widget_Control, info.WidgetIDs.AInfoText, Set_Value=info.AnalysisInfo

  ;Update the table display
  IF Widget_Info(info.widgetIDs.TableContents, /Combobox_Number) EQ 0 THEN $
     Widget_Control, info.widgetIDs.TableContents, Combobox_AddItem='Features'
  Widget_Control, info.widgetIDs.Table, Table_YSIZE=(size(*info.Features))[2]
  Widget_Control, info.widgetIDs.Table, Set_Value=(*info.Features)
  Widget_Control, info.widgetIDs.Table, Column_Labels=['X', 'Y', 'Brightness']
  Widget_Control, info.widgetIDs.RegisterButton, Sensitive=1
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Feature


;******************************************************************************

;Take the coordinates of the features found and try to match up individual
;features from the left and right channels using separation values set by user
PRO SMF_2CH_GUI_Register, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Coordinates = (*info.Features)[0:1,*]
DeltaX=   info.ChannelSeparation[0] ;X separation of channels
DeltaY=   info.ChannelSeparation[1] ;Y separation of channels
Fuzz=     info.ChannelSeparation[2] ;Uncertainty in separation

;Change cursor to hourglass while this calculation runs
Widget_Control, /Hourglass
Wait, 0.05           ; Allow time to display cursor

;Call the subroutine which actually does the calculation if at >= two features were found
IF N_Elements(Coordinates) GT 2 THEN $
    PairFeatures = register_polarization_ccd_channels_20130731(Coordinates, $
                   DeltaX, DeltaY, Fuzz) $
    ELSE PairFeatures=-1
    
;Check to make sure some pairs were identified.  if not, then generate a dialog
;and leave the display unchanged.  If no pairs are found, then the 'Pairfeatures'
;Array will consist only of '-1', so that gives us a way to check it

IF PairFeatures[0] NE -1 THEN BEGIN ;At least one pair was found...
    *info.PairsTable = PairFeatures ;Store Pair coordinates
    
    ;Display Results in table Widget; update the combobox widget
    Widget_Control, info.widgetIDs.Table, Set_Value = PairFeatures
    Widget_Control, info.widgetIDs.Table, Column_Labels = ['X1','Y1','X2','Y2']
    ;Add display options to the table
    IF Widget_Info(info.widgetIDs.TableContents, /Combobox_Number) EQ 1 THEN $
         Widget_Control, info.widgetIDs.TableContents, Combobox_AddItem = 'Pairs'
    Widget_Control, info.widgetIDs.TableContents, Set_Combobox_Select = 1
    IF Widget_Info(info.widgetIDs.TableContents, /Combobox_Number) EQ 2 THEN $
       Widget_Control, info.widgetIDs.TableContents, Combobox_AddItem = 'Good Pairs'

    Widget_Control, info.widgetIDs.PickPairs, Sensitive = 1
    Widget_Control, info.widgetIDs.AnalysisMenu, sensitive=1
    Widget_Control, info.widgetIDs.DataProcessMenu, sensitive=1
    ENDIF ELSE BEGIN ;If no features matched up then...
    A=Dialog_message(['No pairs of spots could be found.',$
       'Please check that your channel separation is set correctly.'], $
         Title='Fluorophore Registration Failed')
ENDELSE

Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Register


;******************************************************************************
;Open up a new window that allows the user to choose which of the fluorophores
;that have been identified should actually be analyzed

PRO SMF_2CH_GUI_GoodPairs, event
Widget_Control, event.top, Get_UValue=info, /No_Copy

IF N_Elements(*info.PairsTable) GT 1 THEN BEGIN
  help, *info.PairsTable
  
  ;ld_prev = Dialog_Message(['Do you have previously saved molecules?', $
  ;          'If so, do you want to skip to loading just those?'], /center, /question)
  ;if ld_prev eq 'Yes' then goto, load_pairs
;------------------------------------------------------------------------------   
  count = 0
  ;check to see if there are any repeat pairs by checking with
  ;file that has registered molecules from past analysis, otherwise
  ;click 'cancel'
  RegisteredPairs_Check, info, NewPairs, base_filepath, count 
    
 ;this checks to see if registeredpairs_check was run or not
  If count eq 1 then begin
    ; if registered pairs check was run, check to see ; if there are any new pairs
    If n_elements(where(NewPairs gt 0)) ge 4 then begin                              
      *info.PairsTable = fltarr(4, n_elements(NewPairs)/4)
      *info.PairsTable = NewPairs
      
       Pairs = SMF_2CH_Pick_Fluorophores_20130731(*info.SummedImage, info.top, $
               *info.PairsTable, Group_leader = event.top)
       ;Replace with updated pair info (including 'Good' tag)
       *info.PairsTable = Pairs 
       ;;keeps track of 'good' molecules from; form fluorophore picking
       ;dummy = Dialog_Message('First, remove previously found molecules...')
       RegisteredPairs_Track, info, *info.pairstable
       endif else dummy = Dialog_Message(['All pairs found were previously found also.', $
                          'Sum different range of frames!'])      
    endif else begin
    ; if no registeredpairs_check was run then track the pairs you've just registered
    Pairs = SMF_2CH_Pick_Fluorophores_20130731(*info.SummedImage, info.top, $
            *info.PairsTable, Group_leader = event.top)
    ;Replace with updated pair info (including 'Good' tag) 
    *info.PairsTable = Pairs
    ;keeps track of 'good' molecules from; form fluorophore picking
    ;dummy = dialog_Message('Concatenate the results with those previous.')
    RegisteredPairs_Track, info, *info.pairstable
  endelse
;------------------------------------------------------------------------------ 
  
  ;dummy = dialog_message('Finally, choose which file of molecules to use')
  load_pairs:
  check=0
  registeredpairs_load, info, Pairs, check
  if check eq 1 then *info.PairsTable = Pairs

  Widget_Control, info.widgetIDs.Table, Set_Value = Pairs
  Widget_Control, info.widgetIDs.Table, Column_Labels = ['X1','Y1','X2','Y2','Use?']
                                     
ENDIF

Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_GoodPairs

;******************************************************************************
; Sets the size of spot over which to integrate the intensity of
; the fluorophores, as well as the size of the region around the
; fluorophore used to compute the background intensity.
PRO SMF_2CH_GUI_Set_Averaging_Params, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  ;help, *info.summedimage
  print, 'Current Averaging Parameters', info.analysis.AveragingParams
  info.Analysis.AveragingParams = SMF_2CH_Set_Analysis_Params_20130731(*info.SummedImage, $
                                  info.Analysis.AveragingParams, event.top)
  ;Update Analysis info display
  info.AnalysisInfo[4] = StrCompress('Signal Size (pixels): ' + $
               String(info.analysis.AveragingParams.PSize, Format = '(F4.1)'))
  info.AnalysisInfo[5] = StrCompress('Background Averaging (pixels): ' + $
               String(info.analysis.AveragingParams.BSize, Format = '(F4.1)'))
  Widget_Control, info.WidgetIDs.AInfoText, Set_Value=info.AnalysisInfo
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Set_Averaging_Params


;******************************************************************************
;User can select what range of frames to use from within the movie.  Furthermore
;she can choose to average a number of frames together, reducing the number of
;time points, but also averaging out some noise.
PRO SMF_2CH_GUI_Movie_Frames, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy

  Result = SMF_Frame_Parameters(*info.Associated_Name, info.S_Image , $
           *info.Image_Template, info.Analysis.FrameRange, $
           info.Analysis.FrameTime, Group_Leader = event.top)

  info.Analysis.FrameRange = Result.Range
  info.Analysis.FrameTime = Result.Time
  Frames = [Result.Range.Start, Result.Range.Finish, Result.Range.Step]

  ;Update Movie info display
  Info.MovieInfo[2:7] = [$
      [StrCompress('# of Frames (Total): '+ String(Info.S_Image[2], $
        Format='(I4)'))], $
      [StrCompress('# of Frames (Analyzed): '+ String(Frames[1]-Frames[0]+1, $
        Format='(I4)'))],$
      [StrCompress('Start Frame: ' + String(Frames[0], $
        Format='(I4)'))],$
      [StrCompress('End Frame: ' + String(Frames[1], $
        Format='(I4)'))], $
      [StrCompress('Frame Time (Seconds per Frame): ' + String(Result.Time, $
        Format='(F6.3)'))],$
      [StrCompress('Time Step (seconds): ' + String(Result.Time*Frames[2], $
        Format='(F6.3)'))]]

  Widget_Control, info.WidgetIDs.MInfoText, Set_Value=info.MovieInfo

  ;Update Analysis info display
  info.AnalysisInfo[0] = StrCompress('# of Frames Averaged per Time Point: ' + $
          String(Frames[2], Format = '(I4)'))
  info.AnalysisInfo[1] = StrCompress('# of Time Points: ' + $
          String(Fix((Frames[1]-Frames[0]+1)/Frames[2]), Format = '(I4)'))
  Widget_Control, info.WidgetIDs.AInfoText, Set_Value=info.AnalysisInfo
  info.analysis.N_Points = Fix((Frames[1]-Frames[0]+1)/Frames[2])

  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Movie_Frames


;******************************************************************************
;Change the data displayed in the table between a list of individual features
;found and a list of registered pairs of features
PRO SMF_2CH_GUI_TableDisplay, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  Widget_Control, event.id, Get_UValue = widget

  CASE StrUpCase(widget) OF
    'LIST' : BEGIN ;User selects content of data display table
      CASE StrUpCase(event.str) OF
        'FEATURES' : BEGIN
          Widget_Control, info.widgetIDs.Table, Set_Value=*info.Features
          Widget_Control, info.widgetIDs.Table, Column_Labels= $
                          ['X1', 'Y1', 'Brightness']
        ENDCASE
        'PAIRS' : BEGIN
          Widget_Control, info.widgetIDs.Table, Set_Value=*info.PairsTable
          Widget_Control, info.widgetIDs.Table, Column_Labels= $
                          ['X1', 'Y1', 'X2', 'Y2', 'Use?']
        ENDCASE
        'GOOD PAIRS' : IF N_Elements(*info.Pairs) GT 0 THEN BEGIN
          GoodTable = SMF_2CH_GUI_GoodTable_20130731(*info.Pairs)
          Widget_Control, info.widgetIDs.Table, Set_Value=GoodTable
          Widget_Control, info.widgetIDs.Table, Column_Labels= $
                          ['X1', 'Y1', 'X2', 'Y2', 'Done', 'GoodFit1', $
                          'GoodFit2', 'Hetero', 'Printed']
          ENDIF ELSE BEGIN
          Widget_Control, info.widgetIDs.TableContents, Set_Combobox_Select = 1
          Widget_Control, info.widgetIDs.Table, Set_Value = *info.PairsTable
          Widget_Control, info.widgetIDs.Table, Column_Labels = $
                          ['X1', 'Y1', 'X2', 'Y2', 'Use?']
        ENDELSE
        ENDCASE
    ENDCASE

    'TABLE' : BEGIN 
    ;User selects the pair he wishes to work with.  This should
    ;only happen once the 'All_Table_Events' flag has been set
    ;during the 'start analyis' procedure
    ; If cells are selected then set that as the active pair and update
    ; the pair select slider, then generate a 'Pair Select' event to
    ; trigger an update of the analysis siplays
      IF ((event.type EQ 4) and (event.Sel_Top NE -1)) THEN BEGIN
        info.analysis.SelectedPair = event.sel_top < (N_Elements(*info.Pairs)-1)
        Widget_Control, info.WidgetIDs.PairSelect, Set_Value=event.sel_top
        Widget_control, info.WidgetIDs.PairSelect, Send_Event = {ID:0L, TOP:0L, HANDLER:0L}
      ENDIF
    ENDCASE
  ENDCASE
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_TableDisplay

;******************************************************************************
;This widget will activate and deactivate the controls which will allow the user
;to look at intensity as a function of time for a single fluorophore (i.e. pair of spots)
PRO SMF_2CH_GUI_Processing, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_UValue=state
Widget_Control, /Hourglass & Wait, 0.05

CASE StrUpCase(state) OF
  'START' : BEGIN 
  ;Starts the analysis procedure. Set-up data structures, extract
  ;intensities, g-factor correction for each molecule and its background.
  ;
  ;Ultimately, this data extraction is part is meant to supply the basic
  ;information needed to mark some molecules bad
  ;
  ;If the user has asked, trim the movie, leaving only the
  ;frames the user wishes to analyze
  Frames = [info.Analysis.FrameRange.Start,$
           info.Analysis.FrameRange.Finish, info.Analysis.FrameRange.Step]
  IF (Frames[1]-Frames[0] LT (info.S_Image)[2]-1) OR (Frames[2] GT 1) THEN BEGIN
    text = ['Trimming extra frames:','Performing averages:', $
           'Trimming and averaging movie:']
    Print, text[(Frames[2] GT 1) * (1+(Frames[1]-Frames[0] LT (info.S_Image)[2]))]
    SMF_Trim_Movie, info.associated_name, Frames, info.S_Image, $
                    Frame_Format = *info.Image_Template
    ;Update display to show new binary file name, if that changed.  Update the # of
    ;frames actually remaining in the image file and change the related values.
    Info.S_Image[2] = info.analysis.N_Points
    Info.analysis.FrameRange.Start = 0
    Info.analysis.FrameRange.Finish = Info.S_Image[2]
    Info.analysis.FrameRange.Step = 1
    info.movieInfo[0] = StrCompress('Data File: '+File_Basename(*info.Associated_Name))
    Info.MovieInfo[9] = StrCompress('Full Filepath: '+FilePath(*info.Associated_Name))
    ;Widget_Control, info.widgetIDs.MInfoText, Set_Value =  info.MovieInfo
  ENDIF

  ;initialize the 'Pairs' structure.  Then extract intensity
  ;data and store into the structure
  Analyze = ''
  Analyze = SMF_2CH_Construct_Data_Structure_20130731(info, /SaveFile)
  ;Print, Analyze
  ;IF StrUpCase(Analyze) EQ 'NO' THEN BEGIN
  ;      GOTO, Dont_Continue
  ;      Free_Data_Pointers, info
  ;ENDIF
    
  ;Update display and sensitivity of various widgets, also prepare overlay
  ;circles for the movie animation
  print, 'Activate Controls'
  SMF_2CH_Activate_Analysis_Controls_20130731, info, State = 'ON'

  ;Generate a 'Pair Select' event, to produce the first analysis displays
  Widget_control, info.WidgetIDs.PairSelect, Send_Event = {ID:0L, TOP:0L, HANDLER:0L}

  ;Display 'Good Pairs' in the table
  Widget_control, info.WidgetIDs.TableContents, Send_Event = $
          {ID:0L, TOP:0L, HANDLER:0L, UValue: 'List' , Str :'Good Pairs'}

  ;Recenter dislay if it has been resized
  CenterTLB, event.top, 0.5, 0.5

  Dont_Continue: ;The user did not wish to enter analysis mode, so
                 ;leave movie controls active.
  ENDCASE

  'STOP' : BEGIN

  info.Analysis.SelectedPair =0   ;Reset the selected molecule to zero

  SMF_2CH_Pairs_Free_Pointers_20130731, *info.Pairs    ;Free up pointers within the pairs structure

  ;Update display and sensitivity of various widgets
  SMF_2CH_Activate_Analysis_Controls_20130731, info, State = 'OFF'

  ;Update the display text widget, since any further playing with the movie will
  ;be using the trimmed data file, if the user has done any trimming
  info.analysis.frametime = info.analysis.FrameTime * info.analysis.Framerange.step
  Info.S_Image[2] = info.analysis.N_Points
  Info.analysis.FrameRange={Start : 0, Finish: Info.S_Image[2]-1, Step : 1}
  info.movieInfo[0] = StrCompress('Data File: '+File_Basename(*info.Associated_Name))
  info.movieInfo[2] = StrCompress('# of Frames (Total): '+ String(Info.S_Image[2]))
  info.movieInfo[3] = StrCompress('# of Frames (Analyzed): '+String(Info.S_Image[2]))
  info.movieInfo[4] = 'Start Frame: 0'
  Info.MovieInfo[5] = StrCompress('End Frame: ' + String(Info.S_Image[2]) )
  Info.MovieInfo[6] = StrCompress('Frame Time (Seconds per Frame): ' + $
          String(info.analysis.frametime))
  Info.MovieInfo[7] = StrCompress('Time Step (seconds): ' + $
          String(info.analysis.frametime))
  Info.MovieInfo[9] = StrCompress('Full Filepath: '+FilePath(*info.Associated_Name))
  Widget_Control, info.widgetIDs.MInfoText, Set_Value =  info.MovieInfo


  WSET, info.widgetIDs.D1_WID & ERASE ;Clear displays
  WSET, info.widgetIDs.D2_WID & ERASE
  WSET, info.widgetIDs.D3_WID & ERASE
  ENDCASE ;of STOP processing
ENDCASE

Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Processing

;******************************************************************************
;The user can select which autocorrelation calculation to perform and whether to
;use a stretched exponential fit if that is appropriate.  Also, if a linear fit
;is chosen, she can select the offset and the number of points beyond that offset to
;use in the fit
PRO SMF_2CH_GUI_Label_Events, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  Widget_Control, event.id, Get_Value = value
  Widget_Control, event.id, Get_UValue = widget

  Frames =[info.analysis.framerange.start, info.analysis.framerange.finish, $
          info.analysis.framerange.step]

  CASE StrUpCase(widget) OF
    'MOVIES': info.analysis.Load_Movies = 1B - info.analysis.Load_Movies
  ENDCASE

  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Label_Events

;******************************************************************************
;This handler deals with events within the display tab.  The user can choose what
;quantity to display, if any.  The actual display of items it performed
;by a subroutinie called by the 'Analysis' event handler

PRO SMF_2CH_GUI_Display_Tab_Events, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_Value = value
Widget_Control, event.id, Get_UValue = widget

;record the user's choice for the display
IF widget EQ 'Display Choice' THEN info.analysis.Supplemental_Display = event.str

;Clear the display
         WSET, info.widgetIDs.Extra_D
         Erase

Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Display_Tab_Events


;******************************************************************************
;This handler displays the intensity of the two channels as a function of time
;(as wells as the threshhold applied to the signal) in one display, the
;linear dichroism in another display, and the autocorrelation function in the
;third display.  It is initiated by a change in any of the sliders, or by the
; 'Update' button.
PRO SMF_2CH_GUI_Analysis, event
Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, /Hourglass
Widget_Control, event.id, get_UValue=control

Load_Movie = 0 ; initialize load movie flag

;Define the timestamp for each frame
times = (indgen(info.Analysis.N_Points, /float)) * $
         info.analysis.FrameTime*info.analysis.FrameRange.Step

  CASE StrUpCase(control) OF
    'IDDRAW' : BEGIN
      ;the left/right arrow key should decrease/increase the current pair, and the up/down key
      ;should change molecule to good/bad
      Button_Name = ['Good Molecule' , 'Bad Molecule']
      IF event.press EQ 1 THEN BEGIN ;Key pressed (non-ASCII), was it an arrow key?
        CASE event.key OF
          5 : BEGIN ;left arrow - move to next pair
              info.Analysis.SelectedPair -= 1
              if info.Analysis.SelectedPair LT 0 then info.Analysis.SelectedPair = 0
              Widget_control, info.widgetIDs.PairSelect, Set_Value = info.Analysis.SelectedPair
              Widget_Control, info.widgetIDs.Hetero_Button, Set_Value = $
              Button_Name[(*info.Pairs)[info.Analysis.SelectedPair].Hetero]
              ENDCASE
          6 : BEGIN ;right arrow - move to previous pair
              info.Analysis.SelectedPair += 1
              if info.Analysis.SelectedPair GT N_Elements(*info.Pairs)-1 then info.Analysis.SelectedPair = N_Elements(*info.Pairs)-1
              Widget_control, info.widgetIDs.PairSelect, Set_Value = info.Analysis.SelectedPair
              Widget_Control, info.widgetIDs.Hetero_Button, Set_Value = $
              Button_Name[(*info.Pairs)[info.Analysis.SelectedPair].Hetero]
              ENDCASE
          7 : BEGIN ;up arrow - mark as good
              (*info.Pairs)[info.Analysis.SelectedPair].Hetero = 0B
              Widget_Control, info.widgetIDs.Hetero_Button, Set_Value = $
              Button_Name[(*info.Pairs)[info.Analysis.SelectedPair].Hetero]
              ENDCASE
          8 : BEGIN ;down arrow - mark as bad
              (*info.Pairs)[info.Analysis.SelectedPair].Hetero = 1B
              Widget_Control, info.widgetIDs.Hetero_Button, Set_Value = $
              Button_Name[(*info.Pairs)[info.Analysis.SelectedPair].Hetero]
              ENDCASE
          ELSE : ;any other key - do nothing
        ENDCASE; endcase event.key
      ENDIF

      ;threshold is adjusted and saved adjust choice later as appropriate
      ;horizontal thresholding begins here
      Saved_Thresh = ((*info.Pairs)[info.Analysis.SelectedPair]).Threshhold
      Thresh = (Saved_Thresh GT 1) ?  Saved_Thresh :  info.analysis.threshhold;

      I_Tot =*((*info.Pairs)[info.Analysis.SelectedPair]).I_Total
      Thresh_Hist = Histogram(I_Tot, Reverse_Indices=ri, NBins=100)
      Thresh_Limit = max(I_Tot)
      ;Thresh_Limit = max(I_Tot);[ri[Long(100 + 0.95*N_Elements(I_Tot))]] ;95th percentile
      Thresh = Thresh <  Thresh_Limit

      info.analysis.threshhold = Thresh
      (*info.Pairs)[info.Analysis.SelectedPair].Threshhold = Thresh
      Widget_Control, info.widgetIDs.Threshhold, Set_Slider_Max = Thresh_Limit
      Widget_Control, info.widgetIDs.threshhold, Set_Value = Thresh
      ;horizontal thresholding ends here
      ;----------------------------------------------------------------------------
      ;vertical thresholding begins here
      ;Saved_Thresh = ((*info.Pairs)[info.Analysis.SelectedPair]).Threshhold
      ;Thresh = (Saved_Thresh GT 1) ?  Saved_Thresh :  info.analysis.threshhold
      ;I_Tot =*((*info.Pairs)[info.Analysis.SelectedPair]).I_Total
      ;Thresh_Limit = n_elements(I_Tot) * info.analysis.FrameTime
      ;Thresh = Thresh <  Thresh_Limit;
  
      ;info.analysis.threshhold = Thresh
      ;(*info.Pairs)[info.Analysis.SelectedPair].Threshhold = Thresh
      ;Widget_Control, info.widgetIDs.Threshhold, Set_Slider_Max = Thresh_Limit
      ;Widget_Control, info.widgetIDs.threshhold, Set_Value = Thresh   
      ;vertical thresholding ends here        
    ENDCASE


    'THRESHHOLD' : BEGIN
      Widget_Control, event.id, get_Value=dummy
      info.Analysis.Threshhold = dummy
      (*info.Pairs)[info.analysis.SelectedPair].Threshhold = dummy
    ENDCASE ; of THRESHHOLD

    'PAIR' : BEGIN
      ;mark previous pair as done, then set the newly selected pair as active
      Widget_Control, event.id, get_Value=SelectedPair
      info.Analysis.SelectedPair=SelectedPair

      ;Check to see if the user wants to automatically load movies in
      IF info.analysis.Load_Movies  EQ 1 THEN Load_Movie = 1
      info.analysis.Local_Movie = 0 ; Note that the movie hasn't yet been loaded

      ;Load in saved threshhold value if available
      ;Adjust the limits on the threshhold slider, reset threshhold value if too high
      Saved_Thresh = ((*info.Pairs)[SelectedPair]).Threshhold
      Thresh = (Saved_Thresh GT 1) ?  Saved_Thresh :  info.analysis.threshhold
      
      ;horizontal threshold
      I_Tot =*((*info.Pairs)[SelectedPair]).I_Total
      Thresh_Hist = Histogram(I_Tot, Reverse_Indices=ri, NBins=100)
      Thresh_Limit = max(I_Tot);[ri[Long(100 + 0.95*N_Elements(I_Tot))]] ;95th percentile
      Thresh = Thresh <  Thresh_Limit
      ;vertical threshold
      ;I_Tot =*((*info.Pairs)[SelectedPair]).I_Total
      ;Thresh_Limit = n_elements(I_Tot) * info.analysis.FrameTime
      
      info.analysis.threshhold = Thresh
      (*info.Pairs)[SelectedPair].Threshhold = Thresh
      Widget_Control, info.widgetIDs.Threshhold, Set_Slider_Max = Thresh_Limit
      Widget_Control, info.widgetIDs.threshhold, Set_Value = Thresh

      ;Check to see if this molecule was previously marked as experiencing a
      ;change in enevironment
      Button_Name = ['Good Molecule','Bad Molecule']
      Widget_Control, info.widgetIDs.Hetero_Button, Set_Value = $
         Button_Name[(*info.Pairs)[info.Analysis.SelectedPair].Hetero]

      ;Save information into the recovery file
      ;SMF_2CH_Save_Recovery_File_20130731, info
    ENDCASE

    'HETERO' : BEGIN ;this lables pair good or bad
      (*info.Pairs)[info.Analysis.SelectedPair].Hetero = $
         1B - (*info.Pairs)[info.Analysis.SelectedPair].Hetero
      GoodTable = SMF_2CH_GUI_GoodTable_20130731(*info.Pairs)
      Widget_Control, info.widgetIDs.Table, Set_Value = GoodTable
      Button_Name = ['Good Molecule' , 'Bad Molecule']
      Widget_Control, info.widgetIDs.Hetero_Button, Set_Value = $
         Button_Name[(*info.Pairs)[info.Analysis.SelectedPair].Hetero]
    ENDCASE; of "Mark as Heterogeneous"
    
    'LOCAL MOVIE' : BEGIN
      ;Load_Movie = 1
      ;Load_Movie = 1 - Load_Movie
      ;print,'info.analysis.Local_Movie',info.analysis.Local_Movie
      Load_Movie = 1 - info.analysis.Local_Movie
      info.analysis.Local_Movie = Load_Movie
      Button_Name = ['Load Local Movie' , 'Show Local Image']
      Widget_Control, info.widgetIDs.LoadMovie, Set_Value = $
                      Button_Name[Load_Movie]
      IF Load_Movie EQ 1 THEN BEGIN
        ;change sensitivities of widgets
        ;bc while viewing movie....you must stay on this molecule!
        Widget_Control, info.widgetIDs.MovieFrame, sensitive=1, Set_Slider_Max = info.S_image[2]-1
        Widget_Control, info.widgetIDs.MovieSpeed, sensitive=1
        Widget_Control, info.widgetIDs.AnimateButton, sensitive=1
        Widget_Control, info.widgetIDs.StopButton, sensitive=1
        Widget_Control, info.widgetIDs.PairSelect, sensitive=0
        Widget_Control, info.widgetIDs.Threshhold, sensitive=0
        Widget_Control, info.widgetIDs.PairID_Display,sensitive=0
        ;open the movie file and store the info.AnalysisInfo
        OpenR, lun, *info.associated_Name, /get_lun
        image = Assoc(lun, *info.Image_Template)
        *info.movie_images = image
        *info.movie_lun = lun
        ENDIF ELSE BEGIN
        ;go back to summed images mode by reversing sensitivity changes
        info.Analysis.Mstopflag = 1
        Widget_Control, info.widgetIDs.MovieFrame, sensitive=0
        Widget_Control, info.widgetIDs.MovieSpeed, sensitive=0
        Widget_Control, info.widgetIDs.AnimateButton, sensitive=0
        Widget_Control, info.widgetIDs.StopButton, sensitive=0
        Widget_Control, info.widgetIDs.PairSelect, sensitive=1
        Widget_Control, info.widgetIDs.Threshhold, sensitive=1
        Widget_Control, info.widgetIDs.PairID_Display,sensitive=1
        ;dissociate the movie file
        Close, *info.movie_lun
        Free_lun, *info.movie_lun
      ENDELSE
    ENDCASE
    
    'UPDATE' :  BEGIN
      ;something goes here
    ENDCASE
    
    'INTLIMIT':BEGIN
      Widget_Control, info.widgetIDs.intxstartslider, get_Value=info.intxstart
      Widget_Control, info.widgetIDs.intxendslider, get_Value=info.intxend
      Widget_Control, info.widgetIDs.intystartslider, get_Value=info.intystart
      Widget_Control, info.widgetIDs.intyendslider, get_Value=info.intyend
    ENDCASE
   
    'AUTOPICK' : BEGIN
      print, 'Refining molecules by normalized statistical arguments...'
      print, '...input autopick parameter. Consult group reference.'
      auto_pick = txt_box(label='Set Autopicking Parameter', cancel=cncld, $
                  group_leader=event.top, value=0.0)
      IF cncld THEN GOTO, NOAUTOPICK
      print, '...those previously marked bad are kept as such.'
      Button_Name = ['Good Molecule' , 'Bad Molecule']
      w = where((*info.Pairs)[*].Hetero EQ 1B, pbad, complement=wgd, ncomplement=pgood);previously marked bad
      ;FOR z=0, n_elements((*info.Pairs)[*])-1 DO BEGIN
      IF pgood GT 0 THEN BEGIN
        FOR i=0, pgood-1 DO BEGIN
          b_fac = *(*info.Pairs)[wgd(i)].back_l - $
                  (*info.Pairs)[wgd(i)].g_fac * *(*info.Pairs)[wgd(i)].back_r
          IF stddev(b_fac) GT auto_pick THEN (*info.Pairs)[*].Hetero = 1B
        ENDFOR
      ENDIF
      b_fac = 0
      w = where((*info.Pairs)[*].Hetero EQ 1B, badmarks)
      print, '...', string(badmarks), ' molecules were deemed bad.'
      print, '...', string(badmarks - pbad), ' of which were not previously so'
      NOAUTOPICK:
      print, 'END AUTOPICKING'
    ENDCASE
    
    ELSE: print, 'no case found'
    ;Run the rest of the code to update the display
  ENDCASE; ENDCASE OF ENTIRE CASE LOOP

  ;Clear the display then check if an ACF has been cacluated already; if so, display it
  ;First, find the maximum display time
  ;Widget_Control, info.WidgetIDs.ACF_Display_Max, Get_Value = Max_Display_Time
  WSET, info.widgetIDs.D1_WID
  Erase

  ;----------------------------------------------------------------------------
  ;Check to see if the user wants to load in a local movie, if so load it.
  IF Load_Movie EQ 1 THEN BEGIN
    info.analysis.Local_Movie = 1 ; Mark movie as loaded

    ;Pull out the local 'movies' of the spots and blow up for animation,
    ;and output the coordinates of the pair to the animation labels.
    ;Also, define the coordiniates of the overlay circles
    CurrentPair = (*info.Pairs)[info.Analysis.SelectedPair]
    CurrentPair.FinishedAnalysis = 1
    Coordinates= {X1:String(Fix(CurrentPair.X1)), Y1:String(Fix(CurrentPair.Y1)), $
      X2:String(Fix(CurrentPair.X2)), Y2:String(Fix(CurrentPair.Y2))}
    Widget_Control, info.widgetIDs.LH_Coords, $
      Set_Value=StrCompress('('+Coordinates.X1+','+Coordinates.Y1+')', /Remove_All)
    Widget_Control, info.widgetIDs.RH_Coords, $
      Set_Value=StrCompress('('+Coordinates.X2+','+Coordinates.Y2+')', /Remove_All)

    ;Initialize the PCircle and BCircle masks for the animation display
    P_Offsets=[CurrentPair.X1 - Fix(CurrentPair.X1), CurrentPair.Y1 - $
      Fix(CurrentPair.Y1), CurrentPair.X2 - Fix(CurrentPair.X2), $
      CurrentPair.Y2 - Fix(CurrentPair.Y2)]

    FOR i=0, 1 DO BEGIN
      PCircleCoords = SMF_Mask_Circle(11.0*info.Analysis.AveragingParams.PSize/2, $
        offset=P_offsets[2*i:2*i+1], thickness = 1.0)
      BCircleCoords = SMF_Mask_Circle(11.0*info.Analysis.AveragingParams.BSize/2, $
        offset=P_offsets[2*i:2*i+1], thickness = 1.5)

      ;Translate the coordinates to indices of the movie 3-D array
      N_P = N_Elements(PCircleCoords.XPoints) ; How many points in inner circle?
      N_B = N_Elements(BCircleCoords.XPoints) ; How many points in outer circle?

      ;Overlay circles on movie frames
      IF i EQ 0 THEN BEGIN
        ;(*info.Analysis.M_LH)[Particle] = info.top +1
        ;(*info.Analysis.M_LH)[Background] = info.top
      ENDIF ELSE BEGIN
        ;(*info.Analysis.M_RH)[Particle] = info.top +1
        ;(*info.Analysis.M_RH)[Background] = info.top
      ENDELSE
    ENDFOR; of circle masks
    ENDIF ELSE BEGIN; of loading localmovie

    ;if no movie is loaded, then simply display local image from the stored
    ;summed image
    CurrentPair = (*info.Pairs)[info.Analysis.SelectedPair]
    CurrentPair.FinishedAnalysis = 1
   
    Coordinates= {X1:String(Fix(CurrentPair.X1)), Y1:String(Fix(CurrentPair.Y1)), $
      X2:String(Fix(CurrentPair.X2)), Y2:String(Fix(CurrentPair.Y2))}
    Widget_Control, info.widgetIDs.LH_Coords, $
      Set_Value=StrCompress('('+Coordinates.X1+','+Coordinates.Y1+')', /Remove_All)
    Widget_Control, info.widgetIDs.RH_Coords, $
      Set_Value=StrCompress('('+Coordinates.X2+','+Coordinates.Y2+')', /Remove_All)
  
    PairCoord = fltarr(4)
      PairCoord[0] = CurrentPair.X1
      PairCoord[1] = CurrentPair.Y1
      PairCoord[2] = CurrentPair.X2
      PairCoord[3] = CurrentPair.Y2

    Padding_LH = [Round(6-PairCoord[0]) > 0 , Round(6-PairCoord[1]) > 0 , $
             (Round(PairCoord[1])-info.S_Image[1] + 7) > 0 ]

    WSET, info.widgetIDs.LH_WID
    LH_Local_Image = BytArr(13,13) ;Create zeroed array for local image
    LH_Local_Image[Padding_LH[0]:12, Padding_LH[1]:(12-Padding_LH[2])] = $
       (*info.summedimage)[(0 > (Round(PairCoord[0]) - 6)) : Round(PairCoord[0]) + 6, $
        (0 > (Round(PairCoord[1]) - 6)) : (info.S_Image[1]-1 < (Round(PairCoord[1]) + 6))]
    LH_Image = BytScl(Smooth(Congrid(LH_Local_Image, 143,143), 11), $
           Top= info.top -1);Max = Brightness,
    Circle_4 = SMF_Mask_Circle(22, offset = [PairCoord[0]-Round(PairCoord[0]),PairCoord[1]-Round(PairCoord[1])])
    Circle_8 = SMF_Mask_Circle(44, offset = [PairCoord[0]-Round(PairCoord[0]),PairCoord[1]-Round(PairCoord[1])])
    Circle_12 = SMF_Mask_Circle(66, offset = [PairCoord[0]-Round(PairCoord[0]),PairCoord[1]-Round(PairCoord[1])])
    LH_Image[Long(Circle_4.XPoints+71) + Long(Circle_4.YPoints+71) * 143] = info.top+1
    LH_Image[Long(Circle_8.XPoints+71) + Long(Circle_8.YPoints+71) * 143] = info.top+2
    LH_Image[Long(Circle_12.XPoints+71) + Long(Circle_12.YPoints+71) * 143] = info.top
    TV, LH_Image

    ;Padding = # of zero vectors on the [Right, Bottom, Top]
    Padding_RH = [Round(PairCoord[2]-info.S_Image[0] + 7) > 0 , $
             Round(6-PairCoord[3]) > 0 , Round(PairCoord[3]-info.S_Image[1] + 7) > 0 ]

    WSET, info.widgetIDs.RH_WID
    RH_Local_Image = BytArr(13,13) ;Create zeroed array for local image
    RH_Local_Image[0:(12-Padding_RH[0]), Padding_RH[1]:(12-Padding_RH[2])] = $
       (*info.summedimage)[Round(PairCoord[2]) - 6 : (info.S_Image[0]-1 < (Round(PairCoord[2]) + 6)), $
        (0 > (Round(PairCoord[3]) - 6)) : (info.S_Image[1]-1 < (Round(PairCoord[3]) + 6))]
    RH_Image = BytScl(Smooth(Congrid(RH_Local_Image, 143,143), 11), $
           Top= info.top -1);Max = Brightness,
    Circle_4 = SMF_Mask_Circle(22, offset = [PairCoord[2]-Round(PairCoord[2]),PairCoord[3]-Round(PairCoord[3])])
    Circle_8 = SMF_Mask_Circle(44, offset = [PairCoord[2]-Round(PairCoord[2]),PairCoord[3]-Round(PairCoord[3])])
    Circle_12 = SMF_Mask_Circle(66, offset = [PairCoord[2]-Round(PairCoord[2]),PairCoord[3]-Round(PairCoord[3])])
    RH_Image[Long(Circle_4.XPoints+71) + Long(Circle_4.YPoints+71) * 143] = info.top+1
    RH_Image[Long(Circle_8.XPoints+71) + Long(Circle_8.YPoints+71) * 143] = info.top+2
    RH_Image[Long(Circle_12.XPoints+71) + Long(Circle_12.YPoints+71) * 143] = info.top
    TV, RH_Image
  ENDELSE
  ;----------------------------------------------------------------------------


  ;Check to see whether a local movie is loaded; if not, blank out animation
  ;controls, but if there is, make them visible
  ;IF info.analysis.Local_Movie EQ 1 THEN  Widget_Control, $
  ;   info.widgetIDs.MovieBase, Map=1 ELSE Widget_Control, $
  ;   info.widgetIDs.MovieBase, Map=1
  Widget_Control, info.widgetIDs.MovieBASE, Map=1

  ;Pull out the data relevant to the selected fluorophore
  CurrentPair = (*info.Pairs)[info.Analysis.SelectedPair]
  Coordinates = [CurrentPair.X1, CurrentPair.Y1, CurrentPair.X2, CurrentPair.Y2]

  ;Display Background Intensity information B//-gBT
  ;WSET, info.widgetIDs.D1_WID
  ;plot, times, (*CurrentPair.back_l - CurrentPair.g_fac * *CurrentPair.back_r), $
  ;  XTITLE='Time (seconds)', YTITLE='Background Information', color = info.top-5

  ;Calculate the linear dichroism, record it, and display in the middle window
  WSET, info.widgetIDs.D2_WID
  Results = SMF_2CH_Plot_Linear_Dichroism_20130731(times, $
            *CurrentPair.I_Total, *CurrentPair.I_Left, *CurrentPair.I_Right, $
            info.Analysis.Threshhold, *CurrentPair.idealtrace, $
            Finish=info.analysis.AC_Fit_End, Start=info.analysis.AC_Fit_Start, $
            color=info.top)
  *CurrentPair.LinearDichroism = Results.LD
  *CurrentPair.LD_Raw = Results.LD_Raw
  CurrentPair.Mean_LD = Results.Mean_LD

  ;Plot the intensities in the bottom window; record the threshhold used
  WSET, info.widgetIDs.D3_WID
  SMF_2CH_Plot_20130731, times, *CurrentPair.I_Total, *CurrentPair.I_Left, *CurrentPair.I_Right,$
    info.Analysis.Threshhold, *CurrentPair.idealtrace, color = info.top
  CurrentPair.Threshhold = info.Analysis.Threshhold

  ;Calculate and display extra info into the tabbed display window
  WSET, info.widgetIDs.Extra_D
  CASE StrUpCase(info.analysis.Supplemental_Display) OF
    'NOTHING' : ;Do nothing
    'LD HISTOGRAM' : BEGIN
      Data = Reform(*CurrentPair.LinearDichroism, N_Elements(*CurrentPair.LinearDichroism))
      Finite_Data = Where(Finite(Data), count)
      IF count GE 10 THEN toby_hist_plot, Data[Finite_Data], $
        Min = -2, Max = 2, Binsize = 0.05, /normalize, color = info.top-5
      Mean_LD = Mean(Data[Finite_Data])
      Oplot, [Mean_LD,Mean_LD], [0,1], color = info.top
      ;Display the average value below the graph
      Out_Str1 = StrCompress('Mean linear dichroism: ' + $
                 String(Mean_LD, Format = '(F6.3)'))
      Out_Str2 = StrCompress('Number of linear dichroism points used: ' + $
                 String(count, Format = '(I5)'))
      Widget_Control, info.WidgetIDs.Extra_D_Text, Set_Value=[Out_Str1, Out_Str2]
    ENDCASE
    'STDDEV d_Background' : BEGIN
      plot, times, (*CurrentPair.back_l - CurrentPair.g_fac * *CurrentPair.back_r), $
      XTITLE='Time (seconds)', YTITLE='Background Information', color = info.top-5
    ENDCASE
    'POWER SPECTRUM' : BEGIN ;WRITE THIS LATER
      Data = Reform(*CurrentPair.Autocorrelation, N_Elements(*CurrentPair.Autocorrelation))
         SMF_Power_Spectrum, Data, info.analysis.Frametime
    ENDCASE
    ELSE : Print, 'Tab_Display choice caused an error'
  ENDCASE

  ;Always update the display table incase anything has changed
  GoodTable = SMF_2CH_GUI_GoodTable_20130731(*info.Pairs)

  Widget_Control, info.widgetIDs.Table, Set_Value = GoodTable
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END; of SMF_GUI_Analysis


;******************************************************************************


;******************************************************************************
;This handler deals with events generated by the 'Computer' menu
;It is essentially a menu for printing and outputting data
PRO SMF_2CH_GUI_ComputerMenu, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  Widget_Control, event.id, Get_UValue = widget

  CASE StrUpCase(widget) OF
    'PRINT PLOTS' : BEGIN
             CurrentPair = (*info.Pairs)[info.Analysis.SelectedPair]
             SMF_GUI_Print_Plots, CurrentPair, Frametime=info.analysis.frametime, $
                THRESHHOLD=info.Analysis.threshhold, $;Displayed on intensity plot
                FILENAME=*info.Associated_Name, Method = info.analysis.AC_Technique
             (*info.Pairs)[info.Analysis.SelectedPair].Printed = 1
             GoodTable = SMF_2CH_GUI_GoodTable_20130731(*info.Pairs)
             Widget_Control, info.widgetIDs.Table, Set_Value = GoodTable
    ENDCASE
    ;'PRINT INFO'        : SMF_Print_Summary, info
    ;'OUTPUT DATA'       : SMF_File_Output, info
    ;'OUTPUT LD'         : SMF_Save_Linear_Dichroism_Movie, info
    'SAVE ANALYSIS'     : SMF_2CH_save_analysis_file_20130731, info
    ELSE : a = dialog_Message('Oops -- there was a programming error. ' + $
              'Contact someone, I guess.')
  ENDCASE

  Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; of SMF_GUI_ComputerMenu


;******************************************************************************
;Event handler for the animation widget.  This takes the local
;image information loaded into the info structure and displays it
;as an animation, with circles to represent the areas used to
;integrate the signal and background intensities.  A 'Timer' event
;is used to run the animation, and all events (sliders, button,
;and timer) and dealt with by this event handler.
PRO SMF_2CH_GUI_Movie, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy

  eventName = TAG_NAMES(event, /STRUCTURE_NAME); What kind of event is this?
  image = *info.movie_images

  ;Get info about the currently selected pair of spots and circles for overlay
  CurrentPair = (*info.Pairs)[info.Analysis.SelectedPair]
  PairCoord = fltarr(4)
    PairCoord[0] = CurrentPair.X1
    PairCoord[1] = CurrentPair.Y1
    PairCoord[2] = CurrentPair.X2
    PairCoord[3] = CurrentPair.Y2

  feat_l = PairCoord[0:1]
  feat_r = PairCoord[2:3]

  IF eventName EQ 'WIDGET_SLIDER' THEN BEGIN   ; Code for slider events.
    Widget_Control, event.id, Get_UValue = slider ; which slider was moved?
    CASE StrUpCase(slider) OF
      'FRAME' : BEGIN
        info.Analysis.MFrame = event.value

        Padding_LH = float([Round(6 - feat_l[0]) > 0 , Round(feat_l[0] - $
          info.S_image[0] + 7) > 0 , Round(6 - feat_l[1]) > 0 , (Round(feat_l[1]) -$
          info.S_image[1] + 7) > 0 ])
        LH_Local_Image = Intarr(13,13)
        LH_Local_Image[Padding_LH[0]:(12 - Padding_LH[1]) , Padding_LH[2]:(12 - $
          Padding_LH[3])] = (image[info.analysis.mFrame])[(0 > (Round(feat_l[0]) - $
          6)) : (info.S_Image[0]-1 < Round(feat_l[0]) + 6), (0 > (Round(feat_l[1]) - $
          6)) : (info.S_Image[1]-1 < (Round(feat_l[1]) + 6))]
        LH_Local_Image = Smooth(Congrid(LH_Local_Image, 143,143), 11)

        Padding_RH = [Round(feat_r[0] - info.S_Image[0] + 9) > 0 , $
          Round(8 - feat_r[1]) > 0 , Round(feat_r[1] - info.S_Image[1] + 9) > 0]
        RH_Local_Image = IntArr(17,17)
        RH_Local_Image[0:(16-Padding_RH[0]), Padding_RH[1]:(16-Padding_RH[2])] = $
          (image[info.analysis.mFrame])[Round(feat_r[0]) - 8 : $
          (info.S_Image[0] - 1 < (Round(feat_r[0]) + 8)), $
          (0 > (Round(feat_r[1]) - 8)) : (info.S_Image[1]-1 < (Round(feat_r[1]) + 8))]
        RH_Local_Image = Smooth(Congrid(RH_Local_Image, 143,143), 11)
          
        WSET, info.widgetIDs.LH_WID
        TVSCL, LH_Local_Image, top=4000
        WSET, info.widgetIDs.RH_WID
        TVSCL, RH_Local_Image, top=4000
      ENDCASE
      ;Max delay 0.5 seconds.
      'SPEED' : BEGIN
        info.Analysis.MDelay = 1 - (ALog10(float(event.Value+1))/4)
      ENDCASE
    ENDCASE
    WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
    RETURN ; Exit from this event code without animating
  ENDIF

  ;Response to a BUTTON event...
  IF eventName EQ 'WIDGET_BUTTON' THEN BEGIN   ; Code for BUTTON events.
    Widget_Control, event.id, Get_UValue = button
    CASE StrUpCase(button) OF
      'ANIMATE' : BEGIN
        ;If stop flag is 0, then display next frame.
        IF info.Analysis.Mstopflag EQ 0 THEN BEGIN
         Padding_LH = float([Round(6 - feat_l[0]) > 0 , Round(feat_l[0] - $
           info.S_image[0] + 7) > 0 , Round(6 - feat_l[1]) > 0 , (Round(feat_l[1]) - $
           info.S_image[1] + 7) > 0 ])
        LH_Local_Image = Intarr(13,13)
        LH_Local_Image[Padding_LH[0]:(12 - Padding_LH[1]) , Padding_LH[2]:(12 - $
          Padding_LH[3])] = (image[info.analysis.mFrame])[(0 > (Round(feat_l[0]) - $
          6)) : (info.S_Image[0]-1 < Round(feat_l[0]) + 6), (0 > (Round(feat_l[1]) - $
          6)) : (info.S_Image[1]-1 < (Round(feat_l[1]) + 6))]
        LH_Local_Image = Smooth(Congrid(LH_Local_Image, 143,143), 11)

        Padding_RH = [Round(feat_r[0] - info.S_Image[0] + 9) > 0 , $
          Round(8 - feat_r[1]) > 0 , Round(feat_r[1] - info.S_Image[1] + 9) > 0]
        RH_Local_Image = IntArr(17,17)
        RH_Local_Image[0:(16-Padding_RH[0]), Padding_RH[1]:(16-Padding_RH[2])] = $
          (image[info.analysis.mFrame])[Round(feat_r[0]) - 8 : $
          (info.S_Image[0] - 1 < (Round(feat_r[0]) + 8)), $
          (0 > (Round(feat_r[1]) - 8)) : (info.S_Image[1]-1 < (Round(feat_r[1]) + 8))]
        RH_Local_Image = Smooth(Congrid(RH_Local_Image, 143,143), 11)

        WSET, info.widgetIDs.LH_WID 
        TVSCL, LH_Local_Image, top=4000
        WSET, info.widgetIDs.RH_WID 
        TVSCL, RH_Local_Image, top=4000
        ENDIF
        ;Update frame number. If end is reached, start over!
        info.Analysis.MFrame = info.Analysis.MFrame + 1
        IF info.Analysis.MFrame GT info.S_image[2]-1 THEN info.Analysis.MFrame = 0
        ;Set a timer event to get back into this event handler.
        WIDGET_CONTROL, event.ID, TIMER=0
        info.Analysis.Mstopflag = 0   ; Update stop flag.
      ENDCASE ; of ANIMATE button CASE.
      'STOP': BEGIN
        info.Analysis.Mstopflag = 1
        info.Analysis.MFrame = info.Analysis.MFrame - 1  ; Subtract a frame.
      ENDCASE ; of STOP button CASE
    ENDCASE; of "Widget_Button" CASE
    WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
    RETURN ; Exit from this event code
  ENDIF ; of BUTTON code

  ;Response to a TIMER event...
  IF eventName EQ 'WIDGET_TIMER' THEN BEGIN
    IF info.Analysis.Mstopflag EQ 0 THEN BEGIN
      Padding_LH = float([Round(6 - feat_l[0]) > 0 , Round(feat_l[0] - $
        info.S_image[0] + 7) > 0 , Round(6 - feat_l[1]) > 0 , (Round(feat_l[1]) - $
        info.S_image[1] + 7) > 0 ])
      LH_Local_Image = Intarr(13,13)
      LH_Local_Image[Padding_LH[0]:(12 - Padding_LH[1]) , Padding_LH[2]:(12 - $
        Padding_LH[3])] = (image[info.analysis.mFrame])[(0 > (Round(feat_l[0]) - $
        6)) : (info.S_Image[0]-1 < Round(feat_l[0]) + 6), (0 > (Round(feat_l[1]) - $
        6)) : (info.S_Image[1]-1 < (Round(feat_l[1]) + 6))]
        LH_Local_Image = Smooth(Congrid(LH_Local_Image, 143,143), 11)

      Padding_RH = [Round(feat_r[0] - info.S_Image[0] + 9) > 0 , $
        Round(8 - feat_r[1]) > 0 , Round(feat_r[1] - info.S_Image[1] + 9) > 0]
      RH_Local_Image = IntArr(17,17)
      RH_Local_Image[0:(16-Padding_RH[0]), Padding_RH[1]:(16-Padding_RH[2])] = $
        (image[info.analysis.mFrame])[Round(feat_r[0]) - 8 : $
        (info.S_Image[0] - 1 < (Round(feat_r[0]) + 8)), $
        (0 > (Round(feat_r[1]) - 8)) : (info.S_Image[1]-1 < (Round(feat_r[1]) + 8))]
      RH_Local_Image = Smooth(Congrid(RH_Local_Image, 143,143), 11)

      Wait, info.Analysis.Mdelay  ; Delay for a moment.
      WSET, info.widgetIDs.LH_WID ; Plot left hand side;
      TVSCL, LH_Local_Image
      WSET, info.widgetIDs.RH_WID ; Repeat for right hand side;
      TVSCL, RH_Local_Image
      ;Update frame slide
      Widget_Control, info.widgetIDs.MovieFrame, Set_Value = info.Analysis.MFrame

      info.Analysis.MFrame = info.Analysis.MFrame + 1 ; Update frame number.
      IF info.Analysis.MFrame GT (info.S_image[2] - 1) THEN info.Analysis.MFrame = 0
      ; Set a timer event to get back into this event handler.
      IF info.Analysis.Mstopflag EQ 0 THEN WIDGET_CONTROL, event.ID, TIMER=0
    ENDIF
    ;WIDGET_Control, info.widgetIDs.MovieFrame, Set_Value = info.Analysis.MFrame
    WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
  ENDIF ; of TIMER code

END ; of SMF_GUI_Movie


;******************************************************************************
;Event handler for the 'Quit" option within the 'File' menu
PRO SMF_2CH_GUI_Quit, event
  Widget_Control, event.top, /Destroy
END

;************************************
; Cleanup procedure to deal with pointers in the info structure
PRO SMF_2CH_GUI_Cleanup, tlb
  Widget_Control, tlb, Get_UValue=info, /No_Copy

  ;Restore original device display settings
  Device, Decomposed = info.Device_Settings.Decomposed
  r= info.Device_Settings.r
  g= info.Device_Settings.g
  b= info.Device_Settings.b

  TVLCT, r, g, b

  IF N_Elements(info) EQ 0 THEN RETURN ELSE BEGIN
    SMF_2CH_Free_Data_Pointers_20130731, info  ;For each pair free up the pointers

    Ptr_Free, info.Associated_Name
    Ptr_Free, info.summedImage
    Ptr_Free, info.Image_Template
    Ptr_Free, info.BPimage
    Ptr_Free, info.Features
    Ptr_Free, info.pairs
    Ptr_Free, info.PairsTable
    Ptr_Free, info.analysis.M_LH
    Ptr_Free, info.analysis.M_RH
    Ptr_Free, info.analysis.MCircles
  ENDELSE
END

;******************************************************************************
;******************************************************************************

PRO SMF_2CH_GUI, $                          ;The program name
    F_Coords = F_Coords, $             ;Coordinates of fluorophores
    Tab_Display = Tab_Display       ;Displays are tiled by default, set to make tabbed


  ;Error handler to catch any errors within SMF_GUI program
  Catch, theError
  IF theError NE 0 THEN BEGIN
    catch, /cancel
    ok = Error_Message(Traceback=1)
    RETURN
  ENDIF

  ;Checking for positional and keyword parameters, defining where it is necessary
  ;image=LoadNileRed() ; if no image supplied, display filler
  image=Indgen(100,100,100) ; if no image supplied, Initialize a zero array
  ndim=Size(image, /N_Dimensions)
  IF ndim NE 3 THEN $
    Message, 'A multi-frame movie is Required.', /NoName
  IF N_Elements(F_Coords) EQ 0 THEN F_Coords = Replicate(0.0,7)



;Start defining widgets

;Top level base widget.  Note that the 'MBar' keyword creates _output_
tlb = Widget_Base(Column=2, TLB_Size_Events=1, MBar=menubarID, $
      Title='Kaufman SMF_2CH_GUI: 2-Channel Data Extraction')


          ;******** MENUBAR STARTS ***************
;Put some buttons in the menubar of the top level base
;Start with a file menu
fileID = Widget_Button(menubarID, Value='File', /Menu)

;Define the Open menu.
openID = Widget_Button(fileID, Value='Load Movie', Event_Pro='SMF_2CH_GUI_Load_Movie', /Menu)
button = Widget_Button(openID, Value='Raw binary File', UValue='.bin')
button = Widget_Button(openID, Value='TIFF File', UValue='.tif')
;button = Widget_Button(openID, Value='legacy dat File', UValue='.dat')

;Define a 'Fix sample drift' button
buttonFD = Widget_Button(fileID, Value = 'Fix Sample Drift', Sensitive=0, $
      Event_Pro='SMF_2CH_GUI_Drift')

;Define a 'Recover from crash...' button
;button = Widget_Button(fileID, UValue = 'BINARY', $
;    Value = 'Recover from crash... (Binary file)', Event_Pro='SMF_2CH_GUI_Recover')
;button = Widget_Button(fileID, UValue = 'LEGACY', $
;    Value = 'Recover from crash... (Legacy DAT file)', Event_Pro='SMF_GUI_Recover')

;Define a quit button
quitID= Widget_Button(fileID, Value='Quit', Event_Pro='SMF_2CH_GUI_Quit')

;Next add a processing menu
iprocessID = Widget_Button(MenubarID, Value='Finding Features', /Menu, Sensitive=0)
button = Widget_Button(iprocessID, Value='Sum Images', UValue='Sum', $
         Event_Pro='SMF_2CH_GUI_Sum_Frames')
buttonBP = Widget_Button(iprocessID, Value='Band Pass', UValue='BPass', $
         Event_Pro='SMF_2CH_GUI_BPass')
buttonFF = Widget_Button(iprocessID, Value='Find Fluorophores', UValue='Feature', $
         Event_Pro='SMF_2CH_GUI_Feature')
button = Widget_Button(iprocessID, Value='Set Channel Separation', $
         UValue='Separation', Event_Pro='SMF_2CH_GUI_Channel_Separation')
buttonReg = Widget_Button(iprocessID, Value='Register Channels', UValue='Register', $
         Event_Pro='SMF_2CH_GUI_Register', Sensitive = 0)
buttonChoose = Widget_Button(iprocessID, Value='Select Fluorophores', UValue='GoodPairs',$
         Event_Pro='SMF_2CH_GUI_GoodPairs', Sensitive = 0)


;A menu to allow control over parameters that affect how the data will be processed
dprocessID = Widget_Button(MenubarID, Value='Data Processing Parameters', /Menu, $
                   Sensitive=0)
button = Widget_Button(dprocessID, Value='Set Averaging Parameters', $
      UValue='Averaging Params',   Event_Pro='SMF_2CH_GUI_Set_Averaging_Params')
button = Widget_Button(dprocessID, Value='Adjust Movie Parameters', UValue='Range', $
         Event_Pro='SMF_2CH_GUI_Movie_Frames')


;A menu to initiate, or conclude, data analysis
AnalysisMenuID = Widget_Button(MenubarID, Value='Analysis', /Menu, $
                   Sensitive = 0, Event_Pro='SMF_2CH_GUI_Processing')
button = Widget_Button(AnalysisMenuID, Value='Start Processing', UValue='Start')
button = Widget_Button(AnalysisMenuID, Value='Stop Processing', UValue='Stop')


;A menu for local settings to do with the computer
computerID = Widget_button(MenubarID, Value = 'Computer Commands', /Menu, $
              Sensitive = 0, Event_Pro = 'SMF_2CH_GUI_ComputerMenu')
;button = Widget_Button(computerID, Value = 'Print current graphs...', UValue = 'Print Plots')
;button = Widget_Button(computerID, Value = 'Print Analysis Info...', UValue = 'Print Info')
;button = Widget_Button(computerID, Value = 'Output Data...', UValue = 'Output Data')
button = Widget_Button(computerID, Value = 'Save all analysis data...', UValue = 'Save Analysis')
;button = Widget_Button(computerID, Value = 'Output LD Movie', UValue = 'Output LD')
;                    ********MENUBAR ENDS**************
;******************************************************************************

;Create separate bases for the left and right hand sides
LHB = Widget_Base(tlb, column=1)
RHB = Widget_Base(tlb, column=1)

;Create a Tab widget, to allow user to view either the feature coordinates
;or general information about the imported movie
TabID = Widget_Tab(LHB, xsize=400, ysize=350)

;Create a table widget to display the coordinates of the fluorophores found by
;the feature algorithm
fluorophoreID = Widget_Base(TabID, Column=1, Event_Pro='SMF_2CH_GUI_TableDisplay', $
              Title = 'Features Table', Frame =2)
comboBox = Widget_ComboBox(fluorophoreID,  UValue = 'List',$
          YSize=20, Frame=4, /Dynamic_Resize)
columnLabels =['X1', 'Y1', 'Bright', '']
Col_Width = [40,40,40,40,40,40,40,40,40,40,40,40]
Table_Format = ['(F6.1)', '(F6.1)','(F6.1)','(F6.1)','(I1)','(I1)','(I1)','(I1)','(I1)','(I1)','(I1)','(I1)','(I1)']
FlurophoretableID= Widget_Table(fluorophoreID, XSize=12, Column_Width = Col_Width, $
  UValue='Table', Column_Labels=columnLabels, /Resizeable_Columns, Format=Table_Format, $
  X_Scroll_Size=8, Y_Scroll_Size = 12, /scroll)

;Create a tab widget with a text widget to display movie info
InfoTextBase = Widget_Base(TabID, Title = 'Movie Info')
MInfoTextID = Widget_Text(InfoTextBase, XSize = 50, YSize = 20)

;Create a tabwidget with a text widget to display Analysis info
AnalysisTextBase = Widget_Base(TabID, Title = 'Analysis Info (Text)', $
         column=1,  Event_Pro = 'SMF_2CH_GUI_Label_Events')
AInfoTextID = Widget_Text(AnalysisTextBase, XSize = 50, YSize = 10)


;Widget to control the conditions for recalculating and displaying analysis
ACF_Display_Choice_Base = Widget_Base(AnalysisTextBase, Frame = 1, row=1, /nonexclusive)
;Movie_Button = Widget_Button(ACF_Display_Choice_Base, Value = 'Autoload Movies', $
;         UValue = 'Movies')


;Create a tab widget with an extra display location for analysis
DisplayTabBase = Widget_Base(TabID, Title = 'Display Window', column = 1, $
              Event_Pro = 'SMF_2CH_GUI_Display_Tab_Events')
Tab_Display_ID = Widget_Draw(DisplayTabBase, xsize=350, ysize=200, frame=2)
ControlsBase = Widget_Base(DisplayTabBase, row = 1, frame = 1)
;Display_Choices = ['Nothing', 'LD Histogram', 'Power Spectrum']
Display_Choices = ['Nothing'];, 'LD Histogram', 'STDDEV d_Background', 'Power Spectrum']
Display_ComboBox = Widget_Combobox(ControlsBase, Value=Display_Choices, $
         UValue='Display Choice' )
Display_Output = Widget_Text(DisplayTabBase, XSize = 50, YSize = 5)

;Create a series of sliders that allows the user to adjust analysis parameters
AnalysisID = Widget_Base(LHB, Row=3, Event_Pro = 'SMF_2CH_GUI_Analysis', $
                   /Base_Align_Center, Frame = 2, map=0)
AnalysisTopBase = Widget_Base(AnalysisID, row = 1)

AnalysisTopButtonBase = Widget_Base(AnalysisTopBase, column = 1)
;button = Widget_button(AnalysisTopButtonBase, Value = 'Update ACF', UValue='Update')
Hetero_Button = Widget_Button(AnalysisTopButtonBase, Value = 'Bad Molecule', UValue='Hetero')
AutoPick_Button = Widget_Button(AnalysisTopButtonBase, Value = 'Autopick', UValue='Autopick')
MLoadButton = Widget_Button(AnalysisTopButtonBase, Value = 'Load Local Movie', UValue='Local Movie')
PairID_Display = Widget_Draw(AnalysisTopBase, xsize=20, ysize=20, /keyboard_Events, UVALUE = 'IDDRAW', Event_PRO = 'SMF_2CH_GUI_Analysis')
PairID = Widget_Slider(AnalysisTopBase, Title = 'Selected Fluorophore', UValue = 'Pair')
threshholdID = Widget_Slider(AnalysisTopBase, Title = 'Threshhold Intensity', $
            UValue = 'Threshhold', Scroll = 250)

;Create an area to show the time behavior of a single fluorophore
MovieBase = Widget_Base(AnalysisID, row=1, /Base_Align_Center, $
           Frame =2)
    ;Lefthand Display
M_LHB = Widget_Base(MovieBase, column=1, /Base_Align_Center)
LH_Coords_ID = Widget_Label(M_LHB, value='(0,0)', /Dynamic_Resize)
LH_Display_ID = Widget_Draw(M_LHB, xsize=143, ysize=143, frame=2,/keyboard_Events, Event_PRO = 'SMF_2CH_GUI_Analysis' )
    ;Righthand Display
M_RHB = Widget_Base(MovieBase, column=1, /Base_Align_Center)
RH_Coords_ID = Widget_Label(M_RHB, value='(0,0)', /Dynamic_Resize)
RH_Display_ID = Widget_Draw(M_RHB, xsize=143, ysize=143, frame=2, /keyboard_Events, Event_PRO = 'SMF_2CH_GUI_Analysis')
    ; Base for controls for animation
MSB = Widget_Base(MovieBase, column = 1, /Base_Align_Center, $
           Event_Pro = 'SMF_2CH_GUI_Movie')
MFrameSlider = Widget_Slider(MSB, Title='Frame #', UValue='Frame', sensitive =0)
MSpeedSlider = Widget_Slider(MSB, Title='Animation Speed', UValue='Speed', /Suppress_Value, $
               Value = 40, Maximum=99, sensitive=0)
MAnimateButton = Widget_Button(MSB, Value = 'Animate', UValue = 'Animate',sensitive=0)
MStopButton = Widget_Button(MSB, Value = 'Stop', UValue = 'Stop',sensitive=0)

;Create a widget to display the images and graphs as we process the movies
;If user has set the 'Tab_Display' keyword, place each window on a separate
;tab, otherwise tile them vertically

IF Keyword_Set(Tab_Display) THEN BEGIN
Tab_Display = 1B
DisplayTabID = Widget_Tab(RHB)
Display1BaseID = Widget_Base(DisplayTabID, Title='Display 1', Column=1)
display1_ID = Widget_Draw(Display1BaseID, xsize=512, ysize=256, Frame=2)
Display2BaseID = Widget_Base(DisplayTabID, Title='Display 2', Column=1)
display2_ID = Widget_Draw(Display2BaseID, xsize=512, ysize=256, Frame=2)
Display3BaseID = Widget_Base(DisplayTabID, Title='Display 3', Column=1)
display3_ID = Widget_Draw(Display3BaseID, xsize=512, ysize=256, Frame=2, /keyboard_Events, Event_PRO = 'SMF_2CH_GUI_Analysis')
ENDIF ELSE BEGIN
Tab_Display = 0B
DisplayBaseID = Widget_Base(RHB, Column=1)
display1_ID = Widget_Draw(DisplayBaseID, xsize=512, ysize=256, Frame=2)
display2_ID = Widget_Draw(DisplayBaseID, xsize=512, ysize=256, Frame=2)
display3_ID = Widget_Draw(DisplayBaseID, xsize=512, ysize=256, Frame=2)
ENDELSE

;Realize the widgets
Widget_Control, tlb, /Realize

;Position the display
CenterTLB, tlb, 0.5, 0.5

;Get the IDs of the draw widgets for future displaying
Widget_Control, display1_ID, Get_Value=D1_id
Widget_Control, display2_ID, Get_Value=D2_id
Widget_Control, display3_ID, Get_Value=D3_id
Widget_Control, LH_Display_ID, Get_Value=LH_D
Widget_Control, RH_Display_ID, Get_Value=RH_D
Widget_Control, Tab_Display_ID, Get_Value=Extra_D
;Widget_Control, Tau_Display_ID, Get_Value = Tau_WID

;Set up table format for displaying pairs (and analysis info later)
Float_Format =replicate('(F6.1)',300)
Int_Format = Replicate('(I1)', 300)
Widget_Control, FlurophoretableID, Format = $
     Transpose([[Float_Format],[Float_Format],[Float_Format],[Float_Format],$
     [Int_Format],[Int_Format], [Int_Format],[Int_Format],[Int_Format], $
     [Int_Format],[Int_Format],[Int_Format], [Int_Format]])


;Set the autocorrelation time to the maximum autocorrelation possible time
;Widget_Control, CorrelationID, Set_Slider_Max = (Size(image))[3]-1
;Widget_Control, CorrelationID, Set_Value = (Size(image))[3]-1

;For a runtime realization of the program, do not allow fitting
;(because it uses 'execute' - a command that does not work within
;the virtual machine environment due to its lack of command line)

;Widget_Control, Fit_button, Sensitive = 0

;*********************************************************************************
;Initialize the text widgets
MovieInfo=[['Data File: Default Nile Red data'],['Image Size (pixels): N/A'], $
    ['# of Frames (Total): N/A'],  ['# of Frames (Analyzed): N/A'],$
    ['Start Frame: N/A'], ['End Frame: N/A'], ['Frame Time (Seconds per Frame): N/A'],$
    ['Time Step (seconds): N/A'], ['Channel Offset (pixels): N/A'],$
    ['Full Filepath: Using Default Data' ]]
AnalysisInfo = [['# of Frames Averaged per Time Point: N/A'], ['# of Time Points: N/A'],$
    ['BPass Parameters: N/A'], ['Feature Parameters: N/A'],$
    ['Signal Size (pixels): N/A'], ['Background Averaging (pixels): N/A']]

Widget_Control, MInfoTextID, Set_Value = MovieInfo
Widget_Control, AInfoTextID, Set_Value = AnalysisInfo

;Store the current device information
Device, Get_Decomposed = Old_decomposed
Tvlct, r, g, b, /get
Device_Settings = {Decomposed:Old_decomposed, r:r, g:g, b:b}

;Load the greyscale Color Table and turn off decomposed colors

LoadCT, 0
Tvlct, r, g, b, /get

device, decomposed=0

;Set the top three entries in the color table to Red and Green and Blue, stored in
;index 'top', 'top+1' and 'top+2' respectively

top=!d.Table_Size-3
r(top)=[255b, 0b, 0b]
g(top)=[0b, 255b, 100b]
b(top)=[0b, 50b, 255b]
Tvlct, r, g, b

;Display the first frame of the image
WSet, D3_id
TVSCL, image, top = top

;*******************************************************************************

;Create a structure to hold the IDs of all the display widgets that will need
;to be addressed later
widgetIDs = { $
   ;Display widgets and their addresses
   D1: display1_ID, $       ;Display #1
   D1_WID: D1_id, $         ;Display #1 address
   D2: display2_ID, $       ;Display #2
   D2_WID: D2_id, $         ;Display #2 address
   D3: display3_ID, $       ;Display #3
   D3_WID: D3_id, $         ;Display #3 address
   ;Menu buttons that need storage
   FileMenu: FileID, $                  ;File menu
   DriftButton: buttonFD, $             ;'Fix sample drift' button
   ImageProcessMenu: iprocessID, $      ;Image processing menu
   DataProcessMenu: dprocessID, $       ;Data processing menu
   RegisterButton: buttonReg, $         ;Register channels button (from menu)
   PickPairs: buttonChoose, $           ;Selct fluorophores (from menu)
   ProcessMenu: iprocessID, $           ;Image processing menu
   AnalysisMenu: AnalysisMenuID, $      ;Analysis menu ID
   ComputerMenu:computerID, $
   ;Table-display related infos
   TableContents: comboBox, $              ;The contents displayed in the table
   Table: FlurophoretableID, $             ;List of features, pairs, etc, as chosen
   AnalysisID:AnalysisID, $                ;ID of the analysis base widget
   Minfo: MInfoTextID, $                   ;ID of text widget with movie info
   Ainfo: AInfoTextID, $                   ;ID of text widget with analysis info
   Extra_D : Extra_D, $                    ;Display widget for extra info (on tab)
   Extra_D_Combobox : Display_Combobox, $  ;ID of combobox to choose display item
   Extra_D_Text:Display_Output, $           ;Text display below the extra info display
   MInfoText: MInfoTextID, $     ;Text widget to display movie parameters
   AInfoText: AInfoTextID, $              ;Text widget to display analysis parameters
   ;Autoload_MoviesID : Movie_Button, $     ;check box for autoloading blowup movies
   ;Analysis-base controls
   PairSelect: PairID, $                ;Choice of active molecule from table
   PairID_Display : PairID_Display,$
   Threshhold: ThreshholdID, $          ;ID of slider to threshhold intensity data
   Hetero_Button: Hetero_Button,$ ;Button to mark molecule as having change of env.
   MovieBase: MovieBase, $       ;Base widget for the animations
   LH_Coords: LH_Coords_ID, $    ;Label showing coords of LH spot
   RH_Coords: RH_Coords_ID, $    ;Label showing coords of RH spot
   LH_Display: LH_Display_ID, $  ;Display of LH spot for movie
   LH_WID: LH_D, $               ;Lefthand display window ID
   RH_Display: RH_Display_ID, $  ; Display of RH spot for movie
   RH_WID: RH_D, $               ;Righthand display window ID
   LoadMovie: MLoadButton, $
   MovieFrame: MFrameSlider, $   ;Slider displaying/controling which frame displayed
   MovieSpeed: MSpeedSlider, $
   AnimateButton: MAnimateButton, $
   StopButton: MStopButton $
           }


;Create a structure to hold onto all the parameters that will be used in
;performing the analysis

analysis = { SelectedPair:0, $          ;Fluorophore currently being analyzed
   FrameRange:{Start:0, Finish:(Size(image))[3]-1, Step:1}, $ ; frames used in analysis
   N_Points: (Size(image))[3], $ ;Number of analyzable points
   AveragingParams:{PSize:5.0, BSize:9.0, Calibration:1.0, $
                    vert_shift:50}, $ ;Params for extracting intensity data
   FrameTime: 1.0, $          ;Seconds per frame of movie
   Threshhold:0L, $          ;Threshhold intensity, points with less are ignored
   AC_Technique:'Basic_ACF', $ ;Main method used to calculate autocorrelation function
   Secondary_ACF : 'Basic_ACF', $     ;Secondary ACF calculation method
   AC_Fit:'Exponenetial', $         ;Fitting method for ACF 1
   AC_Fit2:'Exponenetial', $        ;fitting method for ACF2
   LinFitInfo: {Start:0, Length:3}, $ ;Some info for the linear fit to ACF1
   LinFitInfo2: {Start:0, Length:3}, $ ;Some info for the linear fit to ACF2
   Velocity_Interval: 1, $       ;interval used to calculate ang. veloc. #1
   Velocity_Interval2: 1, $       ;interval used to calculate ang. veloc. #2
   AC_Fit_Start:0, $   ;The beginning of time interval used to calculate ACF
   AC_Fit_End: (Size(image))[3], $ ;The end of time interval used to calc. ACF
   AC_Fit1_Iter: 0, $          ;The fitting iteration for ACF 1
   AC_Fit2_Iter: 0, $          ;The fitting iteration for ACF 2
   Supplemental_Display: 'Nothing', $;What is displayed in the extra tab display
   Display_ACFs : 0B, $      ;calculate both indicated ACFs?
   N_Tau_Segments: 2, $   ;# of segments for which tau is calculated in tau tab
   Tau_Segment_Length: 5L, $  ;Number of points in each segment
   Load_Movies : 0B, $    ; automatically load in local movies for each molecule
   Local_Movie : 0B, $    ;Is the local animation loaded into memory?
   CorrelationLimit:(Size(image))[3]-1, $   ;Maximum time for autocorrelation
   ShowFit:0, $          ;Allow user to turn on/off the ACF fitting routine
   Autoupdate: 0B, $            ;Automatically recalc ACF whenever slider is changed
   MFrame:0, $                           ;Current frame in animation
   MCircles : Ptr_New(/Allocate_Heap), $  ;Overlay circles for animation
   M_LH : Ptr_New(/Allocate_Heap), $      ;Expanded animation, left hand side
   M_RH : Ptr_New(/Allocate_Heap), $      ;Expanded animation, right hand side
   MDelay: 0.1, $                         ;Delay in animation (in seconds)
   MStopFlag: 0 $                  ;Stop flag for animation
            }

;Create an anonymous structure to store info required by the program
info = { widgetIDs:widgetIDs, $           ;Structure containing widget IDs
   analysis:analysis,   $           ;Structure containing analysis parameters
   tab_display:tab_display, $       ;Are display windows tiled(0B), or tabbed(1B)?
   top:top, $                    ;Top index of the color table
   color: {red:r, green:g, blue:b}, $     ;The entire color table
   Device_Settings:Device_Settings, $ ;Original graphics device settings
   Associated_Name:Ptr_New('/home/user/198K_3s_30mW_ORRIT_a-480x221.bin'), $ ;Ptr to image file
   Temp_FilePath: '/home/administrator/', $ ; Filepath used to deposit temporary files
   S_Image: (Size(image))[1:3], $   ;X,Y size of individual frames and # of frames
   Image_Template : Ptr_New(BytArr((Size(image))[1:2])), $ ;The type/size of a frame
   Base_Filepath : '',  $       ;The base for all other filepaths
   SummedImage:Ptr_New(BytScl(image[*,*,0])), $   ;Summed image
   ChannelSeparation:[239, -1, 2], $          ;Separation between channels on CCD
   BPImage:Ptr_New(BytScl(image[*,*,0])), $     ;Filtered image
   BPFiltered:0B, $                    ;Has image been filtered?
   BPParam:{PSize:3, NSize:1.0} , $              ;BPass parameters
   Features:Ptr_New(/Allocate_Heap), $         ;Table of identified features
   FeatureParam:[3,0], $                 ;Parameters used by the 'Feature' routine
   MovieInfo:MovieInfo, $           ;Text array of movie parameters
   AnalysisInfo:AnalysisInfo, $      ;Text array of Analysis parameters
   Pairs:Ptr_New(/Allocate_Heap), $   ;Fluorophores (i.e. pairs of registered spots)
   PairsTable:Ptr_New(/Allocate_Heap), $   ;All fluorophore coord.s saved in array
   Movie_Images:Ptr_New(BytArr((Size(image))[1:2])),$
   Movie_Lun: ptr_new(/allocate_heap)$
 ;  intxstart:0,$                         ;minimum x value for intensity display
 ;  intxend: 5000,$                       ;maximum x value for intensity display
 ;  intystart: 0 ,$                       ;minimum y value for intensity display
 ;  intyend: 5000 $                       ;maximum y value for intensity display
               }

;Put the info structure into the 'User Value'
Widget_Control, tlb, Set_UValue=info, /No_Copy, $
    /KBRD_Focus_Events

;Register the program and end the widget definition module

XManager, 'SMF_2CH_GUI', tlb, /No_block, $
    Event_Handler='SMF_2CH_GUI_TLB_Events', $
    Cleanup='SMF_2CH_GUI_Cleanup', Group_Leader=group_leader

END
