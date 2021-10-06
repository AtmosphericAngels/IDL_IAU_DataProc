;+
; PRO: dp_plot_tools
;
; AUTHOR: S.Sala
; MODIFIED: F. Obersteiner, Sep-2016
;
; PURPOSE: plot results of the calculations & modify the plots.
;-
;------------------------------------------------------------------------------------------------------------------------
FUNCTION dp_create_plotobj

  oInfo = OBJ_NEW('IDLsysMonitorInfo')
  numMons = oinfo->GetNumberOfMonitors()
  names = oinfo->GetMonitorNames()
  rects = oInfo->GetRectangles()
  primaryIndex = oInfo->GetPrimaryMonitorIndex()
  OBJ_DESTROY, oInfo

  DEVICE, GET_SCREEN_SIZE=screensize

  XCenter = FIX(screensize[0])
  YCenter = FIX(screensize[1])
  XOff = 0.8*screensize[0]
  YOff = 0.01*screensize[1]
  xsize = 0.25*screensize[0]
  position = [0.1,0.1,0.8,0.5]
  margin = [0.1,0.1,0.1,0.1]
  dimensions = [0.6*screensize[0],0.6*screensize[1]]
  ticklen = 0.01

  date = label_date(date_format = '%H:%I')
  xtickformat = 'LABEL_DATE'

  tickfont_size = 14
  thick = 1
  filled = 1
  errorbar_capsize = 1.
  errorbar_thick = 2

  p1 = errorplot([!Values.D_NAN], [!Values.D_NAN], $
                  margin=margin, $
                  dimensions=dimensions, $
                  color='r',$
                  axis_style=0, $
                  font_size=20,$
                  location=[rects[2, primaryIndex]*0.15, rects[3, primaryIndex]*0.15], $
                  title=' ', $
                  window_title='IAU_DP: Plot',$
                  name="Calibration ",$
                  symbol="circle",$
                  linestyle=0,$
                  sym_size=2,$
                  sym_thick=2,$
                  sym_filled=1,$
                  sym_color='red',$
                  errorbar_capsize=errorbar_capsize,$
                  errorbar_thick=errorbar_thick)


  x_low = axis('X', location=[1,0], TICKUNITS=xtickunits, TICKFORMAT=xtickformat, TICKLEN=ticklen, $
               TICKFONT_SIZE=tickfont_size, TITLE='Time of Measurement')
  x_hgh = axis('X',location=[1,1],showtext=0, tickdir=1, ticklen=ticklen)
  y_lft = axis('Y',tickdir=0, textpos=0, location=[0,0], ticklen=ticklen, tickfont_size=14, $
               text_color='red', title='Normalised Detector Response (Cal Gas)')


  p2 = errorplot([!Values.D_NAN],[!Values.D_NAN],color='b', /OVERPLOT, THICK=1, LINESTYLE=0, name="Sample",$
                  errorbar_capsize=errorbar_capsize,$
                  errorbar_thick=errorbar_thick)
  p2.symbol = "circle"
  p2.sym_size = 2
  p2.sym_filled = 1
  p2.sym_color = 'blue'
  p2.linestyle = " "


  p3 = errorplot([!Values.D_NAN],[!Values.D_NAN],color='r', /OVERPLOT, THICK=1, LINESTYLE=0, name="Cal")
  p3.symbol = "circle"
  p3.linestyle = " "
  p3.sym_size = 2
  p3.sym_thick = 2
  p3.sym_filled = 1
  p3.sym_color = 'red'


  p4 = errorplot([!Values.D_NAN],[!Values.D_NAN],color='purple', /OVERPLOT, THICK=1, LINESTYLE=0, name="Target",$
                  errorbar_capsize=errorbar_capsize,$
                  errorbar_thick=errorbar_thick)
  p4.symbol = "circle"
  p4.linestyle = " "
  p4.sym_size = 2
  p4.sym_thick = 2
  p4.sym_filled = 1
  p4.sym_color = 'purple'


  p5 = errorplot([!Values.D_NAN],[!Values.D_NAN],color='g', /OVERPLOT, THICK=1, LINESTYLE=0, name="Lin.Fit")
  p5.symbol = "circle"
  p5.linestyle = 2
  p5.thick = thick
  p5.sym_filled = filled
  p5.color = 'orange'


  p6 = errorplot([!Values.D_NAN],[!Values.D_NAN],color='b', /OVERPLOT, THICK=1, LINESTYLE=0, name="Run.Mean")
  p6.symbol = "circle"
  p6.linestyle = 2
  p6.thick = thick
  p6.sym_filled = filled
  p6.color = 'blue'


  p7 = errorplot([!Values.D_NAN],[!Values.D_NAN],color='c', /OVERPLOT, THICK=1, LINESTYLE=0, name="Cal.Interpolation")
  p7.symbol = "circle"
  p7.linestyle = 2
  p7.thick = thick
  p7.sym_filled = filled
  p7.color = 'green'


  p8 = errorplot([!Values.D_NAN],[!Values.D_NAN],color='r', /OVERPLOT, THICK=1, LINESTYLE=0, name="Cal.Mean")
  p8.symbol = "circle"
  p8.sym_size = 1
  p8.sym_filled = filled
  p8.linestyle = 2
  p8.thick = thick
  p8.color = 'magenta'



  txt = OBJARR(1)

  FOR i=0, N_ELEMENTS(txt)-1 DO txt[i]=text([0],[0],"",/DATA)

  leg = legend(TARGET=p, /NORMAL, /AUTO_TEXT_COLOR, POSITION=[0.9825,0.95], SHADOW=0, $
               HORIZONTAL_Spacing=0.05, VERTICAL_SPACING=0.02)

  y_rgt = axis('Y',tickdir=1, textpos=1, location=[1,0], ticklen=ticklen )

  y_rgt.tickfont_size = 14
  y_rgt.text_color = 'blue'
  y_rgt.title = 'Normalised Detector Response (Sample Gas)'

  pltstrct = {p1:p1,p2:p2,p3:p3,p4:p4,p5:p5,p6:p6,p7:p7,p8:p8, $
              x_low:x_low, x_hgh:x_hgh, y_lft:y_lft,y_rgt:y_rgt, $
              txt:txt,leg:leg}

  p1.select

  RETURN, pltstrct

END

;########################################################################################################################

FUNCTION dp_modify_plot, p=p, x1=x1, y1=y1, yerr1=yerr1, x2=x2, y2=y2, yerr2=yerr2, x3=x3, y3=y3, yerr3=yerr3, $
                              x4=x4, y4=y4, yerr4=yerr4, x5=x5, y5=y5, yerr5=yerr5, x6=x6, y6=y6, yerr6=yerr6, $
                              x7=x7, y7=y7, yerr7=yerr7, x8=x8, y8=y8, yerr8=yerr8

  COMMON dp_data

  p_vd = OBJ_VALID(p)
  IF (p_vd EQ !NULL) THEN p=dp_create_plotobj() ELSE IF SIZE(p, /TYPE) NE 8 THEN p=dp_create_plotobj()

  IF KEYWORD_SET(x1) THEN BEGIN
    IF NOT KEYWORD_SET(yerr1) THEN yerr1=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x1))
    p.p1.setdata, x1, y1, yerr1
  ENDIF

  IF KEYWORD_SET(x2) THEN BEGIN
    IF NOT KEYWORD_SET(yerr2) THEN yerr2=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x2))
    p.p2.setdata, x2, y2, yerr2
  ENDIF

  IF KEYWORD_SET(x3) THEN BEGIN
    IF NOT KEYWORD_SET(yerr3) THEN yerr3=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x3))
    p.p3.setdata, x3, y3, yerr3
  ENDIF

  IF KEYWORD_SET(x4) THEN BEGIN
    IF NOT KEYWORD_SET(yerr4) THEN yerr4=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x4))
    p.p4.setdata, x4, y4, yerr4
  ENDIF

  IF KEYWORD_SET(x5) THEN BEGIN
    IF NOT KEYWORD_SET(yerr5) THEN yerr5=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x5))
    p.p5.setdata, x5, y5, yerr5
  ENDIF

  IF KEYWORD_SET(x6) THEN BEGIN
    IF NOT KEYWORD_SET(yerr6) THEN yerr6=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x6))
    p.p6.setdata, x6, y6, yerr6
  ENDIF

  IF KEYWORD_SET(x7) THEN BEGIN
    IF NOT KEYWORD_SET(yerr7) THEN yerr7=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x7))
    p.p7.setdata, x7, y7, yerr7
  ENDIF

  IF KEYWORD_SET(x8) THEN BEGIN
    IF NOT KEYWORD_SET(yerr8) THEN yerr8=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x8))
    p.p8.setdata, x8, y8, yerr8
  ENDIF


  p.p1.getdata,x1,y1,yerr1
  p.p2.getdata,x2,y2,yerr2
  p.p3.getdata,x3,y3,yerr3
  p.p4.getdata,x4,y4,yerr4

  IF x1[0] EQ 0. THEN x1 = !VALUES.D_NAN
  IF x2[0] EQ 0. THEN x2 = !VALUES.D_NAN
  IF x3[0] EQ 0. THEN x3 = !VALUES.D_NAN
  IF x4[0] EQ 0. THEN x4 = !VALUES.D_NAN

  min_x=min([min(x1),min(x2),min(x3),min(x4)],/NaN)
  max_x=max([max(x1),max(x2),max(x3),max(x4)],/NaN)
  min_y=min([min(y1,/NaN),min(y2,/NaN),min(y3,/NaN),min(y4,/NaN)], /NaN)
  max_y=max([max(y1,/NaN),max(y2,/NaN),max(y3,/NaN),max(y4,/NaN)],/NaN)

  IF FINITE(min_y) THEN BEGIN
    p.p1.xrange=[min_x-0.03*(max_x-min_x),max_x+0.03*(max_x-min_x)]
    p.p1.yrange=[min_y-0.07*(max_y-min_y), max_y+0.07*(max_y-min_y)]
  ENDIF ELSE BEGIN
    p.p1.xrange=[0.,1.]
    p.p1.yrange=[0.,1.]
  ENDELSE

  tmp = p.leg
  tmp.delete
  p.leg = legend(TARGET=[p.p1,p.p2,p.p3,p.p4,p.p5,p.p6,p.p7,p.p8], /NORMAL, /AUTO_TEXT_COLOR, POSITION=[0.9825,0.95], $
                 SAMPLE_WIDTH=0.0005, SHADOW=0, HORIZONTAL_Spacing=0.05, VERTICAL_SPACING=0.02)

  RETURN, p

END

;########################################################################################################################

FUNCTION dp_create_plotobj2_multi, ytitle1=ytitle1, ytitle2=ytitle2, ytitle3=ytitle3

  IF KEYWORD_SET(ytitle1) EQ 0 THEN ytitle1= ""
  IF KEYWORD_SET(ytitle2) EQ 0 THEN ytitle2= ""
  IF KEYWORD_SET(ytitle3) EQ 0 THEN ytitle3= ""



  oInfo = OBJ_NEW('IDLsysMonitorInfo')
  numMons = oinfo->GetNumberOfMonitors()
  names = oinfo->GetMonitorNames()
  rects = oInfo->GetRectangles()
  primaryIndex = oInfo->GetPrimaryMonitorIndex()
  OBJ_DESTROY, oInfo


  Device,  Get_Screen_Size=screenSize

  XCenter = FIX(ScreenSize[0])
  YCenter = FIX(ScreenSize[1])
  XOff = 0.8*screenSize[0]
  YOff = 0.6*screenSize[1]
  xsize = 0.25*screensize[0]
  position=[0.1,0.1,0.8,0.5]
  margin=[0.125,0.1,0.125,0.1]
  dimensions=[(0.35*screensize[0]),0.6*screensize[1]]
  ticklen = 0.01

  date = label_date(DATE_FORMAT='%H:%I')
  xtickformat = 'LABEL_DATE'

  tickfont_size = 14
  thick = 1
  filled = 1

  p1 = errorplot([!Values.D_NAN],[!Values.D_NAN],$
                  margin=[0.15,0,0.05,0.1], $
                  dimensions=dimensions, $
                  location=[rects[2, primaryIndex]*0.17, rects[3, primaryIndex]*0.17], $
                  name="retention_time",$
                  layout=[1,3,1],$
                  linestyle=0,$
                  symbol=5,$
                  sym_size=2,$
                  sym_thick=2,$
                  sym_filled=1,$
                  sym_color='red',$
                  ytext_color='red',$
                  ytitle=ytitle1 ,$
                  xticklen=1,$
                  xgridstyle=1,$
                  xsubgrid=-1,$
                  yticklen=ticklen,$
                  ytickfont_size=12)


  p2 = errorplot([!Values.D_NAN],[!Values.D_NAN],$
                  color='b',$
                  /CURRENT,$
                  margin=[0.15,0,0.05,0],$
                  layout=[1,3,2] ,$
                  symbol=5,$
                  sym_size=2,$
                  sym_thick=2,$
                  sym_filled=1,$
                  ytickfont_size=12,$
                  yticklen=ticklen,$
                  xticklen=1,$
                  xgridstyle=1,$
                  xsubgrid=-1,$
                  ytext_color='blue',$
                  ytitle=ytitle2 ,$
                  sym_color='blue')


  p3 = errorplot([!Values.D_NAN],[!Values.D_NAN],$
                  color='g',$
                  /CURRENT,$
                  margin=[0.15,0.2,0.05,0],$
                  layout=[1,3,3],$
                  symbol=5,$
                  sym_size=2,$
                  sym_thick=2,$
                  sym_filled=1,$
                  ytickfont_size=12,$
                  yticklen=ticklen,$
                  xticklen=1,$
                  xgridstyle=1,$
                  xsubgrid=-1,$
                  ytext_color='green',$
                  ytitle=ytitle3 ,$
                  xtitle="measurement no.",$
                  sym_color='green')

  ; p2.select

  ; y_rgt = axis('Y',tickdir=1, textpos=1, location=[1,0], ticklen=0, title="Normalised area/height",text_color='blue' )

  pltstrct = {p1:p1,p2:p2,p3:p3}
  p1.select

  RETURN, pltstrct

END

;########################################################################################################################

FUNCTION dp_modify_plot2_multi, p2=p2, x1=x1, y1=y1, yerr1=yerr1, x2=x2, y2=y2, yerr2=yerr2, x3=x3, y3=y3, yerr3=yerr3, ytitle1=ytitle1, ytitle2=ytitle2, ytitle3=ytitle3

  IF NOT KEYWORD_SET(ytitle1)  THEN ytitle1= "normalised RT"
  IF NOT KEYWORD_SET(ytitle2)  THEN ytitle2= "normalised H/A"
  IF NOT KEYWORD_SET(ytitle3)  THEN ytitle3= "S/N"

  COMMON dp_data

  p_vd = OBJ_VALID(p2)
  IF (p_vd EQ !NULL) THEN BEGIN
    p2=dp_create_plotobj2_multi(ytitle1=ytitle1, ytitle2=ytitle2, ytitle3=ytitle3)
  ENDIF ELSE BEGIN
   IF size(p2, /type) NE 8 THEN p2=dp_create_plotobj2_multi(ytitle1=ytitle1, ytitle2=ytitle2, ytitle3=ytitle3)
  ENDELSE


  IF KEYWORD_SET(x1) THEN BEGIN
    IF NOT KEYWORD_SET(yerr1) THEN yerr1=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x1))
    p2.p1.setdata, x1, y1, yerr1
  ENDIF

  IF KEYWORD_SET(x2) THEN BEGIN
    IF NOT KEYWORD_SET(yerr2) THEN yerr2=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x2))
    p2.p2.setdata, x2, y2, yerr2
  ENDIF

  IF KEYWORD_SET(x3) THEN BEGIN
    IF NOT KEYWORD_SET(yerr3) THEN yerr3=REPLICATE(!VALUES.D_NAN, N_ELEMENTS(x3))
    p2.p3.setdata, x3, y3, yerr3
  ENDIF

  p2.p1.getdata,x1,y1,yerr1
  p2.p2.getdata,x2,y2,yerr2
  p2.p3.getdata,x3,y3,yerr3

  min_x=min(x1,/nan)
  max_x=max(x1,/nan)

  p2.p1.xrange=[min_x-0.01,max_x+0.01]
  p2.p2.xrange=[min_x-0.01,max_x+0.01]
  p2.p3.xrange=[min_x-0.01,max_x+0.01]


  min_y1=min(y1,/nan)
  max_y1=max(y1,/nan)

  IF finite(min_y1) THEN BEGIN
    p2.p1.yrange=[min_y1-0.001*min_y1,max_y1+0.001*max_y1]
  ENDIF ELSE BEGIN
    p2.p1.yrange=[0,1]
  ENDELSE

  min_y2=min(y2,/nan)
  max_y2=max(y2,/nan)
  IF finite(min_y2) THEN BEGIN
    p2.p2.yrange=[min_y2-0.001*min_y2,max_y2+0.001*max_y2]
  ENDIF ELSE BEGIN
    p2.p2.yrange=[0,1]
  ENDELSE

  min_y3=min(y3,/nan)
  max_y3=max(y3,/nan)
  IF finite(min_y3) THEN BEGIN
    p2.p3.yrange=[min_y3-0.1*min_y3,max_y3+0.1*max_y3]
  ENDIF ELSE BEGIN
    p2.p3.yrange=[0,1]
  ENDELSE

  RETURN, p2

END
