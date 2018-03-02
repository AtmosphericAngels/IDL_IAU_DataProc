PRO dp_wid_dataproc_upd, event

COMMON dp_data

; gather abundantly used information...
  ID_caltreat   = WIDGET_INFO(event.top, FIND_BY_UNAME='caltreat_dl')
  sel_caltreat  = WIDGET_INFO(ID_caltreat, /DROPLIST_SELECT)
  ID_samtreat   = WIDGET_INFO(event.top, FIND_BY_UNAME='samtreat_dl')
  sel_samtreat  = WIDGET_INFO(ID_samtreat, /DROPLIST_SELECT)
  ID_calip      = WIDGET_INFO(event.top, FIND_BY_UNAME='calip_dl')
  sel_calip     = WIDGET_INFO(ID_calip, /DROPLIST_SELECT)
  ID_experiment = WIDGET_INFO(event.top, FIND_BY_UNAME='exp_dl')
  sel_exp       = WIDGET_INFO(ID_experiment, /DROPLIST_SELECT)
  ID_substance  = WIDGET_INFO(event.top, FIND_BY_UNAME='subst_dl')
  sel_subst     = WIDGET_INFO(ID_substance, /DROPLIST_SELECT)
  ID_eval_mode  = WIDGET_INFO(event.top, FIND_BY_UNAME='eval_mode')
  sel_eval_mode = WIDGET_INFO(ID_eval_mode, /DROPLIST_SELECT)

  uname_sel=WIDGET_INFO(event.id, /uname); check caller
  
  CASE 1 OF
    (uname_sel EQ 'exp_dl') OR (uname_sel EQ 'chg_substnames') : $ 
      BEGIN ; selected experiment changed, update droplists: cal treat, sam treat, substances
        IF ((dp_chrom[sel_exp]).subst[sel_subst].rres.cal_treat)[0] NE '' THEN BEGIN        
          cal_ip=(dp_chrom[sel_exp]).subst[sel_subst].rres.cal_ip_mthd
          caltreat=(dp_chrom[sel_exp]).subst[sel_subst].rres.cal_treat
          samtreat=(dp_chrom[sel_exp]).subst[sel_subst].rres.sam_treat
          new_sel_calip=(WHERE(cal_ip_mthd EQ cal_ip))[0]
          new_sel_caltreat=(WHERE(cal_treat_mthd EQ caltreat))[0]
          new_sel_samtreat=(WHERE(sam_treat_mthd EQ samtreat))[0]
          WIDGET_CONTROL, ID_calip, SET_DROPLIST_SELECT=new_sel_calip
          WIDGET_CONTROL, ID_caltreat, SET_DROPLIST_SELECT=new_sel_caltreat
          WIDGET_CONTROL, ID_samtreat, SET_DROPLIST_SELECT=new_sel_samtreat          
        ENDIF
        WIDGET_CONTROL, ID_substance, SET_VALUE=substlist[sel_exp]
        WIDGET_CONTROL, ID_eval_mode, SET_DROPLIST_SELECT=((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_select)[0]
      END
      
    ELSE: $
      BEGIN
        IF ((dp_chrom[sel_exp]).subst[sel_subst].rres.cal_treat)[0] NE '' THEN BEGIN        
          cal_ip=(dp_chrom[sel_exp]).subst[sel_subst].rres.cal_ip_mthd
          caltreat=(dp_chrom[sel_exp]).subst[sel_subst].rres.cal_treat
          samtreat=(dp_chrom[sel_exp]).subst[sel_subst].rres.sam_treat
          new_sel_calip=(WHERE(cal_ip_mthd EQ cal_ip))[0]
          new_sel_caltreat=(WHERE(cal_treat_mthd EQ caltreat))[0]
          new_sel_samtreat=(WHERE(sam_treat_mthd EQ samtreat))[0]
          WIDGET_CONTROL, ID_calip, SET_DROPLIST_SELECT=new_sel_calip
          WIDGET_CONTROL, ID_caltreat, SET_DROPLIST_SELECT=new_sel_caltreat
          WIDGET_CONTROL, ID_samtreat, SET_DROPLIST_SELECT=new_sel_samtreat
          WIDGET_CONTROL, ID_eval_mode, SET_DROPLIST_SELECT=((dp_chrom[sel_exp]).subst[sel_subst].rres.rsp_select)[0]          
        ENDIF
      END    
  ENDCASE
  
END