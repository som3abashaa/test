*&---------------------------------------------------------------------*
*& Report ZRPMM_SHOWROOM_MGMT2
*&---------------------------------------------------------------------*
* Title                  :  Delivery List                              *
* Functional Consultant  :      Mohamed Hosny                          *
* Technical Consultant   :      Ibrahim Othman                         *
* Date                   :      18.03.2019                             *
* Transport Request No   :      SKDK912937                             *
* changed by             :
* last Changed on        :
*----------------------------------------------------------------------
* *Description: *
*----------------------------------------------------------------------*
*                        Modification Log                              *
*----------------------------------------------------------------------*
* ModNo    Date    Consultant         Description of Change(s)         *
*----------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZRPMM_SHOWROOM_MGMT2.
*------------------------Types Declaration--------------------------------------------*
  types: begin of str,
            layvr       type layvar,
            sortf       type layposnr,
            laygr       type laygr,
            fix_id      type wrf_fixid_ty,
            matnr       type matnr,
*            lmver       type laymod_ver,
            LAYMOD_VER       type LAYMOD_VER,
            lm_date_fr  type lm_date_fr,
            lm_date_to  type lm_date_to,
            SHQNO       type SHF_QNT_OPT,
            PERIO       type JAHRPER,
            budat       type DAERF,
            LMNUM       type LAYMOD_NUM,
            fix_type    type wrf_fix_type_ty,
            fix_kap_def type wrf_fixkap_def_ty,
            fix_high    type hoehe,
            fix_wide    type breit,
            fix_deep    type laeng,
            fix_tot     type hoehe,
            fix_unit    type meabm,
            VTEXT       type laytx20,
            LTEXT       TYPE LM_BEZ_40,
            FIX_IDT     type WRF_FIXIDT_TY,
            FIX_TYPET   type WRF_FIX_TYPET_TY,
            MAKTX       type MAKTX,
            monyear     type JAHRPER,
            BUKRS       type BUKRS,
            VTWEG       type VTWEG,
            SPART       type SPART,
            REGIO       type REGIO,
            VKBUR       type VKBUR,
            ABSMG       type RKE2_ABSMG,
            ERLOS       type RKE2_ERLOS,
            VV002       type RKE2_VV002,
            actual      type RKE2_ERLOS,
            tstatus     type RKE2_ABSMG,
            zicon       type char4,
         end of str.
*--------------------------------------------------------------------------------------*
*------------------------Tables Declaration--------------------------------------------*
  tables: twglv, malg, wrf_lmvf, mara, wlmv, ce1sa00.
*--------------------------------------------------------------------------------------*
*------------------------Data Declaration----------------------------------------------*
  data: itab type table of str,
        wa   type str.
*--------------------------------------------------------------------------------------*
*------------------------Field Catalogue-----------------------------------------------*
  data: cat_tab type lvc_t_fcat,
        cat_wa  type lvc_s_fcat,
        layout  type lvc_s_layo.
*--------------------------------------------------------------------------------------*
*------------------------Includes------------------------------------------------------*
  INCLUDE <icon>.
*--------------------------------------------------------------------------------------*
*------------------------Select Options------------------------------------------------*
  select-options: s_layvr  for twglv-layvr OBLIGATORY,
                  s_sortf  for twglv-sortf,
                  s_laygr  for malg-laygr,
                  s_fix_id for wrf_lmvf-fix_id,
                  s_matnr  for mara-matnr,
                  s_budat  for ce1sa00-budat."wlmv-lm_date_fr.
*--------------------------------------------------------------------------------------*
*************************Start of selection*********************************************
  select layvr,
         sortf,
         malg~laygr,
         fix_id,
         mara~matnr,
*         lmver,
         wlmv~LAYMOD_VER,
         lm_date_fr,
         lm_date_to,
         SHQNO,
         PERIO,
         budat,
         LMNUM
   from
    malg
   join
    twglv
   on
    malg~laygr = twglv~laygr
   join
    wlmv
   on
    malg~laygr = wlmv~laygr
   and
    malg~lmver = wlmv~LAYMOD_VER
   join
    wrf_lmvf_asgmt
   on
    malg~laygr = wrf_lmvf_asgmt~laygr
   and
    malg~SHELF = wrf_lmvf_asgmt~FIX_SEQ
*   and
*    malg~lmver = wrf_lmvf_asgmt~LAYMOD_VER
   join
    mara
   on
    malg~matnr = mara~matnr
   join
    ce1sa00
   on
    malg~matnr = ce1sa00~ARTNR
   and
    PALEDGER = '01'
   and
    VRGAR = 'F'
   into table
    @itab
   where
    layvr in @s_layvr
and    sortf in @s_sortf
and    malg~laygr in @s_laygr
and    fix_id in @s_fix_id
and    mara~matnr in @s_matnr
"and    lm_date_fr in lm_date.
and    budat in @s_budat.

sort itab.
delete ADJACENT DUPLICATES FROM itab COMPARING LAYMOD_VER LMNUM lm_date_fr fix_id.

loop at itab ASSIGNING FIELD-SYMBOL(<fs>).
  select single fix_type
                fix_kap_def
                fix_high
                fix_wide
                fix_deep
                fix_unit
    from wrf_lmvf
    into ( <fs>-fix_type,
           <fs>-fix_kap_def,
           <fs>-fix_high,
           <fs>-fix_wide,
           <fs>-fix_deep,
           <fs>-fix_unit )
    where fix_id = <fs>-fix_id.

    <fs>-fix_tot = <fs>-fix_high * <fs>-fix_wide * <fs>-fix_deep.

    select single VTEXT from twgvt into <fs>-VTEXT where layvr = <fs>-layvr and spras = sy-langu.

    select single LTEXT from twmlt into <fs>-LTEXT where laygr = <fs>-laygr and spras = sy-langu.

    select single FIX_IDT from WRF_LMVFT into <fs>-fix_idt where fix_id = <fs>-fix_id and spras = sy-langu.

    select single FIX_TYPET from WRF_LMVF_TYPET into <fs>-FIX_TYPET where FIX_TYPE = <fs>-fix_type and spras = sy-langu.

    select single MAKTX from makt into <fs>-MAKTX where matnr = <fs>-matnr and spras = sy-langu.

    <fs>-monyear = <fs>-budat(4) && '0' && <fs>-budat+4(2).

    select single BUKRS VTWEG SPART REGIO VKBUR
      from CE1SA00
      into ( <fs>-BUKRS, <fs>-VTWEG, <fs>-SPART, <fs>-REGIO, <fs>-VKBUR )
      where PALEDGER = '01'
      and VRGAR = 'F'
      and PERIO = <fs>-monyear
      and ARTNR = <fs>-matnr.

    select  sum( absmg ) sum( ERLOS ) sum( VV002 )
      from CE1SA00
      into ( <fs>-ABSMG, <fs>-ERLOS, <fs>-VV002 )
      where PALEDGER = '01'
      and VRGAR = 'F'
      and PERIO = <fs>-monyear
      and ARTNR = <fs>-matnr.

      <fs>-actual  = <fs>-ERLOS + <fs>-VV002.
      <fs>-tstatus = <fs>-ABSMG - <fs>-SHQNO.
      IF <fs>-tstatus >= <fs>-SHQNO.
        <fs>-zicon = icon_green_light.
      else.
        <fs>-zicon = icon_red_light.
      ENDIF.

      modify itab from <fs>.
endloop.
****************************************************************************************


  cat_wa-fieldname = 'LAYVR'.
  cat_wa-scrtext_l = 'Layout(Store)'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'VTEXT'.
  cat_wa-scrtext_l = 'Layout Description'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'SORTF'.
  cat_wa-scrtext_l = 'Layout Area No.'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'LAYGR'.
  cat_wa-scrtext_l = 'Layout Module (Zone)'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'LTEXT'.
  cat_wa-scrtext_l = 'Layout Module Desc.'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_ID'.
  cat_wa-scrtext_l = 'Fixture No.'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_IDT'.
  cat_wa-scrtext_l = 'Fixture Desc.'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_TYPE'.
  cat_wa-scrtext_l = 'Fixture Category'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_TYPET'.
  cat_wa-scrtext_l = 'Fixture Category Desc.'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_KAP_DEF'.
  cat_wa-scrtext_l = 'Fixture Default Capacity'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_HIGH'.
  cat_wa-scrtext_l = 'Fixture Height'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_WIDE'.
  cat_wa-scrtext_l = 'Fixture Width'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_DEEP'.
  cat_wa-scrtext_l = 'Fixture Depth'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_TOT'.
  cat_wa-scrtext_l = 'Fixture Total Space'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'FIX_UNIT'.
  cat_wa-scrtext_l = 'Fixture Unit'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'MATNR'.
  cat_wa-scrtext_l = 'Material No.'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'MAKTX'.
  cat_wa-scrtext_l = 'Material Description'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'LAYMOD_VER'.
  cat_wa-scrtext_l = 'Layout Mod. Version'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'LM_DATE_FR'.
  cat_wa-scrtext_l = 'LModule From'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'LM_DATE_TO'.
  cat_wa-scrtext_l = 'LModule To'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'BUDAT'.
  cat_wa-scrtext_l = 'Billing Posting Date'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'PERIO'.
  cat_wa-scrtext_l = 'Period/Year'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'BUKRS'.
  cat_wa-scrtext_l = 'Company Code'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'VTWEG'.
  cat_wa-scrtext_l = 'Distribution Channel'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'SPART'.
  cat_wa-scrtext_l = 'Division'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'REGIO'.
  cat_wa-scrtext_l = 'Sales Region'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'VKBUR'.
  cat_wa-scrtext_l = 'Sales Office'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'SHQNO'.
  cat_wa-scrtext_l = 'Target Sales Quantity'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'ABSMG'.
  cat_wa-scrtext_l = 'Actual Sales Quantity'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'ACTUAL'.
  cat_wa-scrtext_l = 'Actual Sales Revenue'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'TSTATUS'.
  cat_wa-scrtext_l = 'Target VS Actual Quantity'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.

  cat_wa-fieldname = 'ZICON'.
  cat_wa-scrtext_l = 'Signal'.
  APPEND cat_wa TO cat_tab.
  CLEAR cat_wa.


  layout-cwidth_opt = 'X'.

data GS_variant TYPE DISVARIANT.

GS_variant-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     i_callback_program = sy-repid
      is_layout_lvc   = layout
      it_fieldcat_lvc = cat_tab
      i_save          = 'X'
      is_variant      = GS_variant
*      i_callback_pf_status = 'ZSTATUS'
    TABLES
      t_outtab        = itab.
  IF sy-subrc <> 0.
*       Implement suitable error handling here
  ENDIF.
