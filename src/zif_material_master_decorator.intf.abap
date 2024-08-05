INTERFACE zif_material_master_decorator
  PUBLIC .

  CONSTANTS: c_refclassname TYPE seoclsname VALUE 'ZIF_MATERIAL_MASTER_DECORATOR'.


  CONSTANTS: BEGIN OF c_material_plant,
               segname                   TYPE edilsegtyp VALUE 'ZE1_MARCM',
               database_table_name       TYPE tabname16  VALUE 'MARC',
               no_data                   TYPE char1      VALUE '/',
               material_plant            TYPE stringval  VALUE 'c_material_plant',
               under_score               TYPE char1      VALUE '_',
               dash                      TYPE char1      VALUE '-',
               suffix_plt                TYPE char4      VALUE '_plt',
               identtable                TYPE stringval  VALUE 'zz1_identtable0',
               identtab                  TYPE stringval  VALUE 'zzidenttab0',
               zzidenttab01              TYPE fieldname  VALUE 'ZZIDENTTAB01',
               zzidenttab02              TYPE fieldname  VALUE 'ZZIDENTTAB02',
               zzidenttab03              TYPE fieldname  VALUE 'ZZIDENTTAB03',
               zzidenttab04              TYPE fieldname  VALUE 'ZZIDENTTAB04',
               zzidenttab05              TYPE fieldname  VALUE 'ZZIDENTTAB05',
               zzretail                  TYPE fieldname  VALUE 'ZZRETAIL',
               zzretail_descr            TYPE fieldname  VALUE 'ZZRETAIL_DESCR',
               zzbu                      TYPE fieldname  VALUE 'ZZBU',
               zzbubezei                 TYPE fieldname  VALUE 'ZZBUBEZEI',
               zzopti                    TYPE fieldname  VALUE 'ZZOPTI',
               zz1_identtable01_plt      TYPE fieldname  VALUE 'ZZ1_IDENTTABLE01_PLT',
               zz1_identtable02_plt      TYPE fieldname  VALUE 'ZZ1_IDENTTABLE02_PLT',
               zz1_identtable03_plt      TYPE fieldname  VALUE 'ZZ1_IDENTTABLE03_PLT',
               zz1_identtable04_plt      TYPE fieldname  VALUE 'ZZ1_IDENTTABLE04_PLT',
               zz1_identtable05_plt      TYPE fieldname  VALUE 'ZZ1_IDENTTABLE05_PLT',
               zz1_retail_plt            TYPE fieldname  VALUE 'ZZ1_RETAIL_PLT',
               zz1_retaildescription_plt TYPE fieldname  VALUE 'ZZ1_RETAILDESCRIPTION_PLT',
               zz1_businessunit_plt      TYPE fieldname  VALUE 'ZZ1_BUSINESSUNIT_PLT',
               zz1_budescription_plt     TYPE fieldname  VALUE 'ZZ1_BUDESCRIPTION_PLT',
               zz1_opti_plt              TYPE fieldname  VALUE 'ZZ1_OPTI_PLT',
             END OF c_material_plant.
  METHODS execute
    IMPORTING
      !iv_message_type   TYPE edidc-mestyp
      !is_f_cust_segment TYPE edidd
    CHANGING
      cs_f_mara_ueb      TYPE mara_ueb OPTIONAL
      cs_f_makt_ueb      TYPE makt_ueb OPTIONAL
      cs_f_marc_ueb      TYPE marc_ueb OPTIONAL
      cs_f_mard_ueb      TYPE mard_ueb OPTIONAL
      cs_f_mfhm_ueb      TYPE mfhm_ueb OPTIONAL
      cs_f_mpgd_ueb      TYPE mpgd_ueb OPTIONAL
      cs_f_mpop_ueb      TYPE mpop_ueb OPTIONAL
      cs_f_mprw_ueb      TYPE mprw_ueb OPTIONAL
      cs_f_mveg_ueb      TYPE mveg_ueb OPTIONAL
      cs_f_mveu_ueb      TYPE mveu_ueb OPTIONAL
      cs_f_marm_ueb      TYPE marm_ueb OPTIONAL
      cs_f_mean_ueb      TYPE mea1_ueb OPTIONAL
      cs_f_mbew_ueb      TYPE mbew_ueb OPTIONAL
      cs_f_mlgn_ueb      TYPE mlgn_ueb OPTIONAL
      cs_f_mvke_ueb      TYPE mvke_ueb OPTIONAL
      cs_f_mlgt_ueb      TYPE mlgt_ueb OPTIONAL
      cs_f_marc2_ueb     TYPE marc_aps_ext_ueb OPTIONAL
      !ct_res_fields     TYPE delfields_tt .
ENDINTERFACE.
