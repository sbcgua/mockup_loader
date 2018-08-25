# Mockup Loader usage reference

## ZCL_MOCKUP_LOADER

### CREATE (static, constructor)

```abap
importing
  I_TYPE        type CHAR4 
  I_PATH        type STRING
  I_AMT_FORMAT  type CHAR2 
  I_ENCODING    type ABAP_ENCODING
  I_DATE_FORMAT type CHAR4
```

Creates an instance of mockup loader and read the zip file (from FILE or for MIME storage).

- **I_TYPE** - source type.
    - can be `'MIME'`, which loads data from W3MI object (uploaded via SMW0) - this is the way "production" unit test should be executed, the object travels with the package so can be used if, for example, transported to another system instance. 
    - Alternatively the parameter accepts `'FILE'`, which then reads the local file via `GUI_UPLOAD` from workstation. The mode created for development purpose to avoid constant reloading of zip to SMW0 while working on unit tests.  
- **I_PATH** - Actual name of W3MI object or path to zip file on local computer.
- **I_AMT_FORMAT** - amount separators. First character defines thousand separator, the second one defines decimal separator. E.g. `'.,'` would suppose amounts like `123.000,12`. Empty parameter resets to default - `' ,'`. The second character cannot be empty - this also resets the format to defaults.
- **I_ENCODING** - encoding of text files in zip. Default is 4103 which is UTF16. See table `TCP00` for list of ABAP encodings. **Maybe will switch to UTF8 in later releases.**
- **I_DATE_FORMAT** - hint how to parse the date. Contains `DMY` in needed order plus separator char. E.g. `'DMY.'` expects `31.12.2017`, `'YMD-'` expects `2017-12-31`.

Also reads GET/SET parameters `ZMOCKUP_LOADER_STYPE`, `ZMOCKUP_LOADER_SPATH` and `ZMOCKUP_LOADER_SMIME`. If `ZMOCKUP_LOADER_STYPE` is not empty it **overrides**  the parameters (`i_type` and `i_path`) used for the call. This is a feature to use during unit test creation or active development, not to upload unit test mockup each time. Parameters can be set via transaction `SU3` or via `ZMOCKUP_LOADER_SWITCH_SOURCE` program. 

**Example:**

```abap
call method zcl_mockup_loader=>create
  exporting
    i_type = 'FILE'
    i_path = 'c:\sap\projectX\unit_tests\mockup.zip'. 
```

### SET_PARAMS

```abap
importing
  I_AMT_FORMAT  type CHAR2 
  I_ENCODING    type ABAP_ENCODING
  I_DATE_FORMAT type CHAR4
```

Changes the parsing parameters on-the-fly. See `CREATE` descrition for parameter explanation.

**Example:**

```abap
call method zcl_mockup_loader=>set_params
  exporting
    i_amt_format = '.,'.
```

### LOAD_DATA

```abap
importing
  I_OBJ    type STRING
  I_STRICT type ABAP_BOOL default ABAP_TRUE
  I_WHERE  type ANY       optional
exporting
  E_CONTAINER type ANY
```

- **I_OBJ** - path to file inside ZIP. Extension `'.txt'` is automatically appended so should not be specified. Please be aware that ZIP file names are **case sensitive** (at least this is how SAP handles them).
- **I_STRICT** - suggests if the structure of the file must strictly correspond to the structure of target container. The call **always** validates that all fields in the text file are present in target structure. `I_STRICT` = 'True' **additionally** means that the number of fields is the same as in the target structure.
    - One exception is `MANDT` field. It may be skipped in a text file even for strict validation. So a text file with all structure fields but MANDT is still considered as strictly equal.
- **I_WHERE** - optional condition to filter the sourse table. See "Using filtering" section below for details.   
- **E_CONTAINER** - container for the data. Can be table or structure. In the latter case just the first data line of the file is parsed, no error is thrown if there are more lines in case like that.   

The method assumes that field names are specified in the first line of the text file and are **capitalized**. The order is not important and can be mixed. `MANDT` field, if present, is ignored (no value transferred).

**Example:**

ZIP:/TEST1/bseg.txt
```
BUKRS BELNR GJAHR BUZEI BSCHL KOART
1000  10    2015  1     40    S    
1000  10    2015  2     50    S    
```
Loading code (`i_strict = false`, so all fields, missing in the file, are initialized with empty value).

```abap
try.
  call method o_ml->load_data
    exporting i_obj       = 'TEST1/bseg'
              i_strict    = abap_false
    importing e_container = lt_bseg.
catch zcx_mockup_loader_error into lo_ex.
  fail( lo_ex->get_text( ) ).
endtry.
```

#### Using filtering

`I_WHERE` accepts several kinds of input:

1) A string condition in form of `"A=B"`, where `A` is a name of a target table filed to filter and `B` is requested value (all others will be filtered out). If `A` is missing in the target table - it is **ignored** (no exception). Be aware that `B` is not typed so it may result in dump if used improperly. `'='` may contain spaces around it.

```abap
  call method o_ml->load_data
    exporting i_obj       = 'TEST1/BSEG'
              i_where     = 'BELNR = 0000000010'
    importing e_container = lt_bseg.
```

2) A structure of range tables which are used to filter the output. Component of the structure must be named after target table fields. The structure may contain ONLY ranges. The structure may contain components (names) which are missing in the target table - they are just ignored. 

```abap
  data:
      begin of l_where,
        belnr  type range of belnr_d,
      end of l_where,
      rl_belnr like line of l_where-belnr,

  rl_belnr-sign   = 'I'.
  rl_belnr-option = 'EQ'.
  rl_belnr-low    = '0000000010'.
  append rl_belnr to l_where-belnr.
...
  call method o_ml->load_data
    exporting i_obj       = 'TEST1/BSEG'
              i_where     = l_where
    importing e_container = lt_bseg.
```

3) A structure of `ZCL_MOCKUP_LOADER=>TY_WHERE` or a table of `TT_WHERE`, where each line contain a filter (applied simultaneously in case of table => AND). `NAME` component should contain target table field name (ignored if missing in target table). `RANGE` is a reference to a range table. (we assume it should be convenient and well-readable in 7.40+ environments).
 
```abap
  data:
      where_tab type zcl_mockup_loader=>tt_where,
      l_where   type zcl_mockup_loader=>ty_where,
      belnr_rng type range of belnr_d,
      r_belnr   like line of rt_belnr,

  r_belnr-sign   = 'I'.
  r_belnr-option = 'EQ'.
  r_belnr-low    = '0000000010'.
  append r_belnr to belnr_rng.

  l_where-name  = 'BELNR'.
  get reference of range_rng into l_where-range.
  append l_where to where_tab. 
...
  call method o_ml->load_data
    exporting i_obj       = 'TEST1/BSEG'
              i_where     = l_where
    importing e_container = lt_bseg.
  " OR
  call method o_ml->load_data
    exporting i_obj       = 'TEST1/BSEG'
              i_strict    = abap_false
              i_where     = where_tab
    importing e_container = lt_bseg.

```

#### "Best practice" suggestions

To improve code readability within numerous datasets we use macros.

```abap
define load_mockup_no_strict.
  call method o_ml->load_data
    exporting
      i_strict    = abap_false
      i_obj       = 'TEST1/' && &1
    importing
      e_container = &2.
end-of-definition.

...

load_mockup_no_strict 'BKPF' lt_bkpf.
``` 


### LOAD_RAW

```abap
importing
  I_OBJ type STRING
  I_EXT type STRING optional
exporting
  E_CONTENT type XSTRING
```

The method is similar to `LOAD_DATA` except that it just extracts ZIPed information as `XSTRING`. Can be used to test some XML/XSLT procedures for example.  

Optionally, **I_EXT** - file extension - can be specified explicitly. Defaulted to `'.txt'` otherwise.

### LOAD_AND_STORE

```abap
importing
  I_OBJ    type STRING
  I_STRICT type ABAP_BOOL default ABAP_TRUE
  I_NAME   type CHAR40
  I_TYPE   type CSEQUENCE
  I_TABKEY type ABAP_COMPNAME optional
```

The method is the combination of `LOAD_DATA` and `STORE` (see `ZCL_MOCKUP_LOADER_STORE` below) - created to avoid intermediary variables to hold the data in between.

See `LOAD_DATA` for description of **I_OBJ** and **I_STRICT**. See `STORE` to clarify **I_NAME** and **I_TABKEY**.

**I_TYPE** is literal name of the type to validate the text data structure and create store container.  Please refer to documentation of RTTI method `cl_abap_typedescr=>describe_by_name` for information on how to specify type names. But briefly it accepts thing like:

-  dictionary types (e.g. `'BSET_TAB'`)
-  type pools types (e.g. `'ABAP_COMPDESCR'` from ABAP type pool)
-  locally defined types (e.g. `'TY_BSET_EXTRACT'` defined locally in program)
-  class **public** types (e.g. `'ZCL_SOME_CLASS=>TY_SOME_TYPE'`)

**Example:**

```abap
try.
  call method o->load_and_store
    exporting i_obj       = 'TEST1/BSEG'
              i_name      = 'BSEG'
              i_type      = 'BSEG_T'. " Disctionary BSEG table type
catch zcx_mockup_loader_error into lo_ex.
  fail( lo_ex->get_text( ) ).
endtry.
```




## ZCL_MOCKUP_LOADER_STORE

### GET_INSTANCE (static)

Gets singleton instance of store. As the store supposed to be called from "code under test" it is designed as singleton.

### FREE_INSTANCE (static)

Frees the instance of store if it was created.

### STORE

```abap
importing
  I_NAME   type CHAR40
  I_DATA   type ANY
  I_TABKEY type ABAP_COMPNAME optional
```

- **I_NAME** - a label to **retrieve** the data later. Overwritten, if already exists.
- **I_DATA** - data to store. Can be table or structure.
- **I_TABKEY** - optional parameter to specify that **retrieve** can use filter by this field to sieve just specific lines of table. **Only table** can be supplied as data if tabkey is specified. Tabkey is the capitalized name of a field which must exist in the supplied table.    

The method stores supplied data with the label `I_NAME` for later retrieval. It makes **a copy** of the supplied data and does not keep the reference to the data so it is safe to modify supplied variable after the call.

Only one key field may be specified as the tabkey, composite keys are not supported. It is a compromise for the code readability - we assumed that in test environment the variety of the data is (or can be intentionally) seriously decreased without affecting the essence of the test. E.g. selecting FI document should assume that they belong to the same company code (`BUKRS`) and fiscal year (`GJAHR`).  

**Example:**

```abap
try.
  call method o_ml->store 
    exporting i_name   = 'BKPF'
              i_tabkey = 'BELNR'  " Key field for the saved table
              i_data   = lt_bkpf. 
catch zcx_mockup_loader_error into lo_ex.
  fail( lo_ex->get_text( ) ).
endtry.
```

### RETRIEVE (static)

```abap
importing
  I_NAME type CHAR40
  I_SIFT type CLIKE optional
  I_WHERE type ANY optional
exporting
  E_DATA type ANY
exceptions
  RETRIEVE_ERROR
```

- **I_NAME** - the label used in the `STORE` or `LOAD_AND_STORE` call 
- **E_DATA** - container to retrieve data to. The type of stored and retrieved data must be identical. Tables, however, are checked for the identical line type. So:
    - Standard/Sorted/Hashed table type are compatible (can be saved as standard and retrieved as sorted)
    - Types like `BKPF_TAB` (dictionary table type) and variables `type table of BKPF` are also compatible (line type is checked)
- **I_SIFT** - the value which is used to filter stored table data to the `E_DATA` container. Only lines where TABKEY field (see `STORE` method) equals to `I_SIFT` value will be retrieved.    
- **I_WHERE** - alternative filtering method to `I_SIFT`. Cannot be used simultaneously. See `LOAD_DATA` method description for more details.  

**E_DATA** can be table or structure. In the latter case just the first line of the stored data (optionally, filtered with `I_SIFT`) is retrieved.

The method is **static**. This is made to avoid the necessity to handle class-exception in production code (which is irrelevant there). Furthermore, it allows to catch the exception as `SY-SUBRC`  and check it later as if it would be the result of a regular DB select.

**Example:**

```abap
if some_test_env_indicator = abap_false. " Production environment
  " Do DB selects here 

else.                                    " Test environment
  call method zcl_mockup_loader=>retrieve
    exporting i_name  = 'BKPF'
              i_sift  = l_document_number " Filter key value
    importing e_data  = ls_fi_doc_header  " Structure or table allowed here
    exceptions others = 4.
endif. 

if sy-subrc is not initial.
  " Data not selected -> do error handling
endif.

```

### PURGE (instance)

```abap
importing
  I_NAME type CHAR40
```

- **I_NAME** - store label to delete. If `'*'` is specified - the whole store is purged.

**Example:**

```abap
call method o_ml->purge
  exporting i_name   = 'BKPF'.
```



## ZCL_MOCKUP_LOADER_UTILS

This is the class for some utilities commonly used by mockup loader components. Though can be useful for other purposes.

### FILTER_TABLE

Implements the data filtering technique, described in the 'Using filtering' section above.

Example:

```abap
zcl_mockup_loader_utils=>filter_table(
  exporting
    i_where     = 'BELNR = 0000000010'
    i_tab       = lt_source_tab
  importing
    e_container = lt_filtered_data ).
```

or

```abap
data lt_filter type zcl_mockup_loader_utils=>tt_filter.
lt_filter = zcl_mockup_loader_utils=>build_filter( 'BELNR = 0000000010' ).
zcl_mockup_loader_utils=>filter_table(
  exporting
    i_filter    = lt_filter
    i_tab       = lt_source_tab
  importing
    e_container = lt_filtered_data ).
```

`e_container` can also be a structure - the first matching record is retrieved.

