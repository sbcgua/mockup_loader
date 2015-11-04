# Mockup Loader for ABAP unit testing (!!!!! DRAFT !!!!!) #

## Contents ##

<!-- start toc -->

- [Synopsis](#synopsis)
- [Installation](#installation)
- [Reference](#reference)
- [Examples](#examples)
- [Excel to TXT VBS script](#excel-to-txt-vbs-script)
- [Contributors](#contributors)
- [License](#license)

<!-- end toc -->

## Synopsis ##

The tool is created to simplify data preparation/loading for SAP ABAP unit tests. In one of our projects we had to prepare much tables data for unit tests. For example, a set of content from `BKPF`, `BSEG`, `BSET` tables (FI document). The output of the methods under test is also often a table or a complex structure. 

Hard-coding all of that data was not an option - too much to code, difficult to maintain and terrible code readability. So we decided to write a tool which would get the data from TAB delimited `.txt` files, which, in turn, would be prepared in Excel in a convenient way. Certain objectives were set:

- all the test data should be combined together in one file (zip)
- ... and uploaded to SAP - test data should be a part of the dev package (W3MI binary object would fit)
- loading routine should identify the file structure (fields) automatically and verify its compatibility with a target container (structure or table) 
- it should also be able to safely skip fields, missing in `.txt` file, if required (*non strict* mode) e.g. when processing structures (like FI document) with too many fields, most of which are irrelevant to a specific test.

```abap
" Test class (o_ml is mockup_loader instance)
...
call method o_ml->load_data " Load test data (structure) from mockup
  exporting i_obj       = 'TEST1/bkpf'
  importing e_container = ls_bkpf.

call method o_ml->load_data " Load test data (table) from mockup
  exporting i_obj       = 'TEST1/bseg'
            i_strict    = abap_false
  importing e_container = lt_bseg.

...

call method o_test_object->some_processing " Call to the code being tested
  exporting i_bkpf   = ls_bkpf
            it_bseg  = lt_bseg
  importing e_result = l_result. 

assert_equals(...).

```

The first part of the code takes TAB delimited text file `bkpf.txt` in TEST1 directory of ZIP file uploaded as binary object via SMW0 transaction...

```
BUKRS BELNR GJAHR BUZEI BSCHL KOART ...
1000  10    2015  1     40    S     ...
1000  10    2015  2     50    S     ...
```

... and puts it (with proper ALPHA exits and etc) to an internal table with `BSEG` line type.  

### Store/Retrieve ###

Later another objective was identified: some code is quite difficult to test when it has a *select* in the middle. Of course, good code design would assume isolation of DB operations from business logic code, but it is not always possible. So we needed to create a way to substitute *selects* in code to a simple call, which would take the prepared test data instead if test environment was identified. We came up with the solution we called "Store". 
   

```abap
" Test class (o_ml is mockup_loader instance)
...
call method o_ml->store " Store some data with 'BKPF' label
  exporting i_name = 'BKPF'
            i_data = ls_bkpf. " One line structure
...

" Working class method
...
if some_test_env_indicator = abap_false. " Production environment
  " Do DB selects here 

else.                                    " Test environment
  call method zcl_mockup_loader=>retrieve
    exporting i_name  = 'BKPF'
    importing e_data  = ls_fi_doc_header
    exceptions others = 4.
endif. 

if sy-subrc is not initial.
  " Data not selected -> do error handling
endif.

```

In case of multiple test cases it can also be convenient to load a number of table records and then **filter** it based on some key field, availble in the working code. This option is also possible:

``` abap
" Test class
...
call method o_ml->store " Store some data with 'BKPF' label
  exporting i_name   = 'BKPF'
            i_tabkey = 'BELNR'  " Key field for the stored table
            i_data   = lt_bkpf. " Table with MANY different documents
...

" Working class method
...
if some_test_env_indicator = abap_false. " Production environment
  " Do DB selects here 

else.                                    " Test environment
  call method zcl_mockup_loader=>retrieve
    exporting i_name  = 'BKPF'
              i_sift  = l_document_number " Filter key from real local variable
    importing e_data  = ls_fi_doc_header  " Still a flat structure here
    exceptions others = 4.
endif. 

if sy-subrc is not initial.
  " Data not selected -> error handling
endif.

```  

As the final result we can perform complitely dynamic unit tests in our projects, covering most of code, including *DB select* related code **without** actually accessing the database. Of course, it is not only the mockup loader which ensures that. This requires accurate design of the project code, separating DB selection and processing code. But the mockup loader and "store" functionality makes it more convenient.     

### Design approach ###

Here are some facts about package content and invocation approach:

- The main class is called `ZCL_MOCKUP_LOADER`. It is designed as a singleton class assuming it loads ZIPed content once when the test class initiallized. Consequently, the "Store" exists in one  instance as well.
- Most methods of the class may throw `ZCX_MOCKUP_LOADER_ERROR` exception, which in particular specifies some error details available via `get_text()` call (also an error code, which is more for own unit testing purpose). Those methods are assumed to be executed from unit test classes, so there should be no problem to handle exceptions properly there. 
- `RETRIEVE()` method, however, which takes data from the "Store" is **static**. It is assumed to be called from "production" code instead of *DB selects*. It does all singleton magic inside, and throws **non-class** based exception. This is made to avoid the necessity to handle exceptions, irrelevant to the main code, and also to be able to catch the exception as `SY-SUBRC` value. `SY-SUBRC` can be checked later as if it would be the result of a regular DB select. So the interference with the main code is minimal. 
- Zipped text files must be in **Unicode** encoding (UTF16).


## Installation ##

### SAPLink ###

A nugget is available to install the code with SAPLink. SAPLink does not support W3MI objects so after import of the nugget please follow manual step 4.2 to finalize installation. Unit test execution is also a recommended step (see manual p4.3).

### Manual installation ###

1. Create a package with SE21/SE80 (`ZMOCKUP_LOADER` would be a good name :)
2. Create the class with SE24 - `ZCL_MOCKUP_LOADER`. Switch to "Source code based" mode and copy `lib/zcl_mockup_loader.abap` content there. Activate.
3. Create the exception class `ZCX_MOCKUP_LOADER_ERROR` based on `CX_STATIC_CHECK`. Mark "With message class" flag.
    1. Create 3 public attributes: 
        * `METHNAME type SCX_ATTRNAME`
        * `MSG type SCX_ATTRNAME`
        * `CODE type CHAR2`
    2. Go to Texts tab, choose exception id `ZCX_MOCKUP_LOADER_ERROR` (the only one there) and press Message Test button. Set `Message class = 0M, Message number = 500, Attrib1 = METHNAME, Attrib2 = MSG`. This message is one of standard messages with text "& & & &".
    3. Create a static public method `RAISE`. Copy the content of the `lib/zcx_mockup_loader_error-raise.abap` there
    4. Activate   
4. Optionally upload unit tests. 
    1. Create a **test class** for the `ZCL_MOCKUP_LOADER`. Copy the content of `test/zcl_mockup_loader-unit_test.abap` there and activate.
    2. Create a binary data object via SMW0 transaction in the package `ZMOCKUP_LOADER`. Call it `ZMOCKUP_LOADER_UNIT_TEST` and upload the `test/zmockup_loader_unit_test.zip`. 
        * This potentially may require setting up MIME type in Settings->Maintain MIME types menu (the setting is quite obvious, e.g. just specify TYPE=ZIP, EXTENTION=\*.zip).
    3. Run the unit test for the `ZCL_MOCKUP_LOADER` class (Menu->Class->Run->Unit tests or Ctrl+Shift+F10). Should pass ;)


## Reference ##

### CLASS_SET_SOURCE (static) ###

```abap
importing
  I_TYPE type CHAR4 
  I_PATH type STRING
```

- **I_TYPE** - source type.
    - can be `'MIME'`, which loads data from W3MI object (uploaded via SMW0) - this is the way "production" unit test should be executed, the object travels with the package so can be used if, for example, transported to another system instance. 
    - Alternatively the parameter accepts `'FILE'`, which then reads the local file via `GUI_UPLOAD` from workstation. The mode created for development purpose to avoid constant reloading of zip to SMW0 while working on unit tests.  
- **I_PATH** - Actual name of W3MI object or path to zip file on local computer.

The method is **static** and should be called in `CLASS_SETUP` method of a test class **before** `get_instance()` is called first.

**Example:**

```abap
call method zcl_mockup_loader=>class_set_source
  importing
    i_type = 'FILE'
    i_path = 'c:\sap\projectX\unit_tests\mockup.zip'. 
```

### GET_INSTANCE (static) ###

```abap
returning
  value(RO_INSTANCE) type ref to ZCL_MOCKUP_LOADER
```

The method to get a singleton instance of the mockup loader. If this is the first call, then instance is created and initialized, which means that it loads ziped content specified by `CLASS_SET_SOURCE` (or `CLASS_CONSTRUCTOR`) and gets ready to supply the test data. 

**Example:**

```abap
data lo_ex type ref to zcx_mockup_loader_error.

try.
  me->o_ml = zcl_mockup_loader=>get_instance( ).
catch zcx_mockup_loader_error into lo_ex.
  fail( lo_ex->get_text( ) ).
endtry.
```

### FREE_INSTANCE (static) ###

Frees the instance for whatever reason you might need it (never used in our code by now ;). 


### LOAD_DATA (instance) ###

```abap
importing
  I_OBJ    type STRING
  I_STRICT type ABAP_BOOL default ABAP_TRUE
exporting
  E_CONTAINER type ANY
```

- **I_OBJ** - path to file inside ZIP. Extension `'.txt'` is automatically appended so should not be specified. Please be aware that ZIP file names are **case sensitive** (at least this is how SAP handles them).
    - Files must be in **Unicode** encoding (UTF16).
- **I_STRICT** - suggests if the structure of the file must strictly correspond to the structure of target container. 'False' means that all fields in the text file must present in target structure, 'True' **additionally** means that the number of fields is the same as in the target structure.
- **E_CONTAINER** - container for the data. Can be table or structure. In the latter case just the first data line of the file is parsed, no error is thrown if there are more lines in case like that.   

The method assumes that field names are specified in the first line of the text file and are **capitalized**. The order is not important and can be mixed. `MANDT` field is ignored (no value transferred).

**Example:**

ZIP:/TEST1/BSEG.txt
```
BUKRS BELNR GJAHR BUZEI BSCHL KOART
1000  10    2015  1     40    S    
1000  10    2015  2     50    S    
```
Loading code (`i_strict = false`, so all fields, missing in the file, are initialized with empty value).

```abap
try.
  call method o_ml->load_data
    exporting i_obj       = 'TEST1/BSEG'
              i_strict    = abap_false
    importing e_container = lt_bseg.
catch zcx_mockup_loader_error into lo_ex.
  fail( lo_ex->get_text( ) ).
endtry.
```

### LOAD_RAW (instance) ###

```abap
importing
  I_OBJ type STRING
  I_EXT type STRING optional
exporting
  E_CONTENT type XSTRING
```

The method is similar to `LOAD_DATA` except that it just extracts ZIPed information as `XSTRING`. Can be used to test some XML/XSLT procedures for example.  

Optionally, **I_EXT** - file extension - can be specified explicitly. Defaulted to `'.txt'` otherwise.


### STORE (instance) ###

```abap
importing
  I_NAME   type CHAR40
  I_DATA   type ANY
  I_TABKEY type ABAP_COMPNAME optional
```

- **I_NAME** - a label to **retrieve** the data later. If exists already, then overwritten.
- **I_DATA** - data to store. Can be table or structure.
- **I_TABKEY** - optional parameter to specify that **retrieve** can use filter by this field to sieve just specific lines of table. **Only table** can be supplied as data if tabkey is specified. Tabkey is the capitalized name of a field which must exist in the supplied table.    

The method stores supplied data with the label `I_NAME` for later retrieval. It makes **a copy** of the supplied data and does not keep the reference to the data so it is safe to modify supplied variable after the call.

Only one key field may be specified as the tabkey, composite keys are not supported . It is a compromise for the code readability - we assumed that in test environment the variety of the data is (or can be intentionally) seriously decreased without affecting the essence of the test. E.g. selecting FI document should assume that they belong to the same company code (`BUKRS`) and fiscal year (`GJAHR`).  

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

### RETRIEVE (static) ###

```abap
importing
  I_NAME type CHAR40
  I_SIFT type CLIKE optional
exporting
  E_DATA type ANY
exceptions
  RETRIEVE_ERROR
```

- **I_NAME** - the label used in the `STORE()` or `LOAD_AND_STORE()` call 
- **E_DATA** - container to retrieve data to. The type of stored and retrieved data must be identical. Tables, however, are checked for the identical line type. So:
    - Standard/Sorted/Hashed table type are compatible (can be saved as standard and retrieved as sorted)
    - Types like `BKPF_TAB` (dictionary table type) and variables `type table of BKPF` are also compatible
- **I_SIFT** - the value which is used to filter stored table data to the `E_DATA` container. Only lines where TABKEY field (see `STORE` method) equals to `I_SIFT` value will be retrieved.     

**E_DATA** can be table or structure. In the latter case just the first line of the stored data (optionally, filtered with `I_SIFT`) is retrieved.

The method is **static**. This is made to avoid the necessity to handle `ZCX_MOCKUP_LOADER_ERROR` exception in production code (which is irrelevant there). Furthermore, it allows to catch the exception as `SY-SUBRC`  and check it later as if it would be the result of a regular DB select.

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

### PURGE (instance) ###

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

### LOAD_AND_STORE (instance) ###

```abap
importing
  I_OBJ    type STRING
  I_STRICT type ABAP_BOOL default ABAP_TRUE
  I_NAME   type CHAR40
  I_TYPE   type CSEQUENCE
  I_TABKEY type ABAP_COMPNAME optional
```

The method is the combination of `LOAD_DATA()` and `STORE()` - created to avoid intermediary variables to hold the data in between.

See `LOAD_DATA()` for description of **I_OBJ** and **I_STRICT**. See `STORE()` to clarify **I_NAME** and **I_TABKEY**.

**I_TYPE** parameter is different, however. It is literal name of the type to validate the text data structure and create store container.  Please refer to documentation of RTTI method `cl_abap_typedescr=>describe_by_name()` for information on how to specify type names. But briefly it accepts thing like:

-  dictionary types (e.g. `'BSET_TAB'`)
-  type pools types (e.g. `'ABAP_COMPDESCR'` from ABAP type pool)
-  locally defined types (e.g. `'TY_BSET_EXTRACT'` defined locally in program)
-  class public types (e.g. `'ZCL_SOME_CLASS=>TY_SOME_TYPE'`)

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

### Some "Best practice" suggestions ###

We actually use a modified version of this code in our projects. For each project (we currently have one big enough to actually do this ;) we create a wrapper class in the native project package and adjust these 2 things:    

1. We modify the `CLASS_CONSTRUCTOR()` to put some default ZIP container for the project in order not to bother with `CLASS_SET_SOURCE()` each time. `CLASS_SET_SOURCE()` is used then just for temporary reads from a workstation file while unit test is being developed. 
2. We create test classes for each class in the project (obviously). And so we created a wrapper method for `LOAD_DATA()` to split the `I_OBJ` parameter (filename) into several components. E.g. `I_CLASS`, `I_OBJ`, `I_SUFFIX` to have better readability and control. This also gives an opportunity to create a macro at the beginning of a test class and to hardcode CLASS name there. Example:

```abap
define load_mockup_no_strict.
  call method o_ml->load_table
    exporting
      i_strict    = abap_false
      i_class     = 'EVENT'
      i_obj       = &1
      i_suffix    = &2
    importing
      e_container = &3.
end-of-definition.

...

load_mockup_no_strict 'BKPF' '2015' lt_bkpf.

``` 

Which ends up calling `LOAD_DATA()` with the filename = `'EVENT/BKPF-2015.txt'`.


## Examples ##

An example can be found in [example/zmockup_loader_example.abap](/example/zmockup_loader_example.abap).

## Excel to TXT VBS script ##

We have much data prepared in Excel files. Many files, many sheets in each. It is boring and time consuming to copy them all to text (although Ctrl+C in Excel actually copies TAB delimited text which greatly simplifies the matter for small cases).

So we created a script which does the work automatically. How to use it:

1. Excel files should be in one directory
2. Each Excel file should have a sheet named `_INDEX`, it should contain a list of other sheet names which are relevant to be converted.
3. Each sheet should contain data, staring in A1 cell. 
    * The first row must contain field names (capitalized).
    * The first column is used to identify the end of length of the table - so it must be continuous
    * Columns with `'_'` at the beginning are ignored - can be used for meta data or to define table size in case real first data field may contain empty values.
4. The script read the current directory and finds all Excel files. 
5. It identifies if any of those are opened currently (though might not work correctly if you run several Excel instances intentionally)
6. Then it gives you the choice to process opened files only or all of them. 
7. Each file is processed according to points 1-3 and each sheet is saved as txt files (in UTF16 excoding) to `XXXXXX/uncompressed/EXCELNAME/` directory, where `EXCELNAME` is the name of the Excel file.
8. After everything is finished the "uncompressed" directory is compressed to a zip file.       

## Contributors ##

Contributors are described in [CONTRIBUTORS.md](/CONTRIBUTORS.md). You are welcomed to suggest ideas and code improvements ! :)

## License ##

The code is licensed under MIT License. Please see [LICENSE](/LICENSE) for details.
