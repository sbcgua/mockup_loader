# Mockup Loader for ABAP unit testing (!!!!! DRAFT !!!!!) #

# Synopsis #

The tool is created to simplify data preparation/loading for SAP ABAP unit tests. In one of our projects we had to prepare much tables data for unit tests. For example, a set of content from BKPF, BSEG, BSET tables (FI document) where the output of the code to verify is also a table or a complex structure. 

Hard-coding all of that data was not an option - too much to code, difficult to maintain and terrible code readability. So we decided to write a tool which would get the data from TAB delimited .txt files, which, in turn, would be prepared in Excel in a convenient way. Certain objectives were set:

- all the test data should be combined in one file (zip)
- test data should be a part of the dev package (W3MI binary object would fit)
- loading routine should identify the file structure (fields) automatically and be able to safely skip irrelevant field, missing in .txt, if required (*non strict* mode) e.g. when processing structures with too many fields like FI document.

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

The first part of the code takes TAB separated text file bkpf.txt in TEST1 directory of ZIP uploaded as binary object via SMW0 transaction...

```
BUKRS BELNR GJAHR BUZEI BSCHL KOART ...
1000  10    2015  1     40    S     ...
1000  10    2015  2     50    S     ...
```

... and converts (with proper ALPHA exits and etc) it is an internal table of BSEG structure.  

Later another objective was identified: some code is quite difficult to test when it has a *select* in the middle. Of course, good code design would assume isolation of DB operations, but it is not always possible. So we needed to create a way to substitute *selects* in code to a simple call which would take the prepared test data instead. We came up with the solution we called "store". 
   

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
else. " Test environment detected
  call method zcl_mockup_loader=>retrieve
    exporting i_name  = 'BKPF'
    importing e_data  = ls_fi_doc_header
    exceptions others = 4.
endif. 

if sy-subrc is not initial.
  " Data not selected -> do error handling
endif.

```

In case of multiple tests it can be also convenient to load a package of table records and then **filter** it based on some key field. So this way is also possible:

``` abap
" Test class
...
call method o_ml->store " Store some data with 'BKPF' label
  exporting i_name   = 'BKPF'
            i_tabkey = 'BELNR'  " Key field for the saved table
            i_data   = lt_bkpf. " Table with MANY different documents
...

" Working class method
...
if some_test_env_indicator = abap_false. " Production environment
  " Do DB selects here 
else. " Test environment
  call method zcl_mockup_loader=>retrieve
    exporting i_name  = 'BKPF'
              i_sift  = l_document_number " Filter key value
    importing e_data  = ls_fi_doc_header  " Still a flat structure here
    exceptions others = 4.
endif. 

if sy-subrc is not initial.
  " Data not selected -> error handling
endif.

```  

As a result we can perform complitely dynamix unit tests in our projects, covering most of code, including *select* related code **without** actually accessing the database. Of course, it is not only the mockup loader which ensures that. This requires accurate design of the project code, separating DB selection and processing code. But it becomes more convenient.     

### Design approach ###

Here are some facts about package content and invocation approach:

- The main class is called ZCL_MOCKUP_LOADER. It is designed as a singlton class assuming it loads ZIPed content once when the test class initiallized. Consequently, the "Store" exists in one  instance as well.
- Most methods of the class may throw ZCX_MOCKUP_LOADER_ERROR exception, which in particular specifies some error details available via get_text() call (also an error code, which is more for own unit testing purpose).
- **Retrieve()** method, however, which takes data from the "Store" is **static**. It does all singlton magic inside, and throws **non-class** based exception. This is made to avoid the necessity to handle exceptions, irrelevant to the main code, and also to be able to catch the exception as SY-SUBRC value and check it later as if it would be the result of a regular DB select.  


# Installation #

A nugget is available to install the code with SAPLink.

For the case of manual installation:

1. Create a package with SE21/SE80 (**ZMOCKUP_LOADER** would be a good name :)
2. Create the class with SE24 - **ZCL_MOCKUP_LOADER**. Switch to "Source code based" mode and copy ZCL_MOCKUP_LOADER.txt content there. Activate.
3. Create the exception class **ZCX_MOCKUP_LOADER_ERROR** (some headache here)
    1. Create 3 public attributes: a) METHNAME type SCX_ATTRNAME, b) MSG type SCX_ATTRNAME, c) CODE type CHAR2.
    2. Go to Texts tab, choose exception id ZCX_MOCKUP_LOADER_ERROR (the only one there) and press Message Test button. Set Message class = 0M, Message number = 500, Attrib1 = METHNAME, Attrib2 = MSG
    3. Create a statuc public method RAISE. Copy the content of the XXXX there
    4. Activate   
4. Optionally upload unit tests. 
    1. Create a **test class** for the ZCL_MOCKUP_LOADER. Copy the content of XXXXX there and activate.
    2. Create a binary data object via SMW0 transaction for the package ZMOCKUP_LOADER. Call it ZMOCKUP_LOADER_UNIT_TEST and upload the ZMOCKUP_LOADER_UNIT_TEST.zip. This potentially may require to setup MIME type in Settings->Maintain MIME types menu (the setting is quite obvious, e.g. just specify TYPE=ZIP, EXTENTION=\*.zip).
    3. Run Unit test for the ZCL_MOCKUP_LOADER class (Menu->Class->Run->Unit tests or Ctrl+Shift+F10). Should pass ;)


# Reference #

### CLASS_SET_SOURCE (static) ###

```abap
importing
  I_TYPE type CHAR4 
  I_PATH type STRING
```

- **I_TYPE** - can be 'MIME', which loads data from W3MI object (SMW0 uploaded via SMW0) - this is the way "production" unit test should be executed, the object travels with the package so can be used if, for example, transported to another client. Alternative the parameter accepts 'FILE', which then reads the local file via GUI_UPLOAD from - created for development purpose to avoid constant reloading of zip to SMW0 while working on unit tests.  
- **I_PATH** - Actual name of W3MI object or path to zip file on local computer.

The method is **static** and should be called in CLASS_SETUP method of a test class.

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

The method to get a singleton instance of the mockup loader. If this is the first call, then instance is created and initialized, which mean that it loads ziped content specified by CLASS_SET_SOURCE (or CLASS_CONSTRUCTOR) and gets ready to supply the test data. 

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
  I_OBJ type STRING
  I_STRICT type ABAP_BOOL default ABAP_TRUE
exporting
  E_CONTAINER type ANY
```

- **I_OBJ** - path to file inside ZIP. Extension '.txt' is automatically appended so should not be specified.
- **I_STRICT** - suggests if the structure of the file must strictly correspond to the structure of target container. 'False' means that all fields in the text file must present in target structure, 'True' **additionally** means that the number of fields is the same as in target structure.
- **E_CONTAINER** - container for the data. Can be table or structure. In the latter case just the first data line of the file is parsed, no error is thrown if there are more lines in case like that.   

The method assumes that field names are specified in the first line of the text file and are **capitalized**. The order is not important and can be mixed. **MANDT** field is ignored (no value transferred).

**Example:**

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

The method is similar to LOAD_DATA() except that it just extracts ZIPed information as XSTRING. Can be used to test some XML/XSLT procedures for example.  

Optionally, **I_EXT** - file extension - can be specified explicitly, defaulted to '.txt' otherwise.


### STORE (instance) ###

```abap
importing
  I_NAME type CHAR40
  I_DATA type ANY
  I_TABKEY type ABAP_COMPNAME optional
```

- **I_NAME** - a label to **retrieve** the data later. If exists already, then overwritten.
- **I_DATA** - data to store. Can be table or structure.
- **I_TABKEY** - optional parameter to specify that **retrieve** can use filter by this field to sieve just specific lines of table. **Only table** can be supplied as data if tabkey is specified. Tabkey must be a field (capitalized) of the supplied table.    

The method stores supplied data with the label **I_NAME** for later retrieval. It makes **a copy** of the supplied data and does not keeps the reference to the data so it is safe to modify supplied variable after the call.

Only one key field may be specified as tabkey, composite keys are not supported . It is a compromise for the code readability - we assumed that in test environment the variety of the data is (or can be intentionally) seriously decreased without affecting the essence of the test. 

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

- **I_NAME** - the label used in the **store** or **load_and_store** call 
- **E_DATA** - container to retrieve data to. The type of stored and retrieved data must be identical. Tables, however, are checked for the identical line type. So:
    - Standard/Sorted/Hashed table type are compatible
    - Types like "BKPF_TAB" (dictionary table type) and "... table of BKPF" are also compatible
- **I_SIFT** - the value which is used to filter stored table data to the E_DATA container. Only lines where TABKEY field (see STORE method) equals to I_SIFT value will be retrieved.     

**E_DATA** can be table or structure. In the latter case just the first line of the stored data (optionally, filtered with **I_SIFT**) is retrieved.

The method is static. This is made to avoid the necessity to handle ZCX_MOCKUP_LOADER_ERROR exception in production code (which is irrelevant there). Furthermore, it allows to catch the exception as SY-SUBRC  and check it later as if it would be the result of a regular DB select.

**Example:**

```abap
if some_test_env_indicator = abap_false. " Production environment
  " Do DB selects here 
else. " Test environment
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

- **I_NAME** - store label to delete. If '\*' is specified - the whole store is purged.

**Example:**

```abap
call method o_ml->purge
  exporting i_name   = 'BKPF'.
```

### LOAD_AND_STORE (instance) ###

```abap
importing
  I_OBJ type STRING
  I_STRICT type ABAP_BOOL default ABAP_TRUE
  I_NAME type CHAR40
  I_TYPE type CSEQUENCE
  I_TABKEY type ABAP_COMPNAME optional
```

The method is the combination of LOAD_DATA() and STORE() - created to avoid intermediary variable to hold the data in between.

See LOAD_DATA() for description of **I_OBJ** and **I_STRICT**. See STORE() to clarify **I_NAME** and **I_TABKEY**.

**I_TYPE** parameter is different, however. It is literal name of the type to validate the text data structure and create store container.  Please refer to documentation of RTTI method cl_abap_typedescr=>describe_by_name for information on how to specify type names. But briefly it accepts thing like:

-  dictionary types (e.g. *'BSET_TAB'*)
-  type pools types (e.g. *'ABAP_COMPDESCR'* from ABAP type pool)
-  locally defined types (e.g. *'TY_BSET_EXTRACT'* defined locally in program)
-  class public types (e.g. *'ZCL_SOME_CLASS=>TY_SOME_TYPE'*)

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

1. We modify the **CLASS_CONSTRUCTOR()** to put some default ZIP container for the project in order not to bother with CLASS_SET_SOURCE() each time.
2. We create test classes for each class in the project (obviously). And so we created wrapper method for LOAD_DATA() method to split the I_OBJ parameter (filename) into several components. E.g. I_CLASS, I_OBJ, I_SUFFIX to have better readability and control. This also gives an opportunity to create a macro at the beginning of test class and to hide CLASS name there. Example:

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

Which ends up calling LOAD_DATA() with the filename = *'EVENT/BKPF-2015.txt'*.


# Examples #

... Section is under construction ...

# Contributors #

Contributors are described in CONTRIBUTORS.md. You are welcomed to suggest ideas and code improvements ! :)

# License #

The code is licensed under MIT License. Please see LICENSE for details.
