# Mockup Loader for ABAP unit testing #

*Version: 0.2.0*    
*[History of changes](/changelog.txt)*    
*See also project [Wiki](../../wiki)*

## Contents ##

<!-- start toc -->

- [Synopsis](#synopsis)
- [Installation](#installation)
- [Reference](#reference)
- [Examples and HOWTOs](#examples-and-howtos)
- [Other features](#other-features)
- [Excel to TXT conversion script](#excel-to-txt-conversion-script)
- [Contributors](#contributors)
- [Publications](#publications)
- [Plans](#plans)
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

In case of multiple test cases it can also be convenient to load a number of table records and then **filter** it based on some key field, available in the working code. This option is also possible:

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

As the final result we can perform completely dynamic unit tests in our projects, covering most of code, including *DB select* related code **without** actually accessing the database. Of course, it is not only the mockup loader which ensures that. This requires accurate design of the project code, separating DB selection and processing code. But the mockup loader and "store" functionality makes it more convenient.  

![](illustration.png)   

### Design approach ###

Here are some facts about package content and invocation approach:

- The main class is called `ZCL_MOCKUP_LOADER`. It is designed as a singleton class assuming it loads ZIPped content once when the test class initialized. Consequently, the "Store" exists in one  instance as well.
- Most methods of the class may throw local exception based on cx_static_check, which in particular specifies some error details available via `get_text()` call (also an error code, which is more for own unit testing purpose). Those methods are assumed to be executed from unit test classes, so there should be no problem to handle exceptions properly there. 
- `RETRIEVE()` method, however, which takes data from the "Store" is **static**. It is assumed to be called from "production" code instead of *DB selects*. It does all singleton magic inside, and throws **non-class** based exception. This is made to avoid the necessity to handle exceptions, irrelevant to the main code, and also to be able to catch the exception as `SY-SUBRC` value. `SY-SUBRC` can be checked later as if it would be the result of a regular DB select. So the interference with the main code is minimal. 
- Zipped text files must be in **Unicode** encoding (UTF16).


## Installation ##

### SAPLink ###

A nugget and slinkees are available to install the code with SAPLink. 
 
Vanilla SAPLink does not support:
- SET/GET parameters so either install ZSAPLINK_USER_PARAMETER plugin or create them manually after import (see manual step 5)
- W3MI objects so please follow manual step 4.2 to finalize installation. 

Unit test execution is a recommended after-step (see manual step 4.3).

### Manual installation ###

1. Create a package with SE21/SE80 (`ZMOCKUP_LOADER` would be a good name :)
2. Create the class with SE24 - `ZCL_MOCKUP_LOADER`. 
	1. Switch to "Source code based" mode and copy `lib/zcl_mockup_loader.abap` content there. 
	2. Add code from `zcl_mockup_loader-local_classes.abap` to local definitions and implementations (`Goto` menu) 
	3. Activate.
3. Optionally upload unit tests. 
    1. Create a **test class** for the `ZCL_MOCKUP_LOADER`. Copy the content of `test/zcl_mockup_loader-unit_test.abap` there and activate.
    2. Create a binary data object via SMW0 transaction in the package `ZMOCKUP_LOADER`. Call it `ZMOCKUP_LOADER_UNIT_TEST` and upload the `test/zmockup_loader_unit_test.zip`. 
        * This potentially may require setting up MIME type in Settings->Maintain MIME types menu (the setting is quite obvious, e.g. just specify TYPE=ZIP, EXTENTION=\*.zip).
    3. Run the unit test for the `ZCL_MOCKUP_LOADER` class (Menu->Class->Run->Unit tests or Ctrl+Shift+F10). Should pass ;)
4. Create SET/GET parameters `ZMOCKUP_LOADER_STYPE` and `ZMOCKUP_LOADER_SPATH` with SM30 and maintenance view `TPARA`.
5. Create the program ZMOCKUP_LOADER_SWITCH_SOURCE 
    1. ... Copy the content of `lib\zmockup_loader_switch_source.abap` and activate.
    2. Create transaction `ZMOCKUP_LOADER_SWSRC` with SE93 and set it to run the program. 

## Reference ##

Complete reference of class method can be found in [REFERENCE.md](REFERENCE.md). 

## Examples and HOWTOs ##

Have a look at the Howto section in the project [Wiki](../../wiki).

A complete example can be found in [example/zmockup_loader_example.abap](/example/zmockup_loader_example.abap).

## Other features ##

### Load source redirection ###

Final test mockup is supposed to be uploaded as MIME object via SMW0. During data or test creation, however, it is more convenient (faster) to read local file. 

Supposedly, you call static `CLASS_SET_SOURCE` method in `CLASS_SETUP` **of testing class** to define the 'normal' type/path to mockup archive. To temporarily switch to another source you can call transaction `ZMOCKUP_LOADER_SWSRC` (or program `ZMOCKUP_LOADER_SWITCH_SOURCE`). It will initialize SET/GET parameters  `ZMOCKUP_LOADER_STYPE` and `ZMOCKUP_LOADER_SPATH` which will overload defaults for current session. User changes in the selection screen immediately change the parameters in session memory, no run is required.

## Excel to TXT conversion script ##

We have a lot of data prepared in Excel files. Many files, many sheets in each. It is boring and time consuming to copy them all to text (although Ctrl+C in Excel actually copies TAB delimited text which greatly simplifies the matter for small cases).

So we created a VB script which does this work automatically. Please read [EXCEL2TXT.md](EXCEL2TXT.md) for more info.

## Contributors ##

Contributors are described in [CONTRIBUTORS.md](/CONTRIBUTORS.md). You are welcomed to suggest ideas and code improvements ! :)

Main development team members are:

- Alexander Tsybulsky
- Svetlana Shlapak
- Bohdan Petrushchak

## Publications ##

- [Unit testing mockup loader for ABAP @SCN](http://scn.sap.com/community/abap/blog/2015/11/12/unit-testing-mockup-loader-for-abap)
- [How to do convenient multicase unit tests with zmockup_loader @SCN](http://scn.sap.com/community/abap/blog/2016/03/20/how-to-do-convenient-multicase-test-with-zmockuploader)

## Plans ##

- Text parser is useful itself for other tasks - maybe worth splitting the class into 2 tools
- Maybe implement direct editing of the text data in SAP. In ALV or, better, with inline Excel. To have a consistent solution and avoid external VBS script  

## License ##

The code is licensed under MIT License. Please see [LICENSE](/LICENSE) for details.
