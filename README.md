# Mockup Loader for ABAP unit testing #

*Version: 0.1*

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
    2. Go to Texts tab, choose exception id `ZCX_MOCKUP_LOADER_ERROR` (the only one there) and press Message Test button. Set `Message class = SY, Message number = 499, Attrib1 = METHNAME, Attrib2 = MSG`. This message is one of standard messages with text "& & & &".
    3. Create a static public method `RAISE`. Copy the content of the `lib/zcx_mockup_loader_error-raise.abap` there. 
        * Add importing parameter `MSG type STRING`
        * Add optional importing parameter `CODE type CHAR2`
        * Add `ZCX_MOCKUP_LOADER_ERROR` to the exceptions section 
    4. Activate   
4. Optionally upload unit tests. 
    1. Create a **test class** for the `ZCL_MOCKUP_LOADER`. Copy the content of `test/zcl_mockup_loader-unit_test.abap` there and activate.
    2. Create a binary data object via SMW0 transaction in the package `ZMOCKUP_LOADER`. Call it `ZMOCKUP_LOADER_UNIT_TEST` and upload the `test/zmockup_loader_unit_test.zip`. 
        * This potentially may require setting up MIME type in Settings->Maintain MIME types menu (the setting is quite obvious, e.g. just specify TYPE=ZIP, EXTENTION=\*.zip).
    3. Run the unit test for the `ZCL_MOCKUP_LOADER` class (Menu->Class->Run->Unit tests or Ctrl+Shift+F10). Should pass ;)


## Reference ##

Complete reference of class method can be found in [REFERENCE.md](REFERENCE.md). 

## Examples ##

An example can be found in [example/zmockup_loader_example.abap](/example/zmockup_loader_example.abap).

## Excel to TXT VBS script ##

We have a lot of data prepared in Excel files. Many files, many sheets in each. It is boring and time consuming to copy them all to text (although Ctrl+C in Excel actually copies TAB delimited text which greatly simplifies the matter for small cases).

So we created a script which does the work automatically. How to use it:

1. Excel files should be in one directory, same as the script. Formats supported: `.xlsx`, `.xls`, `.xlsm`, `.ods`
2. Each Excel file should have a sheet named `_contents`, it should contain a list of other sheet names which are relevant to be converted. In the second column there should be a mark 'X' to save the sheet to text. (See [example/Example.ods](example/Example.ods)) 
3. Each sheet should contain data, staring in A1 cell. 
    * The first row must contain field names (capitalized).
    * The first column is used to identify the end of length of the table - so it must be continuous
    * Columns with `'_'` at the beginning are ignored - can be used for meta data or to define table size in case real first data field may contain empty values (See example).
4. The script reads the current directory and finds all Excel files. 
5. It identifies if any of those are opened currently (though might not work correctly if you run several Excel instances)
6. Then it gives you the choice to process opened files only or all of them. 
7. Each file is processed according to points 1-3 and each sheet is saved as `.txt` file (in UTF16 encoding) to `uncompressed/EXCELFILENAME/` directory, where `EXCELFILENAME` is the name of the Excel file.
8. After everything is finished the "uncompressed" directory is compressed to a zip file and placed to the same directory where script is.

### Command line parameters ###

The script also supports command line parameters and can be executed with `cscript.exe` (preferable - then execution log is output to console). We use `.bat` files like this for example: 

```
@cscript PrepareZip.vbs -o -z c:\sap\mockup.zip
@pause
```

Command line parameters:

- `-h`  - help screen
- `-o`  - silently process just opened files
- `-a`  - silently process all files
- `-z`  - use this path to zip file instead of default one
- `-i`  - copy (include) following path into uncompressed directory and consequently to zip
- `-nz` - Skip archiving, just generate text files to 'uncompressed' dir

## Contributors ##

Contributors are described in [CONTRIBUTORS.md](/CONTRIBUTORS.md). You are welcomed to suggest ideas and code improvements ! :)

Main development team members are:
- Alexander Tsybulsky
- Svetlana Shlapak
- Bohdan Petrushchak

## License ##

The code is licensed under MIT License. Please see [LICENSE](/LICENSE) for details.
