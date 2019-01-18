# Excel to TXT conversion script #

You may have a lot of data prepared in Excel files. Many files, many sheets in each. Although Ctrl+C in Excel actually copies TAB-delimited text which greatly simplifies the matter for minor cases, it is boring and time consuming to copy all the test cases to text. Here are special tools to simplify this workflow.

- [mockup compiler](https://github.com/sbcgua/mockup_compiler) - ABAP implementation, requires [abap2xlsx](https://github.com/ivanfemia/abap2xlsx) installed.
- [mockup compiler JS](https://github.com/sbcgua/mockup-compiler-js) - java script implemenation, requires nodejs environment at the developer's machine.

Both tools do more or less the same: reads directory of `xlsx` files, compilers them into tab-delimited texts and zips. ABAP version of compiler also automatically uploads it to MIME storage (tr. `SMW0`). Both tools can watch the changes in the directory and re-compile the data on-the-fly. See the repos for more details.

JS version cannot upload to SAP, however there are workarounds:
- either use [abap_w3mi_poller](https://github.com/sbcgua/abap_w3mi_poller) which watches specified files (zipped result in this case) and upload to MIME storage
- or use `zmockup_loader_swsrc` tool (part of the mockup loader) to _temporarily_ switch source of unit test to a file target for the _current session_. This is my **preferable** way in fact as it ensures that only working version is uploaded to the system and _gives possibility for different people to work on different parts of unit test (!)_. 

## Requirement to source ##

1. Excel files should be in one directory, same as the script. Formats supported: `.xlsx`. Should theoretically work with `.xls`, `.xlsm`, `.ods` but needs to be tested and fixed in file filtering code, TODO...
2. Each Excel file should have a sheet named `_contents`, it should contain a list of other sheet names which are relevant to be converted. In the second column there should be a mark 'X' to save the sheet to text. (See [example/Example.ods](../example/Example.ods)) 
3. Each sheet should contain data, staring in A1 cell. 
    * The first row must contain field names (capitalized).
    * Columns after the first empty columns are ignored
    * Rows after the first empty row are ignored
    * Columns with `'_'` at the beginning are ignored - can be used for meta data (See example).
4. The program reads the current directory and finds all Excel files. 
5. Each file is processed and converted into tab-delimited `.txt` file (in UTF-8 encoding). The resulting path inside ZIP file is `<EXCEL_FILENAME>/<sheet_name>.txt`. Excel file name is uppercased.

For more details see documentation of the chosen tool.