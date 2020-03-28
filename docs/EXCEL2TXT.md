# Excel to TXT conversion script #

You may have a lot of data prepared in Excel files. Many files, many sheets in each. Although Ctrl+C in Excel actually copies TAB-delimited text which greatly simplifies the matter for minor cases, it is boring and time consuming to copy all the test cases to text. Here are special tools to simplify this workflow.

- [mockup compiler](https://github.com/sbcgua/mockup_loader_toolkit) - ABAP implementation.
- [mockup compiler JS](https://github.com/sbcgua/mockup-compiler-js) - java script implementation, requires nodejs environment at the developer's machine.

Both tools do more or less the same: reads directory of `xlsx` files, compilers them into tab-delimited texts and zips. ABAP version of compiler also automatically uploads it to MIME storage (tr. `SMW0`). Both tools can watch the changes in the directory and re-compile the data on-the-fly. See the repos for more details. As the result you can address your mocks as `<excel_filename>/<sheet_name>.txt` (lower-cased).

JS version cannot upload to SAP, however there are workarounds:
- either use [abap_w3mi_poller](https://github.com/sbcgua/abap_w3mi_poller) which watches specified files (zipped result in this case) and upload to MIME storage
- or use `zmockup_loader_swsrc` tool (part of the mockup loader) to _temporarily_ switch source of unit test to a file target for the _current session_. This is **recommended** way in fact as it ensures that only working version is uploaded to the system and _gives possibility for different people to work on different parts of unit test (!)_. 

For more details see documentation of the chosen tool (READMEs in the dedicated repositories).
