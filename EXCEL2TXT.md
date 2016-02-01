# Excel to TXT conversion script #

We have a lot of data prepared in Excel files. Many files, many sheets in each. It is boring and time consuming to copy them all to text (although Ctrl+C in Excel actually copies TAB delimited text which greatly simplifies the matter for small cases).

So we created a VB script which does this work automatically. 

## How to use works ##

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

## Command line parameters ##

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
- `-nz` - skip archiving, just generate text files to 'uncompressed' dir
- `-bd` - change build directory - where uncompressed folder is created
- `-color` - output in color (requires [ANSICON](https://github.com/adoxa/ansicon))  

