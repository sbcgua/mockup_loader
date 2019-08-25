For significant changes, please first discuss the change you wish to make with the owners of this repository via an issue. This would allow for discussing possible solutions and improve searchability.

Also please kindly follow the following guidelines:

- respect the naming conventions in the code and code style in general. In particular: indentation, keywords in **lower case**, etc ... (exception is declaration part of classes which may be reformatted automatically with SE80)
- respect the name-spacing. e.g. call new objects `z??_mockup_loader_***`
- please create pull requests on top of the latest code, rebase your changes
- please add meaningful descriptions to commit/PRs
- please maintain the documentation appropriately e.g. update the README.md / REFERENCE.md with details of changes
- please kindly restrain from changing the version unless agreed with the owners of this repository
- please keep pull requests small and focused on one issue. Don't mix formatting fixes with functional changes
- keep functions/methods short and focused on one task (NASA style, < 60 lines of code is a good example)

Variable naming

- method params should be prefixed with 'I', 'E', 'C', 'R' for importing, exporting, changing, returning respectively
- local variables should be generally prefixed as 'LT' for tables, 'LS' for structures, 'LV' for values, e.g. `lt_lines`
- iterators (field-symbols) however should not have prefixes e.g. `<line>`
- meaningful names are good even if they are long. Iterators can exceptionally have short names (`<i>`)

Other considerations

- Functional programming approach is welcomed. In particular use static side-effect-less methods where appropriate. They are easier to test and less bug-prone.
- Avoid undefined state, use asserts/exceptions, code should self protect itself
- Code should be as self descriptive as possible - use meaningful variable and function/method names
- Cover code with unit tests
