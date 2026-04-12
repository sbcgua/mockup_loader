---
sidebar_position: 10
---

# Introduction

AJson is a JSON parser/serializer for ABAP designed with the convenience for developer in mind. It works with ABAP release 7.02 or higher.

## Features

- Parse JSON content into a flexible form, not fixed to any predefined data structure, allowing to modify the parsed data, selectively access its parts and slice subsections of it
  - slicing can be particularly useful for REST header separation e.g. `{ "success": 1, "error": "", "payload": {...} }` where 1st level attrs are processed in one layer of your application and payload in another (and can differ from request to request)
- Allows conversion to fixed ABAP structures/tables (`to_abap`)
- Convenient interface to manipulate the data - `set( value )`, `set( structure )`, `set( table )`, `set( another_instance_of_ajson )`, also typed e.g. `set_date`
  - also `setx` for text-based value setting like `setx( '/a/b:123' )` (useful e.g. for constants in APIs or in unit-tests)
- Seralization to string
- Freezing (read only) instance content
- Filtering - create a JSON skipping empty values, predefined paths, or your custom filter.
- Mapping - rule-based changing node names (e.g. snake case to camel case, upper/lower case)
- Iterating (conveniently) through the array items or object members
- Utility to calculate difference between 2 JSONs
- Supports: timestamps
- Supports: data reference initialization

## Example of usage

```abap
data r type ref to zif_ajson.
data fragment type ref to zif_ajson.

r = zcl_ajson=>parse( '{"success": 1, "error": "", "payload": {"text": "hello"}}' ).

r->get( '/success' ).              " returns "1"
r->get_integer( '/success' ).      " returns 1 (number)
r->get_boolean( '/success' ).      " returns "X" (abap_true - because not empty)
r->get( '/payload/text' ).         " returns "hello"

r->members( '/' ).                 " returns table of "success", "error", "payload"

fragment = r->slice( '/payload' ).
fragment->get( '/text' ).          " returns "hello" (the root has changed)

fragment->set(
  iv_path = '/text'
  iv_val  = 'new text' ).
fragment->stringify( ).            " {"text":"new text"}
```

See more examples and usages in the further docs.
