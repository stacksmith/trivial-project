# TRIVIAL-PROJECT

A simple tool for creating new 'projects' populated with files.

Almost every aspect of the project generation is configurable, including key-value substitution of text in files, filename renaming, specification of manifests of file actions, etc.

## DEPENDENCIES

alexandria, cl-ppcre

## LICENSE

BSD 3-clause license

## INSTALLATION

Clone the repo at https://github.com/stacksmith/trivial-project.git to your local project directory.

```
(ql:quickload :trivial-project)
(tp:make-project :NAME "projname" )
```
This will create a simple project based on the 'lame' template that comes with this repo. 

Follow the REPL instructions to create a custom template directory, and enter your local information in the `.local.tp` file.

## KEY-VALUE SUBSTITUTION

In addition to copying files from the template directory, TRIVIAL-PROJECT will substitute specially-tagged keys with values inside text files -- and even filenames.

The syntax is simple: keys and values are obtained directly from the invocation of `(make-project :name "test" :SOMEKEY somevalue ...)`; any occurrences of the string --SOMEKEY--<sup>[1](#myfootnote1)</sup> inside the files will be replaced with the value.

Filenames are likewise subject to substitution.  For portability, the keys are tagged `TP_SOMEKEY_TP`<sup>[1](#myfootnote1)</sup> in filenames; so the template file "TP_SYSTEM_TP.asd" will be named "test.asd" in the new project.

<a name="myfootnote1">1</a>: the tagging syntax is configurable by `:REGEX-NORMAL` and `:REGEX-FILENAME` keys.

## FILE PROCESSING

Possible actions for each file are `:COPY` `:IGNORE` `:PROCESS`.  Each file in the template directory is processed as follows:

* If it is listed in `:MANIFEST` or file specified by `:MANIFEST-FILE`, the corresponding action is taken; otherwise
* If the extension is specified in in `:EXTENSIONS`, corresponding action is taken; otherwise
* action specified in `:DEFAULT-ACTION` (defaults to `:COPY`) is taken.


## USEFUL KEYS

The :NAME key is required.  The other keys used in the default template are:

 KEY | DEFAULT | COMMENT
 --- | ------- | -------
`:SYSTEM` | value of `:NAME` | asdf system name
`:PACKAGE` | value of `:NAME` | package name
`:AUTHOR`  | | set in .local.tp
`:LICENSE` | | set in .local.tp
`:DEFAULT-ACTION` | `:COPY` | what to do with unknown files
`:REGEX-FILENAME` | `"TP_(.*?)_TP"` | filenames TP_XXX_TP have `:XXX` key
`:REGEX-NORMAL`   | `"--(.*?)--"` | text --XXX-- interpreted as `:XXX` key
`:EXTENSIONS`    | see below | list of extensions and actions
`:MANIFEST-FILE` | ".manifest.tp" | optional file containing filenames and actions
`:MANIFEST` | | optional highest-priority list of files and actions

Feel free to add any keys you deem necessary (and change .local.tp to initialize them to useful values).

## NOTES

### Recursive substitution

The key-name substitution is done repeatedly until no keys are left.  It is possible to expand keys to other keys, but be careful to avoid circularity as it will lock up the system.

### WORK IN PROGRESS




