# TRIVIAL-PROJECT

A Common Lisp tool for creating new project directories populated with files.  


## KEY-VALUE SUBSTITUTION

In addition to copying files from the template directory, TRIVIAL-PROJECT will substitute specially-tagged keys with values inside text files -- and even filenames.

The syntax is simple: keys and values are obtained directly from the invocation of `(make-project :name "test" :SOMEKEY somevalue ...)`; any occurrences of the string --SOMEKEY-- inside the files will be replaced with the value.

Filenames are likewise subject to substitution.  For portability, the keys are tagged `TP_SOMEKEY_TP` in filenames; so the template file "TP_SYSTEM_TP.asd" will be named "test.asd" in the new project.


## INSTALLATION

Clone the repo at https://github.com/stacksmith/trivial-project.git to your local project directory.

```
(ql:quickload :trivial-project)
(tp:make-project :NAME "projname" )
```
This will create a simple project based on the 'lame' template that comes with this repo. 

Follow the REPL instructions to create a custom template directory, and enter your local information in the `.local.txt` file.

## USEFUL KEYS

The :NAME key is required.  The other keys used in the default template are:

`SYSTEM` - the name of the asdf system; defaults to name;
`PACKAGE` - the name of the package; defaults to name;
`AUTHOR` - set in .local.txt
`LICENCE` - set in .local.txt

Feel free to add any keys you deem necessary (and change .local.txt to initialize them to useful values).

## NOTES

### Recursive substitution

The key-name substitution is done repeatedly until no keys are left.  It is possible to expand keys to other keys, but be careful to avoid circularity as it will lock up the system.

### WORK IN PROGRESS




