```diff
-quicklisp users: bug :DEPENDS-ON default is not set in the quicklisp distro.
- Please edit the generated .asd file, or to really fix it, edit the TP-SYSTEM-TP.asd template file and make it look like this
+  :depends-on (~~DEPENDS-ON~~)
```
# TRIVIAL-PROJECT

A simple tool for creating new 'projects' populated with files.

Almost every aspect of the project generation is configurable, including key-value substitution of text in files, filename renaming, specification of manifests of file actions, and even the syntax of the substitution engine.  Multipass substitution creates many opportunities for generating complex projects with precision.

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

Follow the REPL instructions to create a custom template directory, and enter your local information in the `.local.tp` file.  This will allow you to customize the projects with your name and preferences.

## GENERAL USAGE

After setting your name/email address and preferred license in .local.tp file, you can just `(tp:make-project :NAME ...)`

Fine tune your template to fit your needs.  Feel free to create different templates for different types of projects; specify the one you need with `:TEMPLATE-PATH` when creating projects.  

Create your own key names (just insert ~~KEYNAME~~ in the text).  Of course, you must either pass the value by hand in `(make-project :NAME xxx :KEYNAME val...)`, or set a useful default in the .local.tp file (or both!).  Same goes for file renaming, with `TP_KEYNAME_TP` syntax in the filename.  If you don't like the ~~xx~~ syntax, change it with `:REGEX-NORMAL`...

Almost every aspect of project generation is configurable.  The rest of this document describes the aspects of configuration.

## KEY-VALUE SUBSTITUTION

In addition to copying files from the template directory, TRIVIAL-PROJECT will replace specially marked text and rename specially named files.

The syntax is simple: keys and values are obtained directly from the invocation of `(make-project :name "test" :SOMEKEY somevalue ...)`; any occurrences of the string `~~SOMEKEY~~` <sup>[1](#myfootnote1)</sup> inside the files will be replaced with the value.

Filenames are likewise subject to substitution.  For portability, the keys are tagged `TP_SOMEKEY_TP`<sup>[1](#myfootnote1)</sup> in filenames; so the template file "TP_SYSTEM_TP.asd" will be named "test.asd" in the new project.

<a name="myfootnote1">1</a>: the tagging syntax is configurable by `:REGEX-NORMAL` and `:REGEX-FILENAME` keys.

## FILE PROCESSING

All files in the template directory and its subdirectories are processed.

Possible actions for each file are `:COPY` `:IGNORE` `:PROCESS`.  Each file in the template directory is processed as follows:

* If it is listed in `:MANIFEST` or file specified by `:MANIFEST-FILE`, the corresponding action is taken; otherwise
* If the extension is specified in in `:EXTENSIONS`, corresponding action is taken; otherwise
* action specified in `:DEFAULT-ACTION` (defaults to `:COPY`) is taken.

Note: the action is determined prior to file renaming.  Use common sense in renaming files (such as not changing the extension).

The manifest is a list of files and file actions.  Files are strings containing the subpath from the template directory.  Only files (not directories) must be listed; directories are created automatically.

## USEFUL KEYS

The :NAME key is required.  The other keys used in the default template are:

 KEY | DEFAULT | COMMENT
 --- | ------- | -------
`:NAME` | | !!! Required; Name of directory of new project
`:SYSTEM` | value of `:NAME` | asdf system name
`:PACKAGE` | value of `:NAME` | package name
`:AUTHOR`  | | set in .local.tp
`:LICENSE` | | set in .local.tp
`:DESCRIPTION` | | !!!
`:DEPENDS-ON` | | !!!
`:DEFAULT-ACTION` | `:COPY` | what to do with unknown files
`:REGEX-FILENAME` | `"TP_(.*?)_TP"` | filenames TP_XXX_TP have `:XXX` key
`:REGEX-NORMAL`   | `"~~(.*?)~~"` | text ~~XXX~~ interpreted as `:XXX` key
`:EXTENSIONS`    | see below | list of extensions and actions
`:MANIFEST` | | optional highest-priority list of files and actions
`:TEMPLATE-PATH` | "~/trivial-project-template/" | pathname of template
`:OUTPUT-PATH` | quicklisp project directory | directory that will contain new project
`:TP-LOCAL-CONFIG-FILENAME` | ".local.tp" | Configuration filename 
`:TP-REGISTER-WITH-ASDF` | T | when T register project path with asdf:*central-registry*


Feel free to add any keys you deem necessary (and change .local.tp to initialize them to useful values).

## NOTES

### Understanding substitution

The key-name substitution is done repeatedly until no keys are left.  It is possible to expand keys to other keys, but be careful to avoid circularity as it will lock up the system.

Keep in mind that substitution, especially from parameters in the `.local.tp` file, involves:

1. The act of Lisp reading the `.local.tp` file using `(read)`.  At this point, values are read in without interpretation; string are strings, symbols are symbols, and lists are lists.

2. Expansion: cl-ppcre expands _strings_; so any expansion-bound values _must_ be strings.  This process takes place _after_ all keywords have been parsed in; therefore any values may include any other keywords (keeping in mind the circularity problem).

If you examine the stock `.local.tp` file you will see that `:SYSTEM` is defined as "~~NAME~~".  This will expand correctly into the value of `:NAME`.  There will be two expansions: `~~SYSTEM~~` into `~~NAME~~`, followed by `~~NAME~~` into the value of `:NAME` in the invocation.

`:DEFAULT-ACTION` however is defined as :COPY.  `:DEFAULT-ACTION` is never expanded - it is an internal symbol only, used to configure the expansion engine.

### Reserved Symbols

`:DEFAULT-ACTION` resolves to a symbol and is not expnadable. `:TEMPATE-PATH`, `:OUTPUT-PATH` are used by the system and are not useful (there are better ways to portably get the project path for instance).

Other internal symbols are `:EXTENSIONS` and `:MANIFEST`, which are used to internally map file types and names to actions.  Therefore symbols ~~EXTENSIONS~~ and ~~MANIFEST~~ should never be used inside text (or at least I cannot think of why you would want to introspect on TRIVIAL-PROJECT itself)

### File Renaming

As mentioned before, the (default but changeable) syntax for keys in filenames to be renamed is TP_xxx_TP, in order to work with the file systems (dashes in filenames are problematic).  In case of a multipass renaming, keep in mind that intermediate names must be in the 'normal' syntax. The final result should be a valid filename and intermediate results are not relevant.

As an example, the stock template has a file named `TP_SYSTEM_TP.asd`.  It will expand to `~~NAME~~.asd` and finally to the value of `:NAME` with extension .asd, unless `:SYSTEM` has been explicitly set. 

### Multipass Renaming

Each string, be it a filename or the contents of a file, is subject to multipass renaming.  It will continue to be processed until no keys are found in the file.  

Note: only the first pass in filenames uses the filename syntax; from second pass on, the standard syntax is used.  This merges the keywords for both filenames and normal string expansion

### Local configuration file and security

`TRIVIAL-PROJECT` _reads_ the `.local.tp` configuration file, which presents some security risks.  In practice, the risk is low; by using any system that generates Lisp files you are indicating some trust in it.  The configuration file is placed by you into your local home directory which is subject to your security settings.  Finally, this library generates stubs of your project, and chances are that you _will_ look at the files it generates before compiling and running the generated project sight unseen.

But you are warned; as with all security related details, you must be vigilant.

### Bluesky projects

There is no particular reason to limit yourself to using `TRIVIAL-PROJECT` for lisp projects, as long as you keep in mind the reserved symbols and circulatiry issues.

### WORK IN PROGRESS




