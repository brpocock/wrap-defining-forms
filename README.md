# wrap-defining-forms

Wrap  defining  forms  so  that   the  source  of  the  definitions  may
be preserved. Mostly meant for in-REPL development.

Inspired by this Stack Overflow question (and composed as an answer):

https://stackoverflow.com/questions/40902449/view-or-extract-the-text-definitions-entered-into-the-top-level-repl-ideally-fo

# Implementations

This works fairly well in SBCL, and was gently tested in Clozure. I made
a little effort to try in out in Allegro, CLisp, and saw some- Google-Fu
that suggested how it might work in LispWorks.

I ran into  problems getting GCL and  ECL to accept it,  which might not
be solved.

Feel free  to port/fix/improve and send  a Pull Request. I  may actually
use some of this in another project.

# Question


View or Extract the text definitions entered into the top level REPL, ideally for Clozure Common Lisp (CCL)

(asked by: https://stackoverflow.com/users/1078016/%ce%bb )
	
When working  in a top level  REPL, I sometimes forget  what definitions
I've entered into the running lisp system.

I use Clozure CL and it provides the option of saving the application as
an image,  which I do  and can  continue where I  left off, but  at this
point  it becomes  impossible  to  review all  the  code,  unless I  had
separately typed and saved the code to xyz file already.

Is there  a way to get/extract/view  the definitions that I  entered, so
I can save them as a source file?

# Answer (abbreviated)

Common Lisp (in general) does not  provide any standard way to “recover”
the source  code of a  definition once  it has been  compiled. Normally,
it's found in whatever file or buffer you're working-from.

(As Leo's  answer [see the Stack  Overflow post] — points  out, there is
Function-Lambda-Expression,   which   can   give   you   some   function
definitions. It won't help with, say, constants or classes, and it won't
always work — as CLHS  says, “Any implementation may legitimately return
nil     as      the     lambda-expression     of      any     function.”
http://clhs.lisp.se/Body/f_fn_lam.htm — his solution is certainly useful
in the most common case(s), but it is not “as general” as this one.)

You could use a  set of “wrapper” macros which store  the forms you pass
to them in  a global hash-table, and then recover  the source from that.
Writing that sounded like an  interesting little challenge, so the below
is my attempt to do something like that. My Silly Wrappers Solution

Note that the  “source” forms stashed in this way  won't preserve reader
macros, comments,  or the like, and  will probably choke on  some things
like defmethod in  subtly horrible ways. That's because  I blindly store
the definitions keyed off the defining form — eg, defun — and the second
word, only. It's not smart enough to notice if you rebound a function as
a macro or generic function  (all three conflicting definitions would be
saved),  it doesn't  read method  combinations or  lambda lists  to save
various methods,  or any  of that.  There are lots  of other  things you
might do — eg, `(SetF (FDefinition 'FOO) `…`)` — that could bypass these
and go unnoticed, so it's far from “foolproof.” *Caveat Lector*.

The macros here  try to inherit the documentation and  lambda lists from
the underlying forms, so they should work pretty nicely with most IDE's.
They do well enough, in Slime.

One way to work  with these would be to directly call  them; eg, in your
REPL you could directly

     My-Package> (use-package :wrap-defining-form)
     My-Package> (defun$ my-fn (x) (+ x (sqrt x)))

A   more  dangerous/interesting   way   is  provided   in  the   package
Wrap-Defining-Form.Shadowing,  in  which  the  macros  shadow  the  real
Common-Lisp package definitions …

     CL-User> (in-package :CL-USER$)
     CL-User$> (defun blah (n) (loop repeat n do (format t "~&Blah …")))

When you're ready to “save” things, run (dump-definitions).

I wrote and tested  this in SBCL, but tried to take  care that it should
work  on many/most  other  implementations. In  particular,  I used  one
non-ANSI function: SB-Introspect:Function-Lambda-List. I believe Clozure
also  has an  implementation of  Function-Lambda-List, and  it hopefully
works      the     same      way,     so      the     function      here
Wrap-Defining-Form::Find-Function-Lambda-List  will search  all packages
for your  implementation's version  of that function.  If it  can't find
one, all is  not lost; but you  won't get hints from your  IDE about the
lambda-list of the wrapped function.

    CL-USER> (describe 'defun$)
    WRAP-DEFINING-FORM:DEFUN$
      [symbol]
    
    DEFUN$ names a macro:
      Lambda-list: (NAME LAMBDA-LIST &BODY BODY)
      Documentation:
        Wrap `DEFUN' and save the original form.
    
        DEFUN: Define a function at top level.
      Source file: wrap-defining-forms.lisp
    ; No value
    
Without Function-Lambda-List, the wrapper looks like

      Lambda-list: (&REST UNKNOWN-LAMBDA-LIST)

… which is not very helpful.

File Backup

Dump-Definitions    also   will    write   to    a   file.    (It   sets
`:If-Exists  :Rename`,  so  you  could  have  one-level-UNDO  protection
as well.)

