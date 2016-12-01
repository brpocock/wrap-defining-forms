# wrap-defining-forms
Wrap defining forms so that the source of the definitions may be preserved

Inspired by this Stack Overflow question (and composed as an answer):

https://stackoverflow.com/questions/40902449/view-or-extract-the-text-definitions-entered-into-the-top-level-repl-ideally-fo

# Question


View or Extract the text definitions entered into the top level REPL, ideally for Clozure Common Lisp (CCL)

(asked by: https://stackoverflow.com/users/1078016/%ce%bb )
	
When working in a top level REPL, I sometimes forget what definitions I've entered into the running lisp system.

I use Clozure CL and it provides the option of saving the application as an image, which I do and can continue where I left off, but at this point it becomes impossible to review all the code, unless I had separately typed and saved the code to xyz file already.

Is there a way to get/extract/view the definitions that I entered, so I can save them as a source file?

# Answer

Common Lisp (in general) does not provide any standard way to recover the source code of a definition once it has been compiled.

The Dribble function does allow you to log your REPL session, and you could manually harvest definitions from that.

You could also use a set of “wrapper” macros which store the forms you pass to them in a global hash-table, and then recover the source from that. The below is my attempt to do something like that.

Note that the “source” forms stashed in this way won't preserve reader macros, comments, or the like, and will probably choke on some things like defmethod in subtly horrible ways.

That being said, here's how I threw something together that kinda does what you're describing…
