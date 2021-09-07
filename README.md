sequitur is a "language" created for outlining Book 8 of Thomas Aquinas' Commentary on Aristotle's _Physics_. Rendering coming soon.

The data is organized into a list of sections (which are the section numbers in Thomas' text). 
Each section contains one or more statements.
Each statement is of the form:
  $name := proposition ?{$premise-1 $premise-2 ... $premise-n} <notes>.

1. `$name := proposition` assigns name  `$name` to the string `proposition`.
2. Curly braces enclose a list of premises:
   1. +{...} means the proposition depends on ALL the listed premises to be true ([$premise-1 AND $premise-2 AND ... AND $premise-n] implies proposition)
   2. |{...} means the proposition depends on one of the listed premise to be true ([$premise-1 OR $premise-2 OR ... OR $premise-n] implies proposition)
3. Each premise is of the form $premise or ~$premise.
  1. $premise means it is a necessary premise
  2. ~$premise means it is a probable premise: the reasoning is not rigorous, but it's relevant and probably true.
4. Angle brackets enclose notes, which are not part of Thomas' text.

