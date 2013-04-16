MLDataVisualizer
----------------
A SML program for visualizing data structures in SML.

**Compiling with MLton and the ML Basis system:**

See http://www.mlton.org/ManualPage and http://www.mlton.org/MLBasis for additional information.

    $ mlton mldv.mlb


**Example usage (while in this phase of the project):**

    $ ./mldv "val a = (1234, 3.14) datatype tree = Node of tree * int * tree | Null"

Make sure that the compiled program is executable (it probably already is if you compiled it):

    $ chmod +x ./mldv
