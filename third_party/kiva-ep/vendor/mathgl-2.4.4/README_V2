README for changes from v.1.*

There are few key changes at end-user level.

1. The structure of library is changed sufficiently.
   There are 4 levels now:

(a) Developer level (for people who want to improve library) contain
    classes for base plotting functions:

mglBase --> mglCanvas --> mglCanvasW  --> mglCanvasQT, mglCanvasFL
               |
               ---------> mglCanvasGL --> mglCanvasGLUT

(b) C&Fortran interface for plotting and data handlinng functions

(c) Unified "inline" classes (mglGraph, mglData, mglWindow, ...), which
    contain only inline members, which call C-functions. This make
    MathGL completely cross-platform, i.e. the same binary files can be
    used in any compiler (MinGW, MSVS, Borland, ...). Moreover it give
    the same classes for all interfaces (Python/Octave/...).

(d) MGL scripts use mglGraph's functions but give some extra
    capabilities for axis setup or textual plot editing

2. There is unified class mglGraph instead of set of mglGraphZB,
   mglGraphPS, ... The quality<->speed level of plot is specified by
   SetQuality() function.

3. There is abstract class mglDataA which allow to derive classes with
   its own data representation. However, I recommend to use mglData for
   usual cases -- it have large set of built in functions.

4. All plotting functions now have unified interface for arguments:
   data objects or numbers; string with style; string with options.
   Options (the same as ones in MGL v.1.*) allows one to make fine
   tuning of plot. I.e. easily set the range of coordinates, plot
   position, font size and so on.

5. The last big change is style representation. There are differences
   for colors and for text style:

*  Colors may have "brighted" version everywhere -- just specify '{cN}'
   instead of 'c'. Here 'c' is base color id, 'N' is digit 1,2...9 for
   brightness. Also the transparency can be changed by adding '{AN}'.

*  Text styles is reverted now -- to be unified with any other styles.
   Color is placed firstly, then separator ':', and after it font styles
   (bold, italic, wire, ...).

Also there are a lot of other (not so general) changes and improvements.
