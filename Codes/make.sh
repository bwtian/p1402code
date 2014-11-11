pdflatex -output-directory=build main.tex
cp *.bib build
cd build; bibtex main
cd ..; pdflatex -output-directory=build main.tex
all:
    mkdir out -p
    TEXINPUTS="src:" pdflatex -output-directory out src/parent.tex
    BIBINPUTS="src:" BSTINPUT="out:" TEXMFOUTPUT="out:" bibtex out/parent
    TEXINPUTS="src:" pdflatex -output-directory out src/parent.tex
    TEXINPUTS="src:" pdflatex -output-directory out src/parent.tex
    
    You can use environment variable TEXMFOUTPUT to specify output directory for bibtex files. In your case, you can create makefile like:

all:
    mkdir out -p
    pdflatex -output-directory out parent.tex
    TEXMFOUTPUT="out:" bibtex out/parent
    pdflatex -output-directory out parent.tex
    pdflatex -output-directory out parent.tex
Consider more complicated variant where you have your source files in subdirectory src. Then you will use also variable BIBINPUTS to specify locations of your *.bib files and BSTINPUTS to specify location of your *.bst files (bibliography style). Then the makefile could look like this:

all:
    mkdir out -p
    TEXINPUTS="src:" pdflatex -output-directory out src/parent.tex
    BIBINPUTS="src:" BSTINPUT="out:" TEXMFOUTPUT="out:" bibtex out/parent
    TEXINPUTS="src:" pdflatex -output-directory out src/parent.tex
    TEXINPUTS="src:" pdflatex -output-directory out src/parent.tex
    
    If you are using TexStudio, there is a simple way to do it. In Preferences/Commands, you can add to the BibTeX line build/%.aux if your output folder for PdfLaTeX is named build.

This is working for me with my_bibliography.bib in the same folder as my LaTeX files and using the following at the end of my LaTeX file.

\bibliographystyle{plain}
\bibliography{bibliography}
I guess you can find similar settings for other editors/compilers.

Hope this helps !
