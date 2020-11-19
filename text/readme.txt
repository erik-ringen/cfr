The text folder contains all files necessary to compile the text of the paper.

-main.Rtex: contains the preamble and collates together the sections

-/sections: contains the .tex files for each section. If r script is embedded in the text, the files are .Rtex and have an initial r chunk to load the relevand files

-/data: contains the .csv files created by code elsewhere in the folder and other data relevant for compiling the text of the paper.

-/images: contains images created with .csv from /data which are used in the text.
-/images/make_images.R: r script creating the images in the same folder.

-references.bib: is the bibtex file imported from Zotero with all the references for the text.
