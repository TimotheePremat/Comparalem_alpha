# Comparalem_alpha
  Alpha version of Comparalem, available only for reproductibility of Premat's PhD.
  
## Licence
Comparalem_alpha is distributed under [Etalab Open Licence](https://www.etalab.gouv.fr/licence-ouverte-open-licence/), equivalent to a CC-BY-2.0.

## What is this?
  Comparalem is a set of R scripts I've wrote for my PhD on Medieval French grapho-phonological variation of final schwa.
  
  Comparalem's aim is to detect <e>/Ø (absence or presence of the letter <e>) variation in lemmas in a semi-automatic way, based on lexical request made in TXM onto annotated corpora of texts. It produces various mesures, data tables, graphs and maps of the distribution of one variante's rate.
  
## Why is it an alpha version?
  Comparalem is not ready for a true public release: it can, in fact, be used to study about any graphical variation in any lemma-tagged corpus, but for now, the code is written with the goal of studying only final <e>/Ø variation in one corpus of Old French, the NCA (Stein, Kunstmann & Glessgen 2006). While it is functional as such, it needs optimization, renaming of scripts, variables and objects, etc. Further development will be done later, and then a public version should be released. Making this alpha version available is only meant to match some minimal reproductibility standards for my PhD (note that reproductibility is rarely a concern in the field of historical linguistics).
  
  If you chose to use this alpha version, be prepared to spend a lot a time proof-checking it and its results: I know it works for my uses, but I haven't tested it yet for other analyses. I'm not to be held responsible for any misleading conclusion using it at this early development stage might give you.
  
## What does this repo contains, and what's missing?
  This repo contains a minimal form of Comparalem: the scripts needed to run the programm into an R console, and an example of dataset. More data is available on request, I do not feel like I own the data to the point of not wanting to share it at all. But, as the datasets are still in a form that I understand but that a random user would not necessarily navigate easily, I don't want this reader to get confused with messy data.
  
  Thus, this repo contains:
  - Two main scripts (`MASTER_pre-treatment_R_form_SRC.R` and `MASTER_R_form_distribu_txt`) ;
  - The bunch of other scripts that are called by these main scripts ;
  - An examplary dataset, in this case data for adjectives from the NCA ;
  - A table of metadata for the NCA (included normalised dates and locations of texts) ;
  - Shapefiles used to produce maps (the shapefiles have been made by Guylaine Brun-Trigaud for the [ADE2022](http://atlasdees.unice.fr/wordpress/) project (Scheer et al. 2022), they are based on Dees (1987) regions) ;
  - Graphs, maps and tables that have been produced by running Comparalem onto a subset of data, namely feminine singular adjectives.
  
  What's missing:
  - A bunch of data I've worked on, mostly all non-verbal POS in the NCA corpus ;
  - The graphics, maps and table output I've produced working on these other POS ;
  - Some scripts that I have written but not used in my PhD and that are really too messy to be displayed here.
  
## References
I'll be publising on this programm soon enough, I hope. In the meantime, you can download the slides of a presentation I gave on the day of my 30ts birthday (Premat 2023) by clicking on [this link](https://phonodiachro.hypotheses.org/files/2023/02/Premat_Comparalem_Neuchatel_2023_compressed.pdf). It does not aim at covering all the program aspects or how to use it, but it illustrates one way it can be used to study adverbs' <e>/Ø variation in Old French.
  
### Quoted references
  - Dees, Anthonij (1987). _Atlas des formes linguistiques des textes littéraires de l’ancien français_. Tübingen: M. Neimeyer Verlag.
  - Premat, Timothée (2023). “Comparalem: étudier la variation graphique dans des corpus annotés de français médiéval” [= Comparalem: researching graphic variation in tagged corpora of medieval French]. Workshop _Le traitement numérique des matériaux galloromans : sources anciennes, nouveaux problèmes_, Neuchâtel University, 16/02/2023. [Slides](https://phonodiachro.hypotheses.org/files/2023/02/Premat_Comparalem_Neuchatel_2023_compressed.pdf)
  - Scheer, Tobias, Guylaine Brun-Trigaud, Laurent Vanni, Pierre-Aurélien Georges, Alex Stein, Abdurrahmaan
Iqbal & Timothée Premat (2022). _Atlas Dees Électronique 22._ Côte d’Azur University & CNRS.
Url : [http://atlasdees.unice.fr/wordpress/](http://atlasdees.unice.fr/wordpress/).
  - Stein, Achim, Pierre Kunstmann et Martin-D. Glessgen (2006). _Nouveau Corpus d’Amsterdam. Corpus informatique
de textes littéraires d’ancien français (ca 1150-1350)_. Built by Anthonij Dees (1987), re-built by
Achim Stein, Pierre Kunstmann and Martin-D. Glessgen. Stuttgart: Institut für Linguistik/Romanistik.

