%% File: copernicus2.cls
%% ---------------------------------
%% The LaTeX2e class file for journals published by Copernicus Publications (Copernicus GmbH)
%% E-mail:     publications@copernicus.org
%% Internet:   http://publications.copernicus.org
%% ---------------------------------
%% Author:
%% Dieter Schmitt
%% Max-Planck-Institut f\"ur Sonnensystemforschung
%% Max-Planck-Str. 2
%% D-37191 Katlenburg-Lindau
%% Germany
%% E-mail: schmitt@mps.mpg.de
%% ---------------------------------
%% This file may be used for non-profit purposes. 
%% It may not be distributed in exchange of money.
%% The author provides it 'as is' and does not
%% guarantee it in any way.
%% ---------------------------------
%% This class file consists of several modules, each with its separate copyright
%% The modules that are specific to Copernicus publications are
%% Copyright (C) 2007-2008 Copernicus GmbH
%%
%% The entire package is available from
%% http://publications.copernicus.org/LaTeX_submission.html
%% ---------------------------------
%% Version 2.5, 2008/07/24

\NeedsTeXFormat{LaTeX2e}[1995/06/01]
\ProvidesClass{copernicus2}[2008/07/24 2.5 (DS)]
\def\clsversion{2.5}

\newif\ifsapiens
\newif\ifasr
\newif\ifdwes
\newif\iffacts
\newif\ifessd

\newif\if@nohyperref
\newif\if@noauthor
\newif\if@noref
\newif\if@nolastpage
\newif\if@cop@home
\newif\if@bar
\newif\if@twostagejnl

\DeclareOption{sapiens}{\sapienstrue\@bartrue\@twostagejnlfalse}
\DeclareOption{asr}{\asrtrue\@bartrue\@twostagejnlfalse}
\DeclareOption{dwes}{\dwestrue\@barfalse\@twostagejnltrue}
\DeclareOption{facts}{\factstrue\@barfalse\@twostagejnlfalse}
\DeclareOption{essd}{\essdtrue\@barfalse\@twostagejnltrue}

\DeclareOption{nohyperref}{\@nohyperreftrue}
\DeclareOption{noauthor}{\@noauthortrue}
\DeclareOption{noref}{\@noreftrue}
\DeclareOption{nolastpage}{\@nolastpagetrue}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions

\LoadClass[fleqn,twoside,twocolumn]{article}

\IfFileExists{copernicuslogo.pdf}{\@cop@hometrue}{\@cop@homefalse}

\if@cop@home
   \if@bar
      \paperheight=283mm
      \paperwidth=213mm
   \else
      \paperheight=277mm
      \paperwidth=210mm
   \fi  
\else
   \paperheight=277mm
   \paperwidth=210mm
\fi

\ifx\pdfoutput\undefined
   \IfFileExists{graphicx.sty}{\RequirePackage[dvips]{graphicx}% LaTeX
      \DeclareGraphicsExtensions{.eps,.ps}}{%
      \ClassWarningNoLine{copernicus2}{Cannot find graphicx.sty; proceeding without it}}
   \RequirePackage[dvips]{color}
\else
   \ifnum\pdfoutput=0
      \IfFileExists{graphicx.sty}{\RequirePackage[dvips]{graphicx}% LaTeX
         \DeclareGraphicsExtensions{.eps,.ps}}{%
         \ClassWarningNoLine{copernicus2}{Cannot find graphicx.sty; proceeding without it}}
      \RequirePackage[dvips]{color}
   \else
      \IfFileExists{graphicx.sty}{\RequirePackage[pdftex]{graphicx}% pdfLaTeX
         \DeclareGraphicsExtensions{.pdf,.png,.jpg}}{%
         \ClassWarningNoLine{copernicus2}{Cannot find graphicx.sty; proceeding without it}}
      \RequirePackage[pdftex]{color}
      \pdfinfo{/Creator (copernicus2.cls version \clsversion)}
      \if@cop@home
         \if@bar
            \pdfpagewidth=213mm
            \pdfpageheight=283mm
         \else
            \pdfpagewidth=210mm
            \pdfpageheight=277mm
         \fi
      \else
         \pdfpagewidth=210mm
         \pdfpageheight=277mm
      \fi
   \fi
\fi

\RequirePackage{authblk}

\RequirePackage[authoryear,round]{natbib}
\def\NAT@sort{0}\def\NAT@cmprs{0}
\renewcommand\NAT@sep{;} \renewcommand\NAT@cmt{, }
\renewcommand\NAT@aysep{,} \renewcommand\NAT@yrsep{,}
\setlength{\bibsep}{\z@}
\let\bibfont\small
\NAT@numbersfalse
\NAT@set@cites
\let\NAT@set@cites\relax
\newcommand{\urlprefix}{}

\IfFileExists{txfonts.sty}{\RequirePackage{txfonts}}{%
   \ClassWarningNoLine{copernicus2}{Cannot find txfonts.sty; proceeding without it}}

\IfFileExists{amssymb.sty}{\RequirePackage{amssymb}}{%
   \ClassWarningNoLine{copernicus2}{Cannot find amssymb.sty; proceeding without it}}

\if@cop@home\if@nohyperref\else
   \usepackage[pdftex,bookmarks=false,colorlinks]{hyperref}
   \hypersetup{anchorcolor=black,citecolor=black,filecolor=black,linkcolor=black,%
      menucolor=black,pagecolor=black,urlcolor=black}
\fi\fi

\IfFileExists{url.sty}{\RequirePackage{url}\urlstyle{same}}%
   {\ClassWarningNoLine{copernicus2}{Cannot find url.sty; proceeding without it}%
   \newcommand{\url}[1]{\ClassError{copernicus2}%
   {To use \string\url, you must have url.sty}{Install url.sty}}}
   
\IfFileExists{textcomp.sty}{\RequirePackage{textcomp}\let\permil\textperthousand}%
   {\ClassWarningNoLine{copernicus2}{Cannot find textcomp.sty; proceeding without it}%
   \newcommand{\permil}{\ensuremath{\kern-.05em%
   \raise.5ex\hbox{\the\scriptfont0 0}%
   \kern-.15em/\kern-.15em\lower.25ex\hbox{\the\scriptfont0 00}}}%
   \def\textmu{\ensuremath{\mu}}}
   
\newcommand{\@journalname}{Journalname}
\newcommand{\@journalurl}{www.jn.net}
\newcommand{\@journalcopyright}{Copyright}
\newcommand{\@journallogo}{Journallogo}
\newcommand{\@sentence}{Sentence.}
\definecolor{textcol}{cmyk}{0.0,0.00,0.00,1.00}
\definecolor{bgcol}{cmyk}{0.00,0.00,0.00,0.00}
%%
\ifsapiens
   \renewcommand{\@journalname}{Surv. Perspect. Integr. Environ. Soc.}
   \renewcommand{\@journalurl}{www.surv-perspect-integr-environ-soc.net}
   \renewcommand{\@journalcopyright}{\copyright\ Author(s)\ \@pyear. This work is distributed under\\the Creative Commons Attribution 3.0 License.}
   \renewcommand{\@journallogo}{\parbox[t]{57mm}{\vskip-5.5mm\includegraphics[width=60mm]{Logo_Institut_VEOLIA.pdf}}}
   \renewcommand{\@sentence}{Published by Copernicus Publications on behalf of the Institut Veolia Environnement.}
   \definecolor{textcol}{cmyk}{0.17,0.95,0.76,0.05}
   \definecolor{bgcol}{cmyk}{0.00,0.00,0.20,0.00}
\fi
\ifasr
   \renewcommand{\@journalname}{Adv. Sci. Res.}
   \renewcommand{\@journalurl}{www.adv-sci-res.net}
   \renewcommand{\@journalcopyright}{\copyright\ Author(s)\ \@pyear. This work is distributed under\\the Creative Commons Attribution 3.0 License.}
   \renewcommand{\@journallogo}{%
      \parbox[t][\z@][t]{40.0mm}{\vskip-3.0mm\includegraphics[width=13.0mm]{copernicuslogo.pdf}\\[-18.5mm]
      \null\hspace{0.5mm}\rule{5mm}{1pt}\hspace{7mm}\rule{27.5mm}{1pt}\\[-0.4mm]
      {\fontencoding{T1}\fontfamily{phv}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
      \null\hfill\scalebox{1.0}[1.0]{Advances in}\\
      \null\hfill\scalebox{1.0}[1.0]{Science and}\\
      \null\hfill\scalebox{1.0}[1.0]{Research}}\\[-2.9mm]
      \null\hspace{0.5mm}\rule{2mm}{1pt}\hspace{10mm}\rule{27.5mm}{1pt}}}
   \renewcommand{\@sentence}{Published by Copernicus Publications.}
   \definecolor{textcol}{cmyk}{1.00,0.96,0.32,0.23}
   \definecolor{bgcol}{cmyk}{0.15,0.15,0.50,0.00}
\fi
\ifdwes
   \renewcommand{\@journalname}{Drink. Water Eng. Sci.}
   \renewcommand{\@journalurl}{www.drink-water-eng-sci.net}
   \renewcommand{\@journalcopyright}{\copyright\ Author(s)\ \@pyear. This work is distributed under\\the Creative Commons Attribution 3.0 License.}
   \renewcommand{\@journallogo}{%
      \parbox[t][\z@][t]{50.5mm}{\vskip-6.5mm
      \null\rule{50.5mm}{1pt}\\[0.1mm]
      {\fontencoding{T1}\fontfamily{phv}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
      \null\hfill\scalebox{1.0}[1.0]{Drinking Water}\\[0.5mm]
      \null\hfill\scalebox{1.0}[1.0]{Engineering and Science}}\\[-2.4mm]
      \null\rule{50.5mm}{1pt}}}
   \renewcommand{\@sentence}{Published by Copernicus Publications on behalf of the Delft University of Technology.}
   \definecolor{textcol}{cmyk}{0.00,0.00,0.00,1.00}
   \definecolor{bgcol}{cmyk}{0.00,0.00,0.00,0.00}
\fi
\iffacts
   \renewcommand{\@journalname}{Field Actions Sci. Rep.}
   \renewcommand{\@journalurl}{www.field-actions-sci-rep.net}
   \renewcommand{\@journalcopyright}{\copyright\ Author(s)\ \@pyear. This work is distributed under\\the Creative Commons Attribution 3.0 License.}
   \renewcommand{\@journallogo}{\parbox[t]{22mm}{\vskip-3.0mm\includegraphics[width=23mm]{FACTS_Logo.pdf}}}
   \renewcommand{\@sentence}{Published by Copernicus Publications on behalf of the Institut Veolia Environnement.}
   \definecolor{textcol}{cmyk}{0.13,1.00,0.71,0.03} 
   \definecolor{bgcol}{cmyk}{0.00,0.00,0.00,0.00}   
\fi
\ifessd
   \renewcommand{\@journalname}{Earth Syst. Sci. Data}
   \renewcommand{\@journalurl}{www.earth-syst-sci-data.net}
   \renewcommand{\@journalcopyright}{\copyright\ Author(s)\ \@pyear. This work is distributed under\\the Creative Commons Attribution 3.0 License.}
   \renewcommand{\@journallogo}{%
     \parbox[t][\z@][t]{44.0mm}{\vskip-3.0mm\includegraphics[width=13.0mm]{copernicuslogo.pdf}\\[-18.5mm]
      \null\hspace{0.5mm}\rule{5mm}{1pt}\hspace{7mm}\rule{31.5mm}{1pt}\\[1.2mm]
      {\fontencoding{T1}\fontfamily{phv}\fontseries{b}\fontshape{n}\fontsize{12}{12}\selectfont
      \null\hfill\scalebox{1.1}[1.1]{Earth System}\\[0.7mm]
      \null\hfill\scalebox{1.1}[1.1]{Science Data}}\\[-1.5mm]
      \null\hspace{0.5mm}\rule{1.5mm}{1pt}\hspace{10.5mm}\rule{31.5mm}{1pt}}}
   \renewcommand{\@sentence}{Published by Copernicus Publications.}
   \definecolor{textcol}{cmyk}{0.87,0.50,0.17,0.15}
   \definecolor{bgcol}{cmyk}{0.00,0.00,0.00,0.00}
\fi
\fboxrule=0.8pt
%\fboxsep=1mm  

\newcommand{\blackbox}{\rule{8mm}{3mm}}
\newcommand{\degree}{\ensuremath{^{\circ}}}
\newcommand{\tophline}{\hline\noalign{\vspace{1mm}}}
\newcommand{\middlehline}{\noalign{\vspace{1mm}}\hline\noalign{\vspace{1mm}}}
\newcommand{\bottomhline}{\noalign{\vspace{1mm}}\hline}
\newcommand{\hhline}{\noalign{\vspace{1mm}}\hline\noalign{\vspace{1mm}}} 
   
\DeclareRobustCommand*{\vec}[1]{\ensuremath{%
   \mathchoice{\mbox{\boldmath$\displaystyle#1$}}
              {\mbox{\boldmath$\textstyle#1$}}
              {\mbox{\boldmath$\scriptstyle#1$}}
              {\mbox{\boldmath$\scriptscriptstyle#1$}}}}

\def\testbx{bx}
\DeclareRobustCommand*{\chem}[1]{\ensuremath{%
   \mathcode`-="0200\mathcode`\=="003D% no space around "-" and "="
   \ifx\testbx\f@series\mathbf{#1}\else\mathrm{#1}\fi}}
\DeclareRobustCommand*{\unit}[1]{\ensuremath{\def\mu{\mbox{\textmu}}\def~{\,}%
   \ifx\testbx\f@series\mathbf{#1}\else\mathrm{#1}\fi}}

\def\acknowname{Acknowledgement}
\def\acknowsname{Acknowledgements}
\def\refname{References}
\def\abstractname{Abstract}
\def\appendixname{Appendix}
\def\figurename{Figure}
\def\tablename{Table}
\def\fnum@figure{\sffamily\color{textcol}\figurename~\thefigure}
\def\fnum@table{\sffamily\color{textcol}\tablename~\thetable}

\def\@normalsize{\@setsize\normalsize{12pt}\xpt\@xpt
   \abovedisplayskip 10\p@ plus2\p@ minus5\p@
   \belowdisplayskip \abovedisplayskip
   \abovedisplayshortskip  \z@ plus3\p@
   \belowdisplayshortskip  6\p@ plus3\p@ minus3\p@
   \let\@listi\@listI}
\def\small{%
   \@setsize\small{11pt}\ixpt\@ixpt
   \abovedisplayskip 8.5\p@ plus3\p@ minus4\p@
   \belowdisplayskip \abovedisplayskip
   \abovedisplayshortskip \z@ plus2\p@
   \belowdisplayshortskip 4\p@ plus2\p@ minus2\p@
   \def\@listi{\leftmargin\leftmargini
      \topsep 4\p@ plus2\p@ minus2\p@\parsep 2\p@ plus\p@ minus\p@
      \itemsep \parsep}}
\let\footnotesize=\small
\def\scriptsize{\@setsize\scriptsize{8pt}\viipt\@viipt}
\def\tiny{\@setsize\tiny{6pt}\vpt\@vpt}
\def\large{\@setsize\large{14pt}\xiipt\@xiipt}
\def\Large{\@setsize\Large{18pt}\xivpt\@xivpt}
\def\LARGE{\@setsize\LARGE{22pt}\xviipt\@xviipt}
\def\huge{\@setsize\huge{25pt}\pt\@xxpt}
\def\Huge{\@setsize\Huge{30pt}\xxvpt\@xxvpt}
 
\normalsize

\if@cop@home
   \if@bar
      \topmargin=-27mm
      \evensidemargin=-6mm
   \else
      \topmargin=-30mm
      \evensidemargin=-9mm
   \fi
\else
   \topmargin=-30mm
   \evensidemargin=-9mm
\fi
\headheight=20mm
\headsep=5mm
\topskip=12pt
\footskip=30pt
\oddsidemargin=-9mm
\textwidth=177mm
\textheight=54\baselineskip
\advance\textheight by \topskip
\columnsep 7mm
\columnseprule 0pt
\parindent=1em
\parskip 0pt plus 1pt

\setcounter{totalnumber}{2}
\setcounter{topnumber}{2}
\setcounter{dbltopnumber}{2}
\def\topfraction{1.}
\def\dbltopfraction{1.}
\setcounter{bottomnumber}{0}
\def\textfraction{0}
\def\floatpagefraction{.7}
\def\dblfloatpagefraction{.7}

%\def\section{\@startsection{section}{1}{\z@}{-3.5ex plus -1ex minus
%   -0.2ex}{2.3ex plus 0.2ex}{\reset@font\sffamily\normalsize\bfseries\color{textcol}}}
%\def\subsection{\@startsection{subsection}{2}{\z@}{-3.25ex plus -1ex
%   minus -0.2ex}{1.5ex plus 0.2ex}{\reset@font\sffamily\normalsize\mdseries\color{textcol}}}
%\def\subsubsection{\@startsection{subsubsection}{3}{\z@}{-3.25ex plus
%   -1ex minus -0.2ex}{1.5ex plus 0.2ex}{\reset@font\sffamily\normalsize\mdseries\color{textcol}}}
\def\section{\@startsection{section}{1}{\z@}{-3.5ex plus -1ex minus
   -0.2ex}{2.3ex plus 0.2ex}{\reset@font\sffamily\normalsize\bfseries\noindent\leavevmode\color{textcol}}}
\def\subsection{\@startsection{subsection}{2}{\z@}{-3.25ex plus -1ex
   minus -0.2ex}{1.5ex plus 0.2ex}{\reset@font\sffamily\normalsize\mdseries\noindent\leavevmode\color{textcol}}}
\def\subsubsection{\@startsection{subsubsection}{3}{\z@}{-3.25ex plus
   -1ex minus -0.2ex}{1.5ex plus 0.2ex}{\reset@font\sffamily\normalsize\mdseries\noindent\leavevmode\color{textcol}}}
\let\paragraph=\subsubsection
\let\subparagraph=\paragraph

\@ifundefined{reset@font}{\let\reset@font=\relax}{}
\setcounter{secnumdepth}{3}

\def\desfont{\sffamily\bfseries\small}
\def\descriptionlabel#1{\hspace\labelsep \desfont #1.}
\def\description{\list{}{\labelwidth\z@ \leftmargin\z@ \itemindent-\leftmargin
   \let\makelabel\descriptionlabel\reset@font\small}}
\let\enddescription\endlist

\newcommand{\introduction}[1][Introduction]{\section{#1}}
\newcommand{\conclusions}[1][Conclusions]{\section{#1}}

\newlength{\nhsize}

%\let\ofigure=\figure 
%\let\endofigure=\endfigure
%\def\figure{\ofigure[tp]\center\vskip2mm\setlength{\nhsize}{\hsize}\addtolength{\nhsize}{-2\fboxsep}\addtolength{\nhsize}{-2\fboxrule}}
%\def\endfigure{\endcenter\vskip-10\p@\endofigure}
%\@namedef{figure*}{\@dblfloat{figure}[tp]\center\vskip2mm\setlength{\nhsize}{\hsize}\addtolength{\nhsize}{-2\fboxsep}\addtolength{\nhsize}{-2\fboxrule}}
%\@namedef{endfigure*}{\endcenter\vskip-10\p@\end@dblfloat}
%
%\let\otable=\table
%\let\endotable=\endtable
%\def\table{\otable[tp]\center\vskip-1mm\setlength{\nhsize}{\hsize}\addtolength{\nhsize}{-2\fboxsep}\addtolength{\nhsize}{-2\fboxrule}}
%\def\endtable{\endcenter\endotable}
%\@namedef{table*}{\@dblfloat{table}[tp]\center\vskip-1mm\setlength{\nhsize}{\hsize}\addtolength{\nhsize}{-2\fboxsep}\addtolength{\nhsize}{-2\fboxrule}}
%\@namedef{endtable*}{\endcenter\end@dblfloat}

\long\def\@makecaption#1#2{\vskip 10\p@{\reset@font\small{\bf #1.} #2\par}} % caption without box
%\long\def\@makecaption#1#2{\vskip 10\p@{\reset@font\small{\bf #1.} #2\par}\vskip 10\p@} % caption without box, use only with above figure/table
%\long\def\@makecaption#1#2{\vskip 10\p@\fcolorbox{bgcol}{bgcol}{\parbox{\nhsize}{\reset@font\small{\bf #1.} #2}\par}\vskip 10\p@} % caption with box

\let\@oxfloat=\@xfloat
\def\@xfloat#1[#2]{\@oxfloat#1[#2]\small}

\sloppy\clubpenalty4000\widowpenalty4000
\leftmargini 2em
\leftmarginii 1.75em
\leftmarginiii 1.5em
\leftmarginiv 1em
\leftmarginv .5em
\leftmarginvi .5em
\def\labelitemi{\bf --}
\def\labelitemii{\bf --}
\def\labelitemiii{$\m@th\bullet$}
\def\labelitemiv{$\m@th\cdot$}

\def\maketitle{%
   \setcounter{footnote}{0}
   \setcounter{page}{\@fpage}
   \thispagestyle{titlepage}
   \setcounter{footnote}{0}%
   \twocolumn[%
      \vspace{25mm}\hspace{10mm}{\begin{minipage}[t]{157mm}
         \null\vfil
         \noindent\begin{minipage}{\hsize}
            \raggedright
            \let\footnotesize\normalsize
            \let\footnoterule\relax
            \def\thefootnote{\fnsymbol{footnote}}%
            \def\@mpfn{footnote}%
            \let\thempfn=\thefootnote
            \renewcommand\@makefntext[1]{%
            \parindent 1em \noindent
            \@makefnmark##1}
            {\sffamily\LARGE\bfseries\center\color{textcol}{\@title}\par}
            \vspace{10pt}
            {\normalsize
               \renewcommand{\Authfont}{\bfseries}%
               \renewcommand{\Affilfont}{\reset@font}%
               \setlength{\affilsep}{4pt}%
               \baselineskip 15pt
               \let\and=\sw@and
               \if@noauthor\else
                  \begin{minipage}[t]{\hsize}\flushleft\baselineskip=12pt%
                     \center\@author
                  \end{minipage}
               \fi
            \par}%
         \end{minipage}
         \if!\@recvd!\else
            \if@twostagejnl
               \par\center{\small Received: \@recvd\ -- Published in \@journalname\ Discuss.: \@pubdiscuss\\[-0.5mm]
               Revised: \@revsd\ -- Accepted: \@accptd\ -- Published: \@published}
            \else         
               \par\center{\small Received: \@recvd\ \if!\@revsd!\else -- Revised: \@revsd\ \fi
               \if!\@accptd!\else -- Accepted: \@accptd\ \fi -- Published: \@published}
            \fi
         \fi
         \if!\@abstr!\else
            \par\vspace{10pt}
            \setlength{\nhsize}{\hsize}\addtolength{\nhsize}{-2\fboxsep}\addtolength{\nhsize}{-2\fboxrule}         
            \fcolorbox{bgcol}{bgcol}{\parbox{\nhsize}{%
               \par\noindent{\reset@font\normalsize\sffamily\bfseries\color{textcol}\abstractname.}\enspace\ignorespaces\@abstr % abstract name left
               %{\vspace{-7.5mm}\center\reset@font\normalsize\sffamily\bfseries\color{textcol}\center\abstractname\\[1.7mm]}\@abstr % abstract name centered
               \if!\@keyw!\else\vspace{1.7mm}\par\noindent{\reset@font\normalsize\sffamily\bfseries\color{textcol} Keywords.}\enspace\ignorespaces\@keyw\fi
            }}
         \fi
         \par\vspace{20pt}
      \end{minipage}%
   }]%
   {\renewcommand\@makefntext[1]{\parindent 1em \noindent ##1}%
   \let\footnotesize\normalsize
   \if@cop@home
      \footnotetext{\vskip0mm\hskip-3mm %
      \if@nohyperref
         \includegraphics[width=2.5cm]{CreativeCommons_Attribution_License.png}
      \else
         \href{http://creativecommons.org/licenses/by/3.0/}{\includegraphics[width=2.5cm]{CreativeCommons_Attribution_License.png}}
      \fi %
      \if!\@corresp!\else\hskip2mm\parbox{57mm}{\vskip-6mm\textit{Correspondence to:} \@corresp}\fi}
   \else
      \if!\@corresp!\else\footnotetext{\textit{Correspondence to:} \@corresp}\fi
   \fi
   \let\footnotesize\small
   \setcounter{footnote}{0}}%
}

\renewcommand\and{\@centercr}
\newcommand\sw@and{\end{minipage}\\[1em]\begin{minipage}[t]{\hsize}%
   \flushleft\baselineskip12pt}
   
\def\bartext#1{\def\@btext{#1}}                        \def\@btext{}

\def\pubyear#1{\if@cop@home\def\@pyear{#1}\fi}         \def\@pyear{\number\year}
\def\pubvol#1{\if@cop@home\def\@pvol{#1}\fi}           \def\@pvol{1}
\def\firstpage#1{\if@cop@home\def\@fpage{#1}\fi}       \def\@fpage{1}

\def\runningauthor#1{\def\@runauth{#1}}                \def\@runauth{}
\def\runningtitle#1{\def\@runtit{#1}}                  \def\@runtit{}
\def\runninghead#1{\def\@runhd{#1}}                    \def\@runhd{\@runauth: \@runtit}

\def\correspondence#1{\def\@corresp{#1}}               \def\@corresp{}

\def\pubdiscuss#1{\if@cop@home\def\@pubdiscuss{#1}\fi} \def\@pubdiscuss{}
\def\received#1{\if@cop@home\def\@recvd{#1}\fi}        \def\@recvd{}
\def\revised#1{\if@cop@home\def\@revsd{#1}\fi}         \def\@revsd{}
\def\accepted#1{\if@cop@home\def\@accptd{#1}\fi}       \def\@accptd{}
\def\published#1{\if@cop@home\def\@published{#1}\fi}   \def\@published{}

\def\lastpage@putlabel{\addtocounter{page}{-1}%
   \immediate\write\@auxout{\string
   \newlabel{LastPage}{{}{\thepage}{}{}{}}}%
   \addtocounter{page}{1}}
\AtEndDocument{\clearpage\lastpage@putlabel}
\def\@lpage{\pageref{LastPage}}

\def\keywords#1{\def\@keyw{#1}}                        \def\@keyw{}
\def\abstract#1{\def\@abstr{#1}}                       \def\@abstr{}
   
\newenvironment{acknowledgements}{\begin{description}\item[\color{textcol}\acknowsname]}{\end{description}}
\newenvironment{acknowledgement}{\begin{description}\item[\color{textcol}\acknowname]}{\end{description}}
   
\def\ps@runninghead{\let\@mkboth\@gobbletwo
   \if@cop@home
      \def\@oddhead{\@runhd\hfil\makebox[\z@][r]{\thepage}
         \if@bar   
            {\setlength{\unitlength}{1.0cm}
            \begin{picture}(0,0)
            \color{bgcol}\put(0.7,0){\rule[-300mm]{14mm}{400mm}}
            \color{textcol}\put(0.7,0){\rule[-300mm]{0.8pt}{400mm}}
            \end{picture}}
         \fi}
      \def\@evenhead{\makebox[\z@][l]{\thepage}\hfil\@runhd
         \if@bar
            {\setlength{\unitlength}{1.0cm}
            \begin{picture}(0,0)
            \color{bgcol}\put(-19.7,0){\rule[-300mm]{14mm}{400mm}}
            \color{textcol}\put(-18.3,0){\rule[-300mm]{0.8pt}{400mm}}
            \end{picture}}
         \fi}
      \def\@oddfoot{\@journalurl\if@noref\else/\@pvol/\@fpage/\@pyear/\fi
         \hfil \@journalname\if@noref\else, \@pvol, \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear\fi}
      \def\@evenfoot{\@journalname\if@noref\else, \@pvol, \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear\fi
         \hfil\@journalurl\if@noref\else/\@pvol/\@fpage/\@pyear/\fi}
   \else
      \def\@oddhead{\@runhd\hfil\makebox[\z@][r]{\thepage}}
      \def\@evenhead{\makebox[\z@][l]{\thepage}\hfil\@runhd}
      \def\@oddfoot{\@journalurl\hfil\@journalname}
      \def\@evenfoot{\@journalname\hfil\@journalurl}
   \fi}
   
\def\ps@titlepage{\let\@mkboth\@gobbletwo
   \if@cop@home
      \def\@oddhead{\parbox[t]{0.5\textwidth}{%
         \@journalname\if@noref\else, \@pvol, \@fpage\if@nolastpage\else{--\@lpage}\fi, \@pyear\fi\\
         \@journalurl\if@noref\else/\@pvol/\@fpage/\@pyear/\fi\\
         \@journalcopyright}
         \hfill\@journallogo
         \if@bar
            {\setlength{\unitlength}{1.0cm}
            \begin{picture}(0,0)
            \color{bgcol}\put(0.7,0){\rule[-300mm]{14mm}{400mm}} 
            \color{textcol}\put(0.7,0){\rule[-300mm]{0.8pt}{400mm}}           
            \put(1,0.3){\rotatebox{270}{\LARGE\itshape\color{textcol}\@btext}}
            \end{picture}}
         \fi}             
      \def\@oddfoot{\@sentence\hfil}
   \else
      \def\@oddhead{\parbox[t]{0.6\textwidth}{%
         Manuscript prepared for \@journalname\\
         with version \clsversion\ of the \LaTeX\ class copernicus2.cls.\\
         Date: \number\day~\ifcase\month\or January\or February\or%
            March\or April\or May\or June\or July\or August\or September\or%
            October\or November\or December\fi~\number\year}}
      \def\@oddfoot{}
   \fi}
   
\pagenumbering{arabic}

\flushbottom
\ps@runninghead

\AtBeginDocument{\mathindent\z@}

%% End of file `copernicus2.cls'.
