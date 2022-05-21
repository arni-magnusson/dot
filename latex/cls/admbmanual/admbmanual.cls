%%
%% This is file `admbmanual.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% admbmanual.dtx  (with options: `class')
%% 
%% This is a generated file.
%% 
%% Copyright (C) 2009 by Dangerous Curve <typesetting@dangerouscurve.org>
%% 
%% This file may be distributed and/or modified under the conditions of
%% the LaTeX Project Public License, either version 1.2 of this license
%% or (at your option) any later version.  The latest version of this
%% license is in:
%% 
%%    http://www.latex-project.org/lppl.txt
%% 
%% and version 1.2 or later is part of all distributions of LaTeX version
%% 1999/12/01 or later.
%% 
\NeedsTeXFormat{LaTeX2e}[1999/12/01]
\ProvidesClass{admbmanual}
    [2009/06/01 v1.0 ADMBMANUAL class file]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\makeatletter
\LoadClass[12pt]{book}
\RequirePackage{ifthen}
\RequirePackage[T1]{fontenc}
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{bm}
\RequirePackage{relsize}
\RequirePackage{fancyhdr}
\RequirePackage{tocbibind}
\RequirePackage{makeidx}
\RequirePackage{multicol}
\RequirePackage{fancyvrb}
\RequirePackage{listings}
\RequirePackage{graphicx}
\RequirePackage{caption}
\RequirePackage{wrapfig}
\RequirePackage{rawfonts}
\RequirePackage{pictexwd}
\RequirePackage{etex}
\RequirePackage{xy}
\RequirePackage{graphics}
\RequirePackage[usenames]{color}
\RequirePackage[pagebackref=true,linktocpage=true,colorlinks=true]{hyperref}
\RequirePackage[all]{hypcap}
\RequirePackage{chappg}
\def\@makechapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright \normalfont
    \ifnum \c@secnumdepth >\m@ne
        \huge\bfseries \@chapapp\space \thechapter
        \par\nobreak
        \vskip 20\p@
    \fi
    \interlinepenalty\@M
    \Huge \bfseries \boldmath #1\par\nobreak
    \vskip 40\p@
  }
}
\def\@makeschapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright
    \normalfont
    \interlinepenalty\@M
    \Huge \bfseries \boldmath #1\par\nobreak
    \vskip 40\p@
  }
}
\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\Large\bfseries\boldmath}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\large\bfseries\boldmath}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-3.25ex\@plus -1ex \@minus -.2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\bfseries\boldmath}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {3.25ex \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize\bfseries\boldmath}}
\renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize\bfseries\boldmath}}
\RequirePackage[%
    top=1.125truein,
    outer=.875truein,
    inner=1.125truein,
    % textheight=8.5truein, % Allow LaTeX to infer this, both to get
    % textwidth=6.5truein,  % rid of warnings and to allow any page size
    footskip=.875truein
]{geometry}
\let\savetableofcontents=\tableofcontents
\renewcommand{\tableofcontents}{%
  \savetableofcontents
  \mainmatter
}
\newcommand*{\manualname}[1]{\gdef\@manualname{#1}}
\newcommand{\@dopagenumbering}{%
  \pagestyle{fancy}
  \fancyhead{}
  \fancyfoot{}
  \renewcommand{\headrulewidth}{0pt}
  \renewcommand{\footrulewidth}{0pt}
  \fancyfoot[R]{\thepage} % basic one-sided footer, or alternatively:
  % \fancyfoot[LO]{\small admb-project.org}
  % \fancyfoot[RO]{\thepage}
  % \fancyfoot[LE]{\thepage}
  % \fancyfoot[RE]{\small\emph{\@manualname} User's Manual}
}
\@dopagenumbering
\pagenumbering{bychapter}
\renewcommand\chapter{\if@openright\cleardoublepage\else\clearpage\fi%
                   %  \thispagestyle{plain}%
                    \global\@topnum\z@
                    \@afterindentfalse
                    \secdef\@chapter\@schapter
} % end \chapter
\renewcommand\appendix{
   \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \gdef\@chapapp{\appendixname}%
  \gdef\thechapter{\@Alph\c@chapter}}
\definecolor{DarkBlue}{rgb}{0,0.08,0.45}
  \if@titlepage
  \renewcommand\maketitle{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \let \footnote \thanks
  \null\vfil
  \vskip 60\p@
  \begin{center}%
    {\@title \par}%
    \vskip 4em%
    {%
         \Large
       \lineskip .75em%
       \begin{tabular}[t]{c}%
           \@author
         \end{tabular}\par
      }%
       \vfill%
       \hspace{9pt}
            \includegraphics[width=10pc]{ADMB-logo.png}\par
       \vspace{-3.75pc}
      {\large \textcolor{DarkBlue}{\textrm{\textit{\@publisher}}}}\par
     \vskip 1.5em%
     % {\large \@date \par}%       % Set date in \large size.
     %\vspace*{1pc}  % Add extra optical space at the bottom.
     \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
  \clearpage
   \thispagestyle{empty}%
   \IfFileExists{license.tex}
  {%
    \centerline{\Large License}
    \bigskip
    \InputIfFileExists{license.tex}{}{}
    \vfil\null
  }
  {
  }
  \frontmatter
} % End \maketitle
\fi
\newcommand\largetitlepart[1]{%
  %{\sffamily\slshape\bfseries
  {\sffamily\slshape
   \fontsize{36}{36}\selectfont
    #1\par
    \vspace{.5\baselineskip}}%
 }
\newcommand\mediumtitlepart[1]{%
  \medskip
  {%
     \sffamily\slshape
     \fontsize{24}{26}\selectfont
      {#1}\par
    }%
   \bigskip
   \medskip
}
\newcommand\mediumtitlepartnonslanted[1]{%
  \medskip
  {%
     \sffamily
     \fontsize{24}{26}\selectfont
      {#1}\par
    }%
   \bigskip
   \medskip
}
\newcommand\smalltitlepart[1]{%
  \medskip
  {%
     \sffamily\slshape
     \fontsize{18}{20}\selectfont
     {#1}\par
   }%
   \bigskip
   \medskip
}
\newcommand\smalltitlepartnonslanted[1]{%
  \smallskip
  {%
     \sffamily
     \fontsize{18}{20}\selectfont
     {#1}\par
   }%
   \bigskip
   \medskip
}
\newcommand*{\publisher}[1]{\gdef\@publisher{#1}}
\publisher{admb-project.org}
\renewcommand*\l@chapter[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 1.0em \@plus\p@
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries \boldmath
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi
}
\renewcommand\tocbibname{References}
\let\savebibliography=\bibliography
\renewcommand{\bibliography}{%
   \cleardoublepage
   \pagenumbering[\tocbibname]{bychapter}
   \savebibliography
}
\renewcommand*\l@section{\@dottedtocline{1}{1.5em}{3em}}
\newcommand\X[1]{\index{#1}}
\newcommand\XX[2]{\index{#1!#2}}
\newcommand{\fontindexentry}[2]{#2@\csname text#1\endcsname{#2}}
\renewenvironment{theindex}
  {\if@twocolumn
      \@restonecolfalse
   \else
      \@restonecoltrue
   \fi
   \setlength{\columnseprule}{0pt}
   \setlength{\columnsep}{45pt}
   \begin{multicols}{2}[\section*{\indexname}]
   %\markboth{\MakeUppercase\indexname}%
    %        {\MakeUppercase\indexname}%
  % \thispagestyle{plain}
  \addcontentsline{toc}{chapter}{\indexname}
   \setlength{\parindent}{0pt}
   \setlength{\parskip}{0pt plus 0.3pt}
   \relax
   \let\item\@idxitem}%
  {\end{multicols}\if@restonecol\onecolumn\else\clearpage\fi}
\let\saveprintindex=\printindex
\renewcommand{\printindex}{%
   \cleardoublepage
   \@dopagenumbering
   \pagenumbering[Index]{bychapter}
   \saveprintindex
}
\DefineVerbatimEnvironment{code}{Verbatim}{fontsize=\small}
\DefineVerbatimEnvironment{smallcode}{Verbatim}{fontsize=\scriptsize}
\DefineVerbatimEnvironment{tinycode}{Verbatim}{fontsize=\tiny}
\newcommand{\aftercodething}[1]{%
  \unskip
  #1\par
  \medskip
}
 \lstset{language=C++, numbers=none, basicstyle={\ttfamily}, columns=flexible, showstringspaces=false}
\newcommand{\afterlistingthing}[1]{%
  \unskip\vspace{-.5\baselineskip}
  #1\par
  \medskip 
}
\newcommand{\@setupemptycaption}{%
  \captionsetup{labelsep=none, justification=centering}
}
\newcommand{\@setupnonemptycaption}{\captionsetup{labelsep=colon}}
\newcommand{\emptycaption}{%
  \@setupemptycaption
  \caption{}
  \@setupnonemptycaption
}
\newcommand{\@loadrms}{%
\ifx\undefined\eightrm
  \font\eightrm=cmr8
\fi
\ifx\undefined\ninerm
  \font\ninerm=cmr9
\fi
\ifx\undefined\twelverm
  \font\twelverm=cmr12
\fi
\ifx\undefined\eighteenrm
  \font\eighteenrm=cmr17
\fi
}
\@loadrms
\newcommand{\br}{\hfill\break}
\newcommand{\BR}{\hfill\break}
 % |\addcontentsline{file}{sec_unit}{entry}|, e.g.,
\renewcommand\addcontentsline[3]{% toc extension, type, tag
  \begingroup
    \renewcommand\br{\ }
    \let\label\@gobble
    \ifx\@currentHref\@empty
      \Hy@Warning{%
        No destination for bookmark of \string\addcontentsline,%
        \MessageBreak destination is added%
      }%
      \phantomsection
    \fi
    \expandafter\ifx\csname toclevel@#2\endcsname\relax
      \begingroup
        \def\Hy@tempa{#1}%
        \ifx\Hy@tempa\Hy@bookmarkstype
          \Hy@WarningNoLine{%
            bookmark level for unknown #2 defaults to 0%
          }%
        \else
          \Hy@Info{bookmark level for unknown #2 defaults to 0}%
        \fi
      \endgroup
      \expandafter\gdef\csname toclevel@#2\endcsname{0}%
    \fi
    \edef\Hy@toclevel{\csname toclevel@#2\endcsname}%
    \Hy@writebookmark{\csname the#2\endcsname}%
      {#3}%
      {\@currentHref}%
      {\Hy@toclevel}%
      {#1}%
    \ifHy@verbose
      \def\Hy@tempa{#3}%
      \@onelevel@sanitize\Hy@tempa
      \typeout{pdftex: bookmark at \the\inputlineno: %
        {\csname the#2\endcsname}%
        {\Hy@tempa}%
        {\@currentHref}%
        {\Hy@toclevel}%
        {#1}%
      }%
    \fi
    \addtocontents{#1}{%
      \protect\contentsline{#2}{#3}{\thepage}{\@currentHref}%
    }%
  \endgroup
}
\newcommand{\cplus}{C\raise.5ex\hbox{\relsize{-3}++}}
\newcommand{\e}[1]{\ensuremath{\times 10^{#1}}}
\newcommand{\pluseq}{\ensuremath{\:+\kern-.25em=}}
\newcommand{\ttpluseq}{{\tt \protect\raisebox{.107ex}{+}=}}
\newcommand{\minuseq}{\ensuremath{\:-\kern-.25em=}}
\newcommand{\ttminuseq}{{\tt \protect\raisebox{.273ex}{-}=}}
\newcommand{\multiplyeq}{\ensuremath{\:*\kern-.22em=}}
\newcommand{\ttmultiplyeq}{{\tt \protect\raisebox{-.066ex}{*}=}}
\newcommand{\divideeq}{\ensuremath{\:/\kern-.33em=}}
\newcommand{\ttdivideeq}{\texttt{/=}}

\newcommand{\scAB}{\textsc{admb}}
\newcommand{\scAR}{\mbox{\textsc{admb-re}}}
\newcommand{\scAD}{{\textsc{autodif}}}
\newcommand\ADM{AD~Model Builder}
\newcommand\ADMS{AD~Model Builder }
\makeatother
\endinput
%%
%% End of file `admbmanual.cls'.
