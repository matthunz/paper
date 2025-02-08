\documentclass[sigplan,dvipsnames,nonacm]{acmart}\settopmatter{printfolios=true,printccs=false,printacmref=false}

\usepackage{listings}

%include polycode.fmt

\title{Aztecs: An Empirical Entity Component System (ECS) for Haskell}
\author{Matt Hunzinger}
\email{matt@@hunzinger.me}


\begin {document}

\definecolor{dkcyan}{rgb}{0.1, 0.3, 0.3}
\definecolor{dkgreen}{rgb}{0,0.3,0}
\definecolor{olive}{rgb}{0.5, 0.5, 0.0}
\definecolor{dkblue}{rgb}{0,0.1,0.5}
\definecolor{col:ln}{rgb}  {0.1, 0.1, 0.7}
\definecolor{col:str}{rgb} {0.8, 0.0, 0.0}
\definecolor{col:db}{rgb}  {0.9, 0.5, 0.0}
\definecolor{col:ours}{rgb}{0.0, 0.7, 0.0}
\definecolor{lightgreen}{RGB}{170, 255, 220}
\definecolor{darkbrown}{RGB}{121,37,0}

\colorlet{listing-comment}{gray}
\colorlet{operator-color}{darkbrown}

\lstdefinestyle{default}{
    basicstyle=\ttfamily\fontsize{8.7}{9.5}\selectfont,
    columns=fullflexible,
    commentstyle=\sffamily\color{black!50!white},
    escapechar=\#,
    framexleftmargin=1em,
    framexrightmargin=1ex,
    keepspaces=true,
    keywordstyle=\color{dkblue},
    mathescape,
    numbers=none,
    numberblanklines=false,
    numbersep=1.25em,
    numberstyle=\relscale{0.8}\color{gray}\ttfamily,
    showstringspaces=true,
    stepnumber=1,
    xleftmargin=1em
}

\lstdefinelanguage{custom-haskell}{
    language=Haskell,
    deletekeywords={lookup, delete, map, mapMaybe, Ord, Maybe, String, Just, Nothing, Int, Bool},
    keywordstyle=[2]\color{dkgreen},
    morekeywords=[2]{String, Map, Ord, Maybe, Int, Bool},
    morekeywords=[2]{Name, Expression, ESummary, PosTree, Structure, HashCode, VarMap},
    keywordstyle=[3]\color{dkcyan},
    mathescape=false, % so that we can write infix $
    escapechar=\%,    % ... but still have a way to write math between % %
    literate=%
        {=}{{{\color{operator-color}=}}}1
        {||}{{{\color{operator-color}||}}}1
        {\\}{{{\color{operator-color}\textbackslash$\,\!$}}}1
        {.}{{{\color{operator-color}.}}}1
        {=>}{{{\color{operator-color}=>}}}1
        {->}{{{\color{operator-color}->}}}1
        {<-}{{{\color{operator-color}<-}}}1
        {::}{{{\color{operator-color}::}}}1
}

\lstset{style=default}
% Environment for code snippets
\lstnewenvironment{code}[1][]
  {\small\lstset{language=custom-haskell,#1}}
  {}

%include haskell.fmt
\newcommand{\keyword}[1]{\textcolor{BlueViolet}{\textbf{#1}}}
\newcommand{\id}[1]{\textsf{\textsl{#1}}}
\newcommand{\varid}[1]{\textcolor{Sepia}{\id{#1}}}
\newcommand{\conid}[1]{\textcolor{OliveGreen}{\id{#1}}}
\newcommand{\tick}{\text{\textquoteright}}
\newcommand{\package}[1]{\textsf{#1}}

\setlength\mathindent{0em}
\renewcommand{\hscodestyle}{\small}

\begin{abstract}
  An Entity Component System, or ECS,
  is a modern approach to organizing your application state as a database,
  providing patterns for data-oriented design and parallel processing.\par

  An ECS is comprised of three main concepts:
  \begin{itemize}
    \item \textbf{Entities}: An \textit{entity} represents a general-purpose object. In a game engine
          context, for example, every coarse game object is represented as an
          entity.~\cite{ecsWiki} Aztecs represents entities as a unique integer
          identifier, similar to other ECS implementations.
    \item \textbf{Components}: A \textit{component} holds the data for a particular aspect of an entity.
          For example, a zombie entity in a game might have a Health and a Transform component.
    \item \textbf{Systems}: A \textit{system} is a pipeline that processes entities and their components.
  \end{itemize}
\end{abstract}

\maketitle

\section{Introduction}
Aztecs implements an Entity Component System (ECS) in Haskell, providing a type-safe and composable DSL.

\begin{code}
{-# LANGUAGE Arrows #-}

module Aztecs where

import Control.Arrow
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S
\end{code}

\subsection{Components}

Components are the building blocks of entities in an ECS.
In Aztecs, a \texttt{Component} is a typeclass that defines its storage.
By default, a component is stored in a \texttt{IntMap}.

\begin{figure}[H]
\begin{code}
newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity
\end{code}
\caption{Defining a \texttt{Position} and \texttt{Velocity} component}
\end{figure}

\subsection{Queries}
Queries can be used to read and write components in a \texttt{World}.
By taking advantage of arrows, a \texttt{Query} can be composed with Haskell language extensions
or \texttt{Arrow} and \texttt{Applicative} combinators.
\begin{figure}[H]
\begin{code}
query :: (Monad m) => Query m () Position
query = proc () -> do
        Velocity v <- Q.fetch -< ()
        Position p <- Q.fetch -< ()
        Q.set -< Position $ p + v
\end{code}
\caption{Do notation}
\end{figure}

\begin{figure}[H]
\begin{code}
query' :: (Monad m) => Query m () Position
query' =
  Q.fetch 
    &&& Q.fetch
    >>> arr (\(Position p, Velocity v) -> Position $ p + v)
    >>> Q.set
\end{code}
\caption{Arrow combinators}
\end{figure}

\begin{figure}[H]
\begin{code}
query'' :: (Monad m) => Query m () Position
query'' =
  (,)
    <$> Q.fetch
    <*> Q.fetch
    >>> arr (\(Position p, Velocity v) -> Position $ p + v)
    >>> Q.set
\end{code}
\caption{Applicative combinators}
\end{figure}

\subsection{Systems}
Systems can run in two ways:

\subsubsection{Access}

Full \texttt{Access} to the \texttt{World} can be queued to run after a system is complete.

\begin{figure}[H]
\begin{code}
setup :: System () ()
setup = S.queue . const . A.spawn_ 
  $ bundle (Position 0) <> bundle (Velocity 1)
\end{code}
\caption{System that queues access to setup an entity with a \texttt{Position} and \texttt{Velocity} component}
\end{figure}

\subsubsection{Queries}

\begin{figure}[H]
\begin{code}
move :: System () ()
move = S.map query >>> S.run print
\end{code}
\caption{System that queries all \texttt{Position} and \texttt{Velocity} components and applies the update}
\end{figure}

\section{Implementation}

\subsection{Archetypes}

Archetypes are groups of unique component storages.\par

For example, if an entity \textbf{0} has a \textit{Health} component with value
\textbf{100} and \textit{Damage} component with value \textbf{50}, the
components would be stored together in an archetype, and each component would
be stored in its unique storage.

\begin{table}[H]
  \centering
  \caption{Archetype for \textit{Health} and \textit{Damage} components}\label{tab:nested}
  \begin{tabular}{l rrr rrr rrr}
    \toprule
    \textbf{Component}       & \textbf{Entity ID} & \textbf{Value} \\ \midrule
    Health                   &
    \begin{tabular}{l}
      0 \\
      1 \\
    \end{tabular} &
    \begin{tabular}{r}
      100 \\
      50  \\
    \end{tabular}                                        \\
    Damage                   &
    \begin{tabular}{l}
      0 \\
      1 \\
    \end{tabular} &
    \begin{tabular}{r}
      10 \\
      20 \\
    \end{tabular}                                        \\ \bottomrule
  \end{tabular}
\end{table}

Now if we spawn an entity \textbf{1} that has a \textit{Health} component with
value \textbf{50} and \textit{Damage} component with value \textbf{20}, the
matching components would be stored together in the same archetype.

\begin{table}[H]
  \centering
  \caption{Archetype for \textit{Health} and \textit{Damage} components}\label{tab:nested}
  \begin{tabular}{l rrr rrr rrr}
    \toprule
    \textbf{Component} & \textbf{Entity ID} & \textbf{Value} \\ \midrule
    Health             & 0                  & 100            \\
    Damage             & 0                  & 10             \\ \bottomrule
  \end{tabular}
\end{table}

Alternatively, we can insert components into existing entities such as this example
procedure:
\begin{enumerate}
  \item Spawn an entity with a \textit{Health} component
        \begin{table}[H]
          \centering
          \caption{Archetype for \textit{Health} components}\label{tab:nested}
          \begin{tabular}{l rrr rrr rrr}
            \toprule
            \textbf{Component} & \textbf{Entity ID} & \textbf{Value} \\ \midrule
            Health             & 0                  & 100            \\ \bottomrule
          \end{tabular}
        \end{table}
  \item Insert a \textit{Damage} component
        \begin{table}[H]
          \centering
          \caption{Archetype for \textit{Health} and \textit{Damage} components}\label{tab:nested}
          \begin{tabular}{l rrr rrr rrr}
            \toprule
            \textbf{Component} & \textbf{Entity ID} & \textbf{Value} \\ \midrule
            Health             & 0                  & 100            \\
            Damage             & 0                  & 10             \\ \bottomrule
          \end{tabular}
        \end{table}
  \item Spawn another enitity with a \textit{Health} component
        \begin{table}[H]
          \centering
          \caption{Archetype for \textit{Health} components}\label{tab:nested}
          \begin{tabular}{l rrr rrr rrr}
            \toprule
            \textbf{Component}       & \textbf{Entity ID} & \textbf{Value} \\ \midrule
            Health                   & 1                  & 50             \\ \bottomrule
          \end{tabular}
        \end{table}
        \begin{table}[H]
          \centering
          \caption{Archetype for \textit{Health} and \textit{Damage} components}\label{tab:nested}
          \begin{tabular}{l rrr rrr rrr}
            \toprule
            \textbf{Component} & \textbf{Entity ID} & \textbf{Value} \\ \midrule
            Health             & 0                  & 100            \\
            Damage             & 0                  & 10             \\ \bottomrule
          \end{tabular}
        \end{table}
  \item Insert a \textit{Damage} component into entity \textbf{1}
        \begin{table}[H]
          \centering
          \caption{Archetype for \textit{Health} and \textit{Damage} components}\label{tab:nested}
          \begin{tabular}{l rrr rrr rrr}
            \toprule
            \textbf{Component}       & \textbf{Entity ID} & \textbf{Value} \\ \midrule
            Health                   &
            \begin{tabular}{l}
              0 \\
              1 \\
            \end{tabular} &
            \begin{tabular}{r}
              100 \\
              50  \\
            \end{tabular}                                        \\
            Damage                   &
            \begin{tabular}{l}
              0 \\
              1 \\
            \end{tabular} &
            \begin{tabular}{r}
              10 \\
              20 \\
            \end{tabular}                                        \\ \bottomrule
          \end{tabular}
        \end{table}
\end{enumerate}

\bibliographystyle{plain}
\bibliography{references}

\end {document}
