\documentclass[sigplan,dvipsnames,nonacm]{acmart}\settopmatter{printfolios=true,printccs=false,printacmref=false}

%include lhs2TeX.fmt

\title{Aztecs: An Empirical Entity Component System (ECS) for Haskell}
\author{Matt Hunzinger}
\email{matt@@hunzinger.me}

\begin {document}

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
import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

setup :: System () ()
setup = S.queue . const . A.spawn_ $
    bundle (Position 0) <> bundle (Velocity 1)

move :: System () ()
move =
    S.map
    ( proc () -> do
        Velocity v <- Q.fetch -< ()
        Position p <- Q.fetch -< ()
        Q.set -< Position $ p + v
    )
    >>> S.run print

main :: IO ()
main = runSystem_ $ setup >>> S.forever move
\end{code}

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
