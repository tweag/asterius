% Mkhprog.lhs - Haskell equivalent of mkprog

% @(#)Mkhprog.lhs	2.4 dated 92/06/23 at 15:37:01

% Crown Copyright 1992

\documentstyle[a4wide]{article}

\title{Mkhprog --- a Command line Parser Generator}
\author{N D North\\
	National Physical Laboratory\\
	Teddington, TW11 0LW, UK.\\
	{\tt ndn\@seg.npl.co.uk}}

% Some macros lifted from elsewhere to make this more standalone.
\makeatletter
% INLINE PROGRAM CODE
%
% \prog{foo} sets its argument in typewriter font.
\def\prog#1{\ifmmode\mbox{\tt #1}\else{\tt #1}\fi}

% NEWVERBATIM (from iso.sty)
%
% \newverbatim{foo} creates a new environment, foo, which behaves exactly
% like the verbatim environment except that it is delimited by
% \begin{foo} ... \end{foo}.
% See the VERBATIM section of latex.tex for the inspiration behind this.
%
\def\newverbatim#1{\expandafter\def\csname #1\endcsname{%
\\@verbatim \frenchspacing\\@vobeyspaces \csname \@x#1verbatim\endcsname}
\expandafter\let\csname end#1\endcsname=\endtrivlist
\new\@xverbatim{#1}}

\begingroup \catcode `|=0 \catcode `[= 1
\catcode`]=2 \catcode `\{=12 \catcode `\}=12
\catcode`\\=12
|gdef|new\@xverbatim#1[
|expandafter|def|csname \@x#1verbatim|endcsname##1\end{#1}[##1|end[#1]]]
|endgroup
\makeatother

\newverbatim{haskell}

\begin{document}
\maketitle

\section*{Introduction}

This document is the source code for, and description of, \prog{mkhprog} ---
a Haskell program which generates the skeleton of other Haskell programs.
In particular, it generates the code needed to parse values passed to a
program from the command line.

Given the command line flags and associated values to be interpreted by
a program, \prog{mkhprog} produces a module containing a \prog{main}
function which acquires the command line, parses any flags and values
present, and passes the values to the main body of the Haskell program
for further processing.

\prog{mkhprog} can produce standard or ``literate'' Haskell, it can
generate a module with any desired name, and it can send its output
to the standard output or to a named file.

The body of this document describes how to use \prog{mkhprog} and then
gives the code which implements it.
Annexes describe auxiliary functions, the continuation style of programming
used, and give hints and tips on compilation and general matters.

\section{\prog{mkhprog} in use}

\subsection{Installation}
\prog{mkhprog} is distributed as a literate Haskell file, \prog{Mkhprog.lhs},
and a UNIX manual page, \prog{mkhprog.1}.
\prog{Mkhprog.lhs} is also a \LaTeX\ source file which documents the program
and its usage.

To install \prog{mkhprog}, compile \prog{Mkhprog.lhs} (converting it to
standard Haskell first if necessary, as described in Annex~\ref{convert})
and place the binary somewhere accessible.
You may also want to put the manual page in your local manual directory.

\subsection{Flags}
We will first describe the command line parameters interpreted by
\prog{mkhprog} itself, and then show how it treats the remainder of its
command line.
The program interprets the following flags:
\begin{description}
\item[\prog{-l}] produces a ``literate'' Haskell script in which all lines of
    code begin with the characters \prog{"> "}.
    This file is an example of a literate Haskell script.
\item[\prog{-m} {\em module\_name}] writes a module called {\em module\_name}
    rather than the default name of \prog{Main}.
\item[\prog{-o} {\em file\_name}] writes output to {\em file\_name} rather than
    the default of standard output.
\end{description}
The first command line flag which is not a member of the above set marks
the beginning of the flags to be interpreted by the generated program, which
we will now describe.

Programs generated by \prog{mkhprog} can interpret command line flags and
associated values of any Haskell type of class \prog{Text}.
To generate a program which interprets a flag \prog{-f} and a value of
type \prog{Foo}, we invoke \prog{mkhprog} as
\begin{verbatim}
mkhprog -f Foo
\end{verbatim}
We may then invoke the generated program as
\begin{verbatim}
prog -f "MkFoo 3 True"
\end{verbatim}
if \prog{MkFoo 3 True} is a valid item of type \prog{Foo} and \prog{Foo}
is an instance of the class \prog{Text}.

If the type associated with the flag is either \prog{Bool} or \prog{String}
then the generated program behaves in a special manner as follows:
\begin{description}
\item[\prog{Bool}] the program interprets the flag without any following
value, i.e. a program generated by
\begin{verbatim}
mkhprog -b Bool
\end{verbatim}
is invoked simply as
\begin{verbatim}
prog -b
\end{verbatim}
with no \prog{True} or \prog{False}.

You can produce programs which interpret a Boolean flag as a toggle
by invoking \prog{mkhprog} as:
\begin{verbatim}
mkhprog +b Bool
\end{verbatim}
The generated program may then be called with either \prog{-b} or
\prog{+b} to set an internal value to \prog{False} or \prog{True}
respectively.

Boolean flags default to \prog{False}.

\item[\prog{String}] the program interprets the flag followed by an unquoted
string, i.e.
\begin{verbatim}
mkhprog -s String
\end{verbatim}
generates a program which is invoked as
\begin{verbatim}
prog -s string
\end{verbatim}
rather than
\begin{verbatim}
prog -s \"string\"
\end{verbatim}
String flags default to \prog{""} (the empty string).
\end{description}

\subsection{Output}
This section shows the output from \prog{mkhprog} and the editing needed
to produce a compilable program from it.
If \prog{mkhprog} is invoked as
\begin{verbatim}
mkhprog -b Bool -s String -f Foo
\end{verbatim}
then the following output is produced:
\begin{verbatim}
module Main (main) where

import System.Environment (getArgs)

defaultArgs :: Args
defaultArgs  =  MkArgs False "" ??

usage :: IO ()
usage  =  print "Usage: prog [-b] [-s String] [-f Foo]"

data Args  =  MkArgs Bool String Foo deriving ()

parse_args :: Args -> String -> IO ()
parse_args (MkArgs x1 x2 x3) ('-':'b':rest)
    =  readbool (parse_args (MkArgs True x2 x3)) rest
parse_args (MkArgs x1 x2 x3) ('-':'s':rest)
    =  readstring (\str -> parse_args (MkArgs x1 str x3)) rest
parse_args (MkArgs x1 x2 x3) ('-':'f':rest)
    =  readval reads (\val -> parse_args (MkArgs x1 x2 val)) rest
parse_args (MkArgs x1 x2 x3) ('-': _ :rest)
    =  usage
parse_args (MkArgs x1 x2 x3)  rest  =  prog x1 x2 x3 (lines rest)

main :: IO ()
main  = do
   argv <- getArgs
   parse_args defaultArgs (unlines argv)

and auxiliary functions ...
\end{verbatim}
The first line simply gives the module header and says that \prog{main}
is being exported.
If \prog{mkhprog} had been invoked with the \prog{-m} flag, then the
module name would have been different.

Next comes the default value of the command line arguments.
This is near the beginning as the user will probably want to edit it.
Boolean flags default to \prog{False}, strings to the empty string and
anything else to \prog{??}.
The user will need to edit \prog{??} to a sensible value before
compilation.

The function \prog{usage} is also near the beginning as it too will
probably need editing.
This function is called in case of the command line flags being invalid
and writes to the standard error a template for how to call the generated
program.

The later parts of the program should not need editing and consist of:
\begin{itemize}
\item A declaration of the \prog{Args} data type, which is used to
    carry information about command line flags.
\item The definition of \prog{parse\_args} which parses command line
    flags and associated values, gives an error message on unrecognised
    flags, and calls \prog{prog} when it hits a non-flag command line
    argument.
    \prog{prog} is then called with the values associated with the command
    line flags (or the default if the flag was not present) and any
    remaining command line arguments, split into a list of strings, one
    per argument.
\item The definition of \prog{main}, which just acquires the command line,
    puts it into a processable form,
    and passes it to \prog{parse\_args}, with \prog{defaultArgs}.
\item Auxiliary functions for reading Booleans, strings and arbitrary
    values from the command line arguments.
    These are given in Annex~\ref{auxfns}.
\end{itemize}

\subsection{Compilation}
Before compilation, the user must perform a few tasks:
\begin{itemize}
\item Edit any instances of \prog{??} in the declaration of \prog{defaultArgs}
    to sensible values.
\item Provide a definition of \prog{prog} to be called from
    \prog{parse\_args}.
\item Ensure that any user-defined data types associated with flags are in
    scope and are instances of the class \prog{Text}.
\end{itemize}
The user may also want to edit the definition of \prog{usage} to give a
more informative error message, though this is not essential.

After these changes, the program is ready for compilation.
The generated program is standard Haskell (version 1.1 or later)
and will compile on any
conforming implementation; some compilers have quirks which prevent
straightforward compilation, which are documented in Annex~\ref{quirks}.

\section{Command line parsing}

The functions which parse the command line are very similar to
those generated by \prog{mkhprog} itself.
Indeed they are edited from \prog{mkhprog} output.

The program starts with a module header which just exports \prog{main}.
\begin{haskell}

> module Main (main) where

> import Control.Exception (evaluate)
> import Control.Monad (replicateM_, void)
> import System.Environment (getArgs)
> import NofibUtils (hash)

\end{haskell}

The \prog{main} function acquires the command line arguments and passes
them to \prog{parse\_args}, together with their default values, encapsulated
in \prog{defaultEnv} (see Annex~\ref{env} for a description of the
environment).
\begin{haskell}

> main :: IO ()
> main = do
>  (n:_) <- getArgs
>  replicateM_ (read n) $ do
>    (_:argv) <- getArgs
>    parse_args defaultEnv (unlines argv)

\end{haskell}

\prog{parse\_args e str} reads occurrences of \prog{-l}, \prog{-m} and
\prog{-o}, with any associated values, from \prog{str}, modifies the
environment \prog{e} appropriately and then passes the remainder of
the command line and the modified environment to \prog{next}.
\begin{haskell}

> parse_args :: Env -> String -> IO ()
> parse_args (MkEnv x1 x2 x3) ('-':'l':rest)
>     =  readbool (parse_args (MkEnv True x2 x3)) rest
> parse_args (MkEnv x1 x2 x3) ('-':'m':rest)
>     =  readstring (\str -> parse_args (MkEnv x1 str x3)) rest
> parse_args (MkEnv x1 x2 x3) ('-':'o':rest)
>     =  readstring (\str -> parse_args (MkEnv x1 x2 (File str))) rest
> parse_args e rest  =  next (pairs (lines rest)) e

\end{haskell}

\prog{pairs} splits a list of strings into a list of pairs of
strings.
If the length of its argument is odd, it adds a dummy empty string,
which will cause an error to be flagged by \prog{next}.
\begin{haskell}

> pairs :: [String] -> [(String, String)]
> pairs []         =  []
> pairs (s:s':ss)  =  (s,s') : pairs ss
> pairs [s]        =  [("",s)]

\end{haskell}

\prog{next sps} checks whether the list of pairs of strings, \prog{sps},
forms valid command line input to \prog{mkhprog}.
If not, a message is printed and the program halts; otherwise \prog{mkprog}
is called to generate the program.
\begin{haskell}

> next :: [(String, String)] -> Cont
> next sps e | check sps  =  mkprog sps e
>            | otherwise  =  usage

\end{haskell}

\prog{check sps} checks whether the list of pairs of strings, \prog{sps},
forms valid command line input to \prog{mkhprog}.
This consists of checking that
each flag is of the form \prog{-c}, or is of the form \prog{+c}
with associated type \prog{Bool}, where \prog{c} is a character
and that none of the characters are duplicated.
\begin{haskell}

> check :: [(String, String)] -> Bool
> check sps  =  flags_ok sps && no_dups [flag | (_:flag:_,_) <- sps]
>               where
>               flags_ok  =  and . map f
>               f (['-',_],_)       =  True
>               f (['+',_],"Bool")  =  True
>               f _                 =  False
>               no_dups []      =  True
>               no_dups (c:cs)  =  not (c `elem` cs) && no_dups cs

\end{haskell}

\prog{usage} prints a message saying how \prog{mkhprog} should be used.
\begin{haskell}

> usage :: IO ()
> usage =
>     putStr
>      "Usage: mkhprog [-l] [-m module_name] [-o file_name] [[-flag type] ...]"

\end{haskell}

\prog{mkprog sps} generates and writes the program by calling a series of
functions, each generating one part.
\begin{haskell}

> mkprog :: [(String, String)] -> Cont
> mkprog sps
>     =  (do_header . nl . do_default types . nl . do_usage sps . nl
>         . do_datadecl types . nl . do_parse_args sps . nl . do_auxfns)
>        stop
>        where
>        types  =  map snd sps

\end{haskell}

\prog{do\_header} writes out the module header.
\begin{haskell}

> do_header :: Cont -> Cont
> do_header c
>     =  modname (\s -> writeln ("module " ++ s ++ " (main) where\n\nimport System (getArgs)") c)

\end{haskell}

\prog{do\_default ts} writes a declaration for the default value of the
command line argument types, \prog{ts}.
\prog{String}s default to the empty string; Booleans default to \prog{False};
all other types default to \prog{??}, which must be edited to a sensible
value by the user.
\begin{haskell}

> do_default :: [String] -> Cont -> Cont
> do_default ts
>     =  writeln "defaultArgs :: Args"
>        . writeln ("defaultArgs  =  MkArgs" ++ concat (map f ts))
>        where
>        f "String"  =  " \"\""
>        f "Bool"    =  " False"
>        f _         =  " ??"

\end{haskell}

\prog{do\_usage sps} writes a usage function, which is called if the
command line arguments are invalid.
\begin{haskell}

> do_usage :: [(String, String)] -> Cont -> Cont
> do_usage sps
>     =  writeln "usage :: IO ()"
>        . writeln ("usage  =  hPutStr stderr \"Usage: prog"
>                   ++ concat (map f sps) ++ "\"")
>        where
>        f (['-',c], "Bool")  =  " [-" ++ [c] ++ "]"
>        f (['+',c], "Bool")  =  " [(+|-)" ++ [c] ++ "]"
>        f (flag, typ)        =  " [" ++ flag ++ " " ++ typ ++ "]"

\end{haskell}

\prog{do\_datadecl ts} declares the type \prog{Args} to be made up of the
types \prog{ts}.
\begin{haskell}

> do_datadecl :: [String] -> Cont -> Cont
> do_datadecl ts
>     =  writeln ("data Args  =  MkArgs" ++ concat (map ((:) ' ') ts)
>                 ++ " deriving ()")

\end{haskell}

\prog{do\_parse\_args sps} writes the function which parses command line
arguments.
It applies \prog{do\_one\_flag} to each element of \prog{sps} and then
writes a final couple of clauses which catch invalid flags and call the
main program, \prog{prog}.
\begin{haskell}

> do_parse_args :: [(String, String)] -> Cont -> Cont
> do_parse_args sps
>     =  writeln "parse_args :: Args -> String -> IO ()"
>        . foldr (.) end
>        (zipWith3 do_one_flag (repeat n) [1..] sps)
>        where
>        n  =  length sps
>        end  =  writeln (phead n ++ wildmatch '-' ++ "usage")
>                . writeln (phead n ++ " rest  =  prog" ++ args 1 n
>                           ++ " (lines rest)")
>

\end{haskell}

\prog{do\_one\_flag n r sp} writes one clause of the function which
parses command line arguments.
\prog{n} is the arity of \prog{MkArgs}, \prog{r} is the number of the current
argument, \prog{sp} is a pair of the flag and its type (as strings).
\begin{haskell}

> do_one_flag :: Int -> Int -> (String, String) -> Cont -> Cont
> do_one_flag n r (['+',flag], _)
>     =  writeln (phead n ++ match '-' flag ++ do_readbool n r False)
>        . writeln (phead n ++ match '+' flag ++ do_readbool n r True)
> do_one_flag n r (['-',flag], "Bool")
>     =  writeln (phead n ++ match '-' flag ++ do_readbool n r True)
> do_one_flag n r (['-',flag], "String")
>     =  writeln (phead n ++ match '-' flag ++ do_readstring n r)
> do_one_flag n r (['-',flag], _)
>     =  writeln (phead n ++ match '-' flag ++ do_readval n r)

\end{haskell}

\prog{do\_auxfns} writes out the functions used by \prog{mkhprog} which
do not change from program to program.
The functions are described in Annex~\ref{auxfns}
\begin{haskell}

> do_auxfns :: Cont -> Cont
> do_auxfns
>     =    writeln "main :: IO ()\n\
>                  \main  =  getArgs >>= (parse_args defaultArgs . unlines)"
>        . nl
>        . writeln "readbool :: (String -> IO ()) -> String -> IO ()\n\
>                  \readbool f \"\"         =  f \"\"\n\
>                  \readbool f ('\\n':cs)  =  f cs\n\
>                  \readbool f _          =  usage"
>        . nl
>        . writeln "readstring :: (String -> String -> IO ()) -> String \
>                                \-> IO ()\n\
>                  \readstring f \"\"  =  f \"\" \"\"\n\
>                  \readstring f cs@(c:cs')\n\
>                  \    =  f s t\n\
>                  \       where\n\
>                  \       st      =  if c == '\\n' then cs' else cs\n\
>                  \       (s,t1)  =  span ((/=) '\\n') st\n\
>                  \       t       =  if t1 == \"\" then t1 else (tail t1)"
>        . nl
>        . writeln "readval :: (Read a) => ReadS a \
>                             \-> (a -> String -> IO ()) -> String\n\
>                  \                       -> IO ()\n\
>                  \readval readsfn f str\n\
>                  \    =  case thing of\n\
>                  \           []    -> usage\n\
>                  \           (_:_) -> f val (if str' == \"\" \
>                                             \then str' else (tail str'))\n\
>                  \       where\n\
>                  \       thing        =  readsfn str\n\
>                  \       (val, str')  =  head thing"

\end{haskell}

\prog{phead n} returns the first part of the \prog{parse\_args} function,
where \prog{n} is the arity of \prog{MkArgs}.
\begin{haskell}

> phead :: Int -> String
> phead n  =  "parse_args (MkArgs" ++ args 1 n ++ ") "

\end{haskell}

\prog{args a b} gives a sequence of argument name strings,
\prog{xa}, $\ldots$, \prog{xb}, separated by spaces.
\begin{haskell}

> args :: Int -> Int -> String
> args a b  =  concat [" x" ++ show r | r <- [a..b]]

\end{haskell}

\prog{match mp c} returns a pattern which matches against \prog{-c}
or \prog{+c} as \prog{mp} is \prog{'-'} or \prog{'+'}.
\begin{haskell}

> match :: Char -> Char -> String
> match mp c  =  "('" ++ [mp] ++ "':'" ++ [c] ++ "':rest)\n    =  "

\end{haskell}

\prog{wildmatch mp} returns a pattern which matches against \prog{-c}
or \prog{+c}, where \prog{c} is any character at all, as \prog{mp} is
\prog{'-'} or \prog{'+'}.
\begin{haskell}

> wildmatch :: Char -> String
> wildmatch mp  =  "('" ++ [mp] ++ "': _ :rest)\n    =  "

\end{haskell}

\prog{do\_readval}, \prog{do\_readstring} and \prog{do\_readbool} produce
pieces of code for reading arbitrary values, strings and booleans
respectively.
\begin{haskell}

> do_readval, do_readstring :: Int -> Int -> String
> do_readbool   :: Int -> Int -> Bool -> String
> do_readval n r     =  "readval reads (\\val -> " ++ bits n r "val"
> do_readstring n r  =  "readstring (\\str -> " ++ bits n r "str"
> do_readbool n r b  =  "readbool (" ++ bits n r (show b)

\end{haskell}

\prog{bits n r str} produces a handy part of \prog{parse\_args}.
\begin{haskell}

> bits :: Int -> Int -> String -> String
> bits n r str
>     =  "parse_args (MkArgs" ++ args 1 (r-1) ++ " " ++ str ++ args (r+1) n
>        ++ ")) rest"

\end{haskell}

\prog{writeln str} writes the string \prog{str} to the chosen
output channel, in  literate format if appropriate,
and then executes its continuation, \prog{c}.
\prog{nl} gives a newline on the standard output.
\begin{haskell}

> writeln :: String -> Cont -> Cont
> writeln str c
>     =  islit (\b ->
>        output (\f env ->
>            let
>              str'   =  (if b then (lit str) else str) ++ "\n"
>              lit s  =  "> " ++ foldr ((++) . conv) "" s
>              conv '\n'  =  "\n> "
>              conv c     =  [c]
>	     in
>		f str' >> c env))

> nl :: Cont -> Cont
> nl c = output (\f env -> f "\n" >> c env)

\end{haskell}

\section{Miscellaneous}

\subsection{Bugs, comments and suggestions}
\prog{mkhprog} was written to implement one person's view of what a
skeleton Haskell program should look like.
It is by no means obvious that this is the best solution and any
suggestions for improvements are welcome.
Please send your bug reports, comments and suggestions to
\prog{ndn\@seg.npl.co.uk}.

\subsection{Acknowledgements}
\prog{mkhprog}'s functionality and manual page are closely based on those
of the C program \prog{mkprog}, whose author is unknown to me so I cannot
thank him or her in person.

Thanks also to the various implementers of Haskell around the world who
have borne with my continual bug reports and naive questions.
Thanks in particular to Will Partain who supplied useful patches to keep
\prog{mkhprog} in line with the changing definition of Haskell.

\appendix
\newpage
\section{Auxiliary functions}
\label{auxfns}

These are the functions produced by \prog{do\_auxfns}, which are used
by all programs written by \prog{mkhprog}.

\prog{readbool f str} checks that a Boolean flag is followed by either
newline or nothing: if it is, then \prog{f} is applied to the rest of
the string \prog{str}; if not, \prog{usage} is called.
\begin{haskell}

> readbool :: (String -> IO ()) -> String -> IO ()
> readbool f ""         =  f ""
> readbool f ('\n':cs)  =  f cs
> readbool f _          =  usage

\end{haskell}

\prog{readstring f str} reads a string, \prog{s}, from \prog{str} as follows:
\begin{itemize}
\item[] If \prog{str} begins with \verb|'\n'|, then \prog{s} is the
    sequence of characters following the newline, up to the next newline
    or the end of \prog{str}, whichever occurs first.
\item[] Otherwise \prog{s} is the initial substring of \prog{str}, up to
    the first newline or the end of \prog{str}, whichever occurs first.
\end{itemize}
\prog{f} is then applied to \prog{s} and \prog{t}, where \prog{t} is the
remainder of \prog{str} after removing \prog{s} and a newline character, if
\prog{s} is followed by one.
\begin{haskell}

> readstring :: (String -> String -> IO ()) -> String -> IO ()
> readstring f ""  =  f "" ""
> readstring f cs@(c:cs')
>       =  f s t
>          where
>          st      =  if c == '\n' then cs' else cs
>          (s,t1)  =  span ((/=) '\n') st
>          t       =  if t1 == "" then t1 else (tail t1)

\end{haskell}

\prog{readval readsfn f str} reads a value from \prog{str} in the same manner
as \prog{readstring}, but the value can be of any type of class \prog{Text}.
The parameter \prog{readsfn} is the appropriate overloading of \prog{reads}
--- this parameter is not essential but, without it, \prog{readval} is
reliant on the ``monomorphic restriction'', whose future presence in
Haskell is not guaranteed.
\prog{f} is applied to the value and the remainder of \prog{str} as in
\prog{readstring}, but if no value can be read, \prog{usage} is called.

Since \prog{mkhprog} does not itself use \prog{readval}, its code does
not form part of this literate Haskell script.
\begin{verbatim}

readval :: (Read a) => ReadS a -> (a -> String -> IO ()) -> String
		       -> IO ()
readval readsfn f str
    =  case thing of
           []    -> usage
           (_:_) -> f val (if str' == "" then str' else (tail str'))
       where
       thing        =  readsfn str
       (val, str')  =  head thing

\end{verbatim}

\section{Continuations and environments}

\subsection{Continuations}
\prog{mkhprog} is written in the continuation style, in which functions take
a continuation argument which tells the program ``what to do next''.
A continuation is a function which takes an environment parameter and gives
the rest of the program's output.
Thus we may define a data type for continuations thus:
\begin{haskell}

> type Cont  =  Env -> IO ()

\end{haskell}

The null continuation, which does nothing, is
\begin{haskell}

> stop :: Cont
> stop e  =  return ()

\end{haskell}

\subsection{Environments}
\label{env}
An environment contains all the information needed to generate the output
program, viz.
\begin{enumerate}
\item Whether it is a literate program.
\item The name of the module to be generated.
\item Whether output is to \prog{stdout} or to a file and, if the latter,
    the file's name.
\end{enumerate}
We may encompass this information in an environment data type:
\begin{haskell}

> data Env  =  MkEnv Bool String Output

\end{haskell}
Where the \prog{Bool} component is \prog{True} or \prog{False} as the
program is literate or not, the \prog{String} component is the module
name, and the \prog{Output} component gives details of the output stream
as follows:
\begin{haskell}

> data Output  =  Stdout | File String

\end{haskell}

The value of the environment is determined by the command line flags to
\prog{mkhprog}.
The default environment is
\begin{haskell}

> defaultEnv :: Env
> defaultEnv  =  MkEnv False "Main" Stdout

\end{haskell}

\subsection{Environment access}
Functions which need to access the environment use the
access functions in this section.

\prog{islit bc} applies \prog{bc} to the value of the Boolean
environment component.
\begin{haskell}

> islit :: (Bool -> Cont) -> Cont
> islit bc e@(MkEnv l _ _)  =  bc l e

\end{haskell}

\prog{modname sc} applies \prog{sc} to the string environment
component.
\begin{haskell}

> modname :: (String -> Cont) -> Cont
> modname sc e@(MkEnv _ m _)  =  sc m e

\end{haskell}

\prog{output oc} applies \prog{oc} to a function which writes its
argument to the output channel given by the \prog{Output} environment
component.
\begin{haskell}

> output :: ((String -> IO ()) -> Cont) -> Cont
> -- output oc e@(MkEnv _ _ Stdout)    =  oc (putStr) e
> output oc e@(MkEnv _ _ Stdout)    =  oc (void . evaluate . hash) e
> output oc e@(MkEnv _ _ (File f))  =  oc (appendFile f) e

\end{haskell}

\section{Hints and tips}

\subsection{Compilation}
\label{quirks}
This section summarises the compilability of \prog{mkhprog} on the
compilers available at the time of writing (92/06/23).
\begin{description}
\item[Glasgow Prototype 0.41] Supports Haskell version 1.0, so
    cannot compile this file.
    Contact me if you want a version of \prog{mkhprog} that compiles under
    Haskell version 1.0.
\item[Chalmers Haskell B 0.997.5] This file (\prog{Mkhprog.lhs}) compiles and
    runs with no problems.
    Command line parameters to \prog{mkhprog} and the programs it generates
    must follow an isolated \prog{-},
    as others are interpreted by the run-time system (see the documentation).
\item[Yale 1.0-0] The Yale system does not support \prog{getArgs} so
    \prog{mkhprog} and the programs it generates are entirely useless on
    this system.
\end{description}

\subsection{Extracting a standard Haskell program}
\label{convert}
\prog{mkhprog} is distributed as a literate Haskell script.
If your compiler is unable to compile such scripts, you should type
the following UNIX command line (assuming this file is called
\prog{Mkhprog.lhs}):
\begin{verbatim}
expand Mkhprog.lhs | sed -e "/^> /\!d" -e "s/^> //" > Mkhprog.hs
\end{verbatim}
The output file \prog{Mkhprog.hs} is then standard Haskell and can be
compiled in the normal way.

\subsection{Things with literate Haskell}
Those, like myself, who like their literate Haskell surrounded by a
\LaTeX\ environment may care to use something like this:
\begin{verbatim}
mkhprog -l <flags> | perl -pe 's/^$/\n\\end{haskell}\n\\begin{haskell}\n/' > Foo.hs
\end{verbatim}
The file \prog{Foo.hs} will then have its declarations placed within a
\LaTeX\ environment called \prog{haskell}.

\end{document}
