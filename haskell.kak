# http://haskell.org
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
# This is based on the 'kakoune/rc/base/haskell.kak'.
#

# Detection
# ‾‾‾‾‾‾‾‾‾

hook global BufCreate .*[.](hs) %{
    set-option buffer filetype haskell
}

# Highlighters
# ‾‾‾‾‾‾‾‾‾‾‾‾

declare-option -hidden range-specs hs_fnames_range


add-highlighter shared/haskell regions
add-highlighter shared/haskell/code default-region group
# add-highlighter shared/haskell/string  region (?<!'\\)(?<!')"  (?<!\\)(\\\\)*" fill string
add-highlighter shared/haskell/string  region (?<!'\\)(?<!')"  (?<!\\)(\\\\)*" group
add-highlighter shared/haskell/macro   region ^\h*?\K#           (?<!\\)\n     fill meta
add-highlighter shared/haskell/pragma  region -recurse \{- \{-#       '#-\}' fill meta
add-highlighter shared/haskell/comment region -recurse \{-  \{-        -\}   group
add-highlighter shared/haskell/line-comment region --(?:[^!#$%&*+./<>?@\\\^|~=]|$) $  group
# add-highlighter shared/haskell/line-comment region --(?:[^!#$%&*+./<>?@\\\^|~=]|$) $  fill comment

# add-highlighter shared/haskell/string  fill string
# add-highlighter shared/haskell/comment fill comment
# add-highlighter shared/haskell/pragma  fill meta
# add-highlighter shared/haskell/macro   fill meta
# add-highlighter shared/haskell/where   fill meta

add-highlighter shared/haskell/string/  fill string

add-highlighter shared/haskell/comment/ fill comment
add-highlighter shared/haskell/comment/ regex \b(TBD|TODO|[Tt]odo)\b 0:red
add-highlighter shared/haskell/line-comment/  fill comment
add-highlighter shared/haskell/line-comment/ regex \b(TBD|TODO|[Tt]odo)\b 0:red

# Other can override this one.
add-highlighter shared/haskell/code/ regex ^[a-z][\w|'0-9]*\s 0:function
add-highlighter shared/haskell/code/ regex ^\h+[a-z][\w|'0-9]*\s+(?=::\s) 0:function
add-highlighter shared/haskell/code/ regex ^\h+[a-z][\w|'0-9]*\s+(?=∷\s) 0:function
# add-highlighter shared/haskell/code regex ^\h+[a-z][\w|'0-9]*\h+(?<=(::|∷)) 0:function
# add-highlighter shared/haskell/code regex (^\h+)(where\n)(\1) 2:function

# For some special words
add-highlighter shared/haskell/code/ regex \bundefined\b 0:red
add-highlighter shared/haskell/code/ regex \bwarning\b 0:red
add-highlighter shared/haskell/code/ regex \berror\b 0:red

add-highlighter shared/haskell/code/ regex (?<!')\b0x+[A-Fa-f0-9]+ 0:value
add-highlighter shared/haskell/code/ regex (?<!')\b\d+([.]\d+)? 0:value
add-highlighter shared/haskell/code/ regex (?<!')\b(import|hiding|qualified|module)(?!')\b 0:keyword
add-highlighter shared/haskell/code/ regex (?<!')\b(import)(?!')\b[^\n]+(?<!')\b(as)(?!')\b 2:keyword
add-highlighter shared/haskell/code/ regex (?<!')\b(class|data|default|deriving|infix|infixl|infixr|instance|module|newtype|pattern|type|where|pure|return)(?!')\b 0:keyword
add-highlighter shared/haskell/code/ regex (?<!')\b(case|do|else|if|in|let|mdo|of|proc|rec|then|otherwise)(?!')\b 0:attribute

# copy something into i register
# exec -draft -save-regs '' "ghGih\"i*"
# k maybe should be there


# The complications below are such because the period has many uses:
# As function composition operator (possibly without spaces) like "." and "f.g"
# Hierarchical modules like "Data.Maybe"
# Qualified imports like "Data.Maybe.Just", "Data.Maybe.maybe", "Control.Applicative.<$>"
# Quantifier separator in "forall a . [a] -> [a]"
# Enum comprehensions like "[1..]" and "[a..b]" (making ".." and "Module..." illegal)

# matches uppercase identifiers:  Monad Control.Monad
# not non-space separated dot:    Just.const
add-highlighter shared/haskell/code/ regex \b([A-Z]['\w]*\.)*[A-Z]['\w]* 0:variable
# add-highlighter shared/haskell/code regex \b([A-Z]['\w]*\.)*[A-Z]['\w]*(?!['\w])(?![.\l]) 0:variable

# matches infix identifier: `mod` `Apa._T'M`
add-highlighter shared/haskell/code/ regex `\b([A-Z]['\w]*\.)*[\w]['\w]*` 0:operator

# matches imported operators: M.! M.. Control.Monad.>>
# not operator keywords:      M... M.->
add-highlighter shared/haskell/code/ regex \b[A-Z]['\w]*\.[~<=>|:!?/.@$*&#%+\^\-\\]+ 0:operator
# matches dot: .
# not possibly incomplete import:  a.
# not other operators:             !. .!
add-highlighter shared/haskell/code/ regex (?<![\w~<=>|:!?/.@$*&#%+\^\-\\])\.(?![~<=>|:!?/.@$*&#%+\^\-\\]) 0:operator

# matches other operators: ... > < <= ^ <*> <$> etc
# not dot: .
# not operator keywords:  @ .. -> :: ~
add-highlighter shared/haskell/code/ regex (?<![~<=>|:!?/.@$*&#%+\^\-\\])[~<=>|:!?/.@$*&#%+\^\-\\]+ 0:operator

# matches operator keywords: @ ->
add-highlighter shared/haskell/code/ regex (?<![~<=>|:!?/.@$*&#%+\^\-\\])(@|~|<-|←|->|→|=>|⇒|::|∷|=|:|[|])(?![~<=>|:!?/.@$*&#%+\^\-\\]) 1:keyword

# matches: forall [..variables..] 
# not the variables
add-highlighter shared/haskell/code/ regex \b(forall)\b[^.\n]*?(\.) 1:keyword 2:keyword

# matches 'x' '\\' '\'' '\n' '\0'
# not incomplete literals: '\'
# not valid identifiers:   w' _'
add-highlighter shared/haskell/code/ regex \B'([^\\]|[\\]['"\w\d\\])' 0:string
# this has to come after operators so '-' etc is correct

# matches []{}(),
#
add-highlighter shared/haskell/code/ regex [\[|\]|\{|\}|\(|\)|,] 0:operator

# add-highlighter shared/haskell/ show-matching # this doesn't work
# While the following work separately, they don't in the cases where the parenthesis
# cross regions. Better to turn on the show-matching in the kakrc globally.
# add-highlighter shared/haskell/string/ show-matching
# add-highlighter shared/haskell/code/ show-matching
# add-highlighter shared/haskell/comment/ show-matching
# add-highlighter shared/haskell/line-comment/ show-matching


#define-command -hidden haskell-hl-fnames %{
    #evaluate-commands -draft %{ %try{
        #execute-keys \%s  
        #set-option buffer hs_fnames_range %val{timestamp}
        #evaluate-commands -itersel %{
            #set-option -add buffer hs_fnames_range "%val{selection_desc}|function"
        #}
    #}}
#}
            # set-option -add buffer hs_fnames_range "%val{selection_desc}|%reg{1}"




# Commands
# ‾‾‾‾‾‾‾‾

# http://en.wikibooks.org/wiki/Haskell/Indentation
# https://github.com/tibbe/haskell-style-guide
# https://wiki.haskell.org/Programming_guidelines

define-command -hidden haskell-filter-around-selections %{
    # remove trailing white spaces
    try %{ execute-keys -draft -itersel <a-x> s \h+$ <ret> d }
}

# try %{ execute-keys -draft -itersel <a-x> s ab <ret> → }
# 
define-command -hidden haskell-replace-arrows %{
    evaluate-commands -draft -itersel %{
        # try %{ execute-keys -draft <a-x> s (?<lt>=\h)ab(?=\h) <ret> c ba <esc> }
        try %{ execute-keys -draft -itersel <a-x> s (?<lt>=\h)->(?=\h) <ret> c → <esc> }
        try %{ execute-keys -draft -itersel <a-x> s (?<lt>=\h)->$ <ret> c → <esc> }
        try %{ execute-keys -draft -itersel <a-x> s (?<lt>=\h)<-(?=\h) <ret> c ← <esc> }
        try %{ execute-keys -draft -itersel <a-x> s (?<lt>=\h)<-$ <ret> c ← <esc> }
        try %{ execute-keys -draft -itersel <a-x> s (?<lt>=\h)=>(?=\h) <ret> c ⇒ <esc> }
        try %{ execute-keys -draft -itersel <a-x> s (?<lt>=\h)=>$ <ret> c ⇒ <esc> }
        try %{ execute-keys -draft -itersel <a-x> s (?<lt>=\h)::(?=\h) <ret> c ∷ <esc> }
        try %{ execute-keys -draft -itersel <a-x> s (?<lt>=\h)::$ <ret> c ∷ <esc> }
        # try %{ execute-keys -draft <a-x> s (?<lt>=\h)ac(?=\h) <ret> c ca <esc> }
    }
}

define-command -hidden haskell-on-new-line %{
    evaluate-commands -draft -itersel %{
        try %{ execute-keys -draft k : haskell-replace-arrows <ret> }
    }
}

# decl str _brace %sh{printf '\x7B'}
# decl str _newline %sh{printf '\n'}

# TBD TODO
# Assume that this is called after entering '>'-char.
# Do we need the following?  There is 'on-sigs' below.
define-command -hidden haskell-indent-on-farrow %{
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # Indent the -> and → if the prev line starts with it and this too.
        try %{
            # Check this line
            execute-keys -draft <a-x><a-k> (^\h+(->|→)\h.*$) <ret>
        }
        #try %{ execute-keys -draft kkxXs (^\h+\|(?!\|).*$\n)\h+(\|(?!\|).*$)<ret>c<c-r>1<c-r>2}
        #try %{ execute-keys -draft kkxXs ^\h+\K\|(?!\|).*$\n\|(?!\|) <ret> s \A|.\z <ret>& }
        #Align to fst clause after |-line.
        #try %{ execute-keys -draft kxXs ^\h+\|(?!\|)\h*\K\h[\S|\h]*$\n\h* <ret> s \A|.\z <ret>& }
        #
    }
}

define-command -hidden haskell-indent-on-nl-bar-char %{
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # Indent the | if the prev line starts with it and this too (but not on ||).
        #try %{ execute-keys -draft kkxXs (^\h+\|(?!\|).*$\n)\h+(\|(?!\|).*$)<ret>c<c-r>1<c-r>2}
        #try %{ execute-keys -draft kkxXs ^\h+\K\|(?!\|).*$\n\|(?!\|) <ret> s \A|.\z <ret>& }
        try %{ execute-keys -draft kxXs (^\h+\K\|(?!\|).*$\n)\h+(\|(?!\|)) <ret> <a-S>1<a-&>&}
        # Align to fst clause after |-line, if this line does not start with |.
        try %{ execute-keys -draft <a-x><a-K> ^\h*\| <ret> kxXs ^\h+\|(?!\|)\h*\K\h[\S|\h]*$\n\h* <ret> s \A|.\z <ret>& }
        #
    }
}

define-command -hidden haskell-indent-on-bar-char %{
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # Indent the | if the prev line starts with it and this too (but not on ||).
        #try %{ execute-keys -draft kxXs (^\h+\|(?!\|).*$\n)\h+(\|(?!\|)) <ret> c<c-r>1<c-r>2 }
        #try %{ execute-keys -draft kxXs ^\h+\K\|(?!\|).*$\n\|(?!\|) <ret> s \A|.\z <ret> & }
        try %{ execute-keys -draft kxXs (^\h+\K\|(?!\|).*$\n)\h*\|(?!\|) <ret> <a-S>1<a-&>&}
    }
}

define-command -hidden haskell-indent-on-nl-where %{
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # The following are for 'where' -handling
        # Make the indentation to pos 3, if the prev line started on the 1st col.
        try %{ execute-keys -draft kkxXs (^\w.*$\n)\h\h(\h\hwhere((\h*)|(\h[\h\S]*))$) <ret> c<esc>\i<c-r>1<c-r>2 }
        # If 'where' starts the line, and nothing following where,
        # indent the line following to start at fst e-char.
        try %{ execute-keys -draft kxXs (^\h*)(\h\h)(where\h*\n)\h* <ret> c<esc>\i<c-r>1<c-r>2<c-r>3<c-r>1<c-r>2<c-r>2 }
        # If something is following, indent to the first clause.
        try %{ execute-keys -draft kxXs (^\h*)(\h\h)(where\h+\S[\S\h]*\n)\h* <ret> c<esc>\i<c-r>1<c-r>2<c-r>3<c-r>1<c-r>2<c-r>2<c-r>2<c-r>2 }
        # If there is something before where, how should we indent?
        # ATM, do nothing special.
        # try %{ execute-keys -draft \; k x X s (^\h+([\h\w\d!#%=]|-(?!-))+)(where\h+\n)\h* <ret> c<esc>\i<c-r>1<c-r>2<c-r>3<c-r>1<c-r>2<c-r>2 }
    }
}


define-command -hidden haskell-indent-on-nl-import %{
    evaluate-commands -itersel %{
        execute-keys \;
        # The following are for 'import' -handling.
        # If the prev line starts with import, move the cursor to the fst col.
        # try %{ execute-keys -draft kXs ^import\h+ <ret>gh}
        # try %{ execute-keys -draft kxXs (^\Kimport\h+.*$\n)\h+ <ret>c<esc>\i<c-r>1 }
        try %{
            execute-keys -draft k<a-x> <a-K> ^$ <ret>
            try %{ execute-keys -draft kx <a-K> ^import\h+qualified\h+ <ret> s ^import\h+([\S|\h]*$\n) <ret>c<esc>\iimport<space><space><space><space><space><space><space><space><space><space><space><c-r>1
            }
        } catch %{
            #  prev line was empty, thus the cursor in the 1st column
            try %{ execute-keys -draft <a-x> <a-K> ^import\h+qualified\h+ <ret> s ^import\h+([\S|\h]*$\n) <ret>c<esc>\iimport<space><space><space><space><space><space><space><space><space><space><space><c-r>1
            }
        }
    }
}


# Note that the line maybe indented already to fst non-whitespace.
# Or not.  So the command to do aligment might have to check for it.
define-command -hidden haskell-indent-on-nl-opening %{
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # The following three are for cases where the line is already indented.
        # And we are splitting the previous line.
        try %¤
            execute-keys -draft <a-x> <a-k> ^\h+\S <ret>
            #try on '['
            try %(
                execute-keys -draft kxXs ^\h+\K\[[^\]]*$\n\h+\S <ret> <a-S>1<a-&>&
            ) catch %(
                execute-keys -draft kxXs ^.*[^\[]\K\[[^\]]*$\n\h+\S <ret><a-S>1<a-&>&
            )
        ¤
        try %¤
            execute-keys -draft <a-x> <a-k> ^\h+\S <ret>
            # try on '('
            try %{
                execute-keys -draft kxXs ^\h+\K\([^\)]*$\n\h+\S<ret><a-S>1<a-&>&
            } catch %{
                execute-keys -draft kxXs ^.*[^\(]\K\([^\)]*$\n\h+\S<ret><a-S>1<a-&>&
            }
        ¤
        try %¤
            execute-keys -draft <a-x> <a-k> ^\h+\S <ret>
            # try on opening brace.
            try %(
                execute-keys -draft kxXs ^\h+\K\{[^\}]*$\n\h+\S <ret><a-S>1<a-&>&
            ) catch %(
                execute-keys -draft kxXs ^.*[^\{]\K\{[^\}]*[^\}]?$\n\h+\S <ret><a-S>1<a-&>&
            )
        ¤
        #
        # The following three are for cases where the line is already indented.
        # We are just making a new line, e.g. 'o' or enter at the end of line.
        # Note that this is also for function starts.
        try %¤
            execute-keys -draft <a-x> <a-k> ^\h+$ <ret>
            #try on '['
            try %(
                execute-keys -draft kxXs ^\h.*\K[^\[]\[[^\]]*$\n\h+<ret><a-S>1<a-&>&
            ) catch %(
                execute-keys -draft kxXs ^\S.*[^\[]\K\[[^\]]*$\n\h+<ret><a-S>1<a-&>&
            )
        ¤
        try %¤
            execute-keys -draft <a-x> <a-k> ^\h+$ <ret>
            # try on '('
            try %{
                execute-keys -draft kxXs ^\h.*\K[^\(]\([^\)]*$\n\h+<ret><a-S>1<a-&>&
                # execute-keys -draft kxXs ^.* <ret> y5jp
            } catch %{
                execute-keys -draft kxXs ^\S.*[^\(]\K\([^\)]*$\n\h+<ret><a-S>1<a-&>&
            }
        ¤
        try %¤
            execute-keys -draft <a-x> <a-k> ^\h+$ <ret>
            try %(
                execute-keys -draft kxXs ^\h.*\K[^\{]\{[^\}]*[^\}]?$\n\h+<ret><a-S>1<a-&>&
            ) catch %(
                execute-keys -draft kxXs ^\S.*[^\{]\K\{[^\}]*[^\}]?$\n\h+<ret><a-S>1<a-&>&
            )
        ¤
        #
        # The following three are for cases where the line is not already indented.
        try %¤
            execute-keys -draft <a-x> <a-k> ^(\S|\n) <ret>
            #try on '['
            try %(
                execute-keys -draft kxXs ^\h+\K\[[^\]]*$\n. <ret><a-S>&
            ) catch %(
                execute-keys -draft kxXs ^.*\K[^\[]\[[^\]]*$\n. <ret><a-S>&
            )
        ¤
        try %¤
            execute-keys -draft <a-x> <a-k> ^(\S|\n) <ret>
            #try on '('
            try %{
                execute-keys -draft kxXs ^\h+\K\([^\)]*$\n. <ret><a-S>&
            } catch %{
                execute-keys -draft kxXs ^.*\K[^\(]\([^\)]*$\n. <ret><a-S>&
            }
        ¤
        try %¤
            execute-keys -draft <a-x> <a-k> ^(\S|\n) <ret>
            #try on opening brace.
            try %(
                execute-keys -draft kxXs ^\h+\K\{[^\}]*$\n. <ret><a-S>&
            ) catch %(
                execute-keys -draft kxXs ^.*\K[^\{]\{[^\}]*[^\}]?$\n. <ret><a-S>&
            )
        ¤
        # If the prev line contains opening and closing, find the first non-space.
        # ???? 
        # try %¤ execute-keys -draft kx<a-k> (\(.*\))|(\[.*\])|(\{.*\}) <ret> Xs ^\h+\K\S.*$\n. <ret> <a-S> & ¤
    }
}

define-command -hidden haskell-indent-on-closing %<
    # align to opening curly brace when alone on a line
    try %< execute-keys -itersel -draft <a-h><a-:><a-k>^\h*\}$<ret>hm<a-S>1<a-&>& >
    # align to opening ']' when alone on a line
    try %< execute-keys -itersel -draft <a-h><a-:><a-k>^\h*\]$<ret>hm<a-S>1<a-&>& >
    # align to opening ')' when alone on a line
    try %< execute-keys -itersel -draft <a-h><a-:><a-k>^\h*\)$<ret>hm<a-S>1<a-&>& >
>


# TODO TODO We should check that we are not in open block here when
# inserting the comma in an empty line.
define-command -hidden haskell-indent-on-comma %<
    # align to previous comma when alone on a line
    # try %< execute-keys -itersel -draft \; kxXs,<ret>& >
    try %< execute-keys -itersel -draft \; <a-x><a-k> ^\h*, <ret> kxXs^\h*,<ret><a-S>1<a-&> >
>

# 
define-command -hidden haskell-indent-on-fsig-arrow %{
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # If prev line is function signature, indent to ::
        # This is tied to '>'-character, thus we check that there is - or = in
        # front of it.
        try %{
            # execute-keys -itersel -draft \; <a-x><a-k> ^\h*([-=]\>|→) <ret> kxXs^\w.*\K(::|∷)\h.*$\n\h*([-=]\>|→)<ret><a-S>1&
            execute-keys -itersel -draft \; <a-x><a-k> ^\h*([-=→⇒]) <ret> kxXs^\w.*\K(::|∷)\h.*$\n\h*[-=→⇒]<ret><a-S>1<a-&>&
        }
    }
}

define-command -hidden haskell-indent-on-nl-function-sigs %{
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # Indent a line following function unless it is a signature.
        # But only for functions that start from col 1...
        # Note, we don't indent lines that end with 'do' or '=' as they are indented
        # later.
        try %{ execute-keys -draft k x <a-K> (::|∷|\bdo|\bmdo|\brec\h*$|=\h*$|^import\b) <ret> <a-k> ^\w <ret> j <a-gt> }
    }
}


define-command -hidden haskell-indent-on-new-line %{
    # Note below:
    # \S means "not \s", that is, any char but whitespace.
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # copy -- comments prefix and following white spaces
        try %{ execute-keys -draft k <a-x> s ^\h*\K--\h* <ret> y gh j P
        # If prev line is indented, indent on first non-space:
        try %¤ execute-keys -draft k x X s ^\h+\K--.*$\n. <ret> <a-S> & ¤
        } catch %{
        # preserve previous line indent (original below, we make this in another way)
        # try %{ execute-keys -draft \; K <a-&> }
        #
        haskell-indent-on-nl-function-sigs
        haskell-indent-on-fsig-arrow
        #
        # If prev line is indented, indent on first non-space:
        # If there is [ or (, don't indent.  (They are on 'on-opening'.)
        # Note the dirty thing with closing brace (zero or one) between end-of-line
        # and newline char...  Somehow braces don't seem to work well here.
        try %¤ execute-keys -draft k x X s ^\h+\K[^\[\(\{\s].*$\}?\n. <ret> <a-S> & ¤
        #
        haskell-indent-on-nl-bar-char
        #
        haskell-indent-on-nl-opening
        #
        haskell-indent-to-fst-clause
        #
        # filter previous line
        try %{ execute-keys -draft k : haskell-filter-around-selections <ret> }
        #
        haskell-indent-on-nl-where
        haskell-indent-on-nl-import
        #
        # Indent after lines beginning with condition or ending with expression or
        # with any of =&$+*-/,
        # 'Let' and 'where' removed as it is handled by the fst-clause.
        # This adds indentation.
        # But not for lines whose prev lines are comments or start with import.
        try %¤ execute-keys -draft <a-x> <a-K> (^\h*--[\w\h])|(^where\h+)|(^import\h+) <ret> k x <a-k> ^\h*(if)|(case\h+[\w']+\h+of|\bdo|\bmdo|\brec|[=/\$\*\+&,]|(?<!-)-)$ <ret> j <a-gt> ¤
        }
    }
}


# Note that the line maybe indented already to fst non-whitespace.
# Or not.
# And the indentation can be after 'prev line indentation' or after 'on-opening'.
# Thus we split this into two cases.
define-command -hidden haskell-indent-to-fst-clause %{
    evaluate-commands -draft -itersel %{
        execute-keys \;
        # The following is for cases where the line is already indented.
        try %{ execute-keys -draft <a-x> <a-k> ^\h+ <ret> k x <a-K> (\bdo\h+$) <ret> X s (\hif|\hthen|\helse)?\h*(([\w']+\h+)+=)?\h*(case\h+[\w']+\h+of|\bdo|\blet)\h*\K\h\S.*$\n\h+ <ret> <a-S> & }
        # The following is for the cases when the line is not indented already.
        try %{ execute-keys -draft <a-x> <a-k> ^(\S|\n) <ret> k x <a-K> (\bdo\h+$) <ret> X s (\hif|\hthen|\helse)?\h*(([\w']+\h+)+=)?\h*(case\h+[\w']+\h+of|\bdo|\blet)\h*\h\K\S*$\n. <ret> <a-S> & }
    }
}





# Initialization
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾

hook -group haskell-highlight global WinSetOption filetype=haskell %{
    add-highlighter window/haskell ref haskell
}

hook global WinSetOption filetype=haskell %¤
    set-option window extra_word_chars "'"
    hook window ModeChange insert:.* -group haskell-hooks  haskell-filter-around-selections
    hook window ModeChange insert:.* -group haskell-hooks  haskell-replace-arrows
    # hook window InsertChar \n -group haskell-indent haskell-indent-on-new-line
    hook window InsertChar \n -group haskell-nlmods %<
        haskell-indent-on-new-line
        haskell-on-new-line
    >
    hook window InsertChar \| %{ haskell-indent-on-bar-char  }
    hook window InsertChar \t %< exec -draft -itersel <a-x>@ >
    hook window InsertChar \} haskell-indent-on-closing
    hook window InsertChar \] haskell-indent-on-closing
    hook window InsertChar \) haskell-indent-on-closing
    hook window InsertChar ,  haskell-indent-on-comma
    hook window InsertChar > haskell-indent-on-fsig-arrow
¤

hook -group haskell-highlight global WinSetOption filetype=(?!haskell).* %{
    remove-highlighter window/haskell
}

    # remove-hooks window haskell-indent
hook global WinSetOption filetype=(?!haskell).* %{
    remove-hooks window haskell-nlmods
    remove-hooks window haskell-hooks
}




