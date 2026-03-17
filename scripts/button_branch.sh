strip_ansi() {
  sed -E 's/\x1B\[[0-9;]*[A-Za-z]//g'
#  sed -r 's/\x1B\[[0-9;]*m//g'
}

pad() {
  local text="$1"
  local vis="$2"
  local pad_len="$3"
  printf "%s%*s" "$text" $(( pad_len - ${#vis} )) ""
}

#############################################################################
# max_ints()
#
# FEATURES:
#   • Returns the maximum value passed on the arguments
#
# NOTES:
#   • Unlimited number of command line arguments allowed
#############################################################################

max_ints() {
  local max="$1"
  shift
  for n in "$@"; do
    (( 10#$n > 10#$max )) && max="$n"
  done
  printf '%s\n' "$max"
}

#############################################################################
# visible_length()
#
# FEATURE LIST:
#   • Returns the visible character count of a string
#   • Ignores ANSI escape sequences
#   • Works with literal \e or real ESC
#   • Safe for empty strings
#
# DESIGN NOTES:
#   • Uses strip_ansi() to normalize input
#   • Then counts remaining characters with ${#var}
#############################################################################

visible_length() {
    local s
    # Turn \e into real ESC, \n into newline, etc.
    s="$(printf '%b' "$1")"

    local clean
    clean="$(sed -r 's/\x1B\[[0-9;]*[ -/]*[@-~]//g' <<< "$s")"
    printf '%s\n' "${#clean}"
}

# Escape‑code‑aware pad_string() function:
# - $1 = the string to pad (may be empty, may contain ANSI escape codes)
# - $2 = the padding character (exactly one character)
# - $3 = the total desired visible length
# - Return value (printed) = the visible length of the padded string
# - Output (via echo) = the padded string itself
pad_string() {
    local input="$1"
    local pad_char="$2"
    local target_len="$3"

    local current_len=$(visible_length "${input}") ; # commpute the visible length of the string

    # if already long enough, print as-is and return length
    if (( current_len >= target_len )); then
        printf '%s' "$input"
        return
    fi

    local pad_count=$(( target_len - current_len )) ; # compute pad count

    local padding=$(printf '%*s' "${pad_count}" '' | tr ' ' "${pad_char}") ; # build padding

    printf '%s' "${input}${padding}" ;  # return input string with padding

}







####################################

####################################

####################################

####################################

####################################

####################################

####################################

####################################

####################################




#############################################################################
# left_pad()
#
# FEATURE LIST:
#   • Pads a string on the LEFT to reach a target visible width
#   • ANSI‑aware: escape codes do not count toward width
#   • Supports multi‑character padding strings
#   • Safe for empty strings
#
# DESIGN NOTES:
#   • Uses visible_length() to compute printable width
#   • Repeats pad string as many times as needed, then trims to fit
#############################################################################
left_pad() {
    local input="$1"
    local pad="$2"
    local width="$3"

    local vis_len
    vis_len="$(visible_length "$input")"

    (( vis_len >= width )) && { printf '%s' "$input"; return; }

    local need=$(( width - vis_len ))

    # Build repeated pad string
    local repeated=""
    while (( ${#repeated} < need )); do
        repeated+="$pad"
    done

    # Trim to exact size
    local padding="${repeated:0:need}"

    printf '%s%s' "$padding" "$input"
}


#############################################################################
# right_pad()
#
# FEATURE LIST:
#   • Pads a string on the RIGHT to reach a target visible width
#   • ANSI‑aware
#   • Supports multi‑character padding strings
#   • Safe for empty strings
#
# DESIGN NOTES:
#   • Same logic as left_pad(), but padding is appended
#############################################################################
right_pad() {
    local input="$1"
    local pad="$2"
    local width="$3"

    local vis_len
    vis_len="$(visible_length "$input")"

    (( vis_len >= width )) && { printf '%s' "$input"; return; }

    local need=$(( width - vis_len ))

    local repeated=""
    while (( ${#repeated} < need )); do
        repeated+="$pad"
    done

    local padding="${repeated:0:need}"

    printf '%s%s' "$input" "$padding"
}



#############################################################################
# center_string()
#
# FEATURE LIST:
#   • Centers a string within a given visible width
#   • ANSI‑aware
#   • Supports multi‑character padding strings
#   • Safe for empty strings
#
# DESIGN NOTES:
#   • Left side receives the extra space when width is odd
#   • Uses the same pad‑repeat logic as left/right pad
#############################################################################
center_string() {
    local input="$1"
    local pad="$2"
    local width="$3"

    local vis_len
    vis_len="$(visible_length "$input")"

    (( vis_len >= width )) && { printf '%s' "$input"; return; }

    local need=$(( width - vis_len ))
    local left=$(( need / 2 ))
    local right=$(( need - left ))

    local rep=""
    while (( ${#rep} < need )); do rep+="$pad"; done

    local left_pad="${rep:0:left}"
    local right_pad="${rep:left:right}"

    printf '%s%s%s' "$left_pad" "$input" "$right_pad"
}



#############################################################################
# full_justify()
#
# FEATURE LIST:
#   • Expands padding BETWEEN words to fill a target width
#   • ANSI‑aware
#   • Preserves original word order
#   • Safe for single‑word strings (falls back to right_pad)
#
# DESIGN NOTES:
#   • Splits on spaces (not regex)
#   • Distributes extra spaces as evenly as possible
#   • Uses visible_length() to compute printable width
#############################################################################
full_justify() {
    local input="$1"
    local pad="$2"
    local width="$3"

    # Split into words
    read -ra words <<< "$(strip_ansi "$input")"

    # Single word → right pad
    if (( ${#words[@]} == 1 )); then
        right_pad "$input" "$pad" "$width"
        return
    fi

    local vis_len
    vis_len="$(visible_length "$input")"

    (( vis_len >= width )) && { printf '%s' "$input"; return; }

    local gaps=$(( ${#words[@]} - 1 ))
    local need=$(( width - vis_len ))

    local base=$(( need / gaps ))
    local extra=$(( need % gaps ))

    local result=""
    local i

    for (( i=0; i<gaps; i++ )); do
        result+="${words[i]}"

        # Build padding for this gap
        local gap_pad=""
        local count=$(( base + (i < extra ? 1 : 0) ))

        while (( ${#gap_pad} < count )); do gap_pad+="$pad"; done
        gap_pad="${gap_pad:0:count}"

        result+="$gap_pad"
    done

    result+="${words[-1]}"

    printf '%s' "$result"
}


####################################



git status
git branch | grep "^\*"
branch=$(git branch | grep "^\*")
branch="${branch:2}"                      ; # remove first two characters
git status --short
git add -n .
staged_files=$(git add -n .)
staged_files="${staged_files//add /  }"   ; # replace "add " with "  "
commit_message="${branch//-/ }"           ; # remove - (dashes) from branch text
issue_number="${commit_message%%[!0-9]*}" ; # everything up to the first non-digit plus space
echo "${issue_number}"
commit_message="${commit_message#[0-9]* }" ; # remove any leading number
strip_debug="/c/lazarus/fpc/3.2.2/bin/x86_64-win64/strip.exe"
top_clock_exe="top_clock/top_clock.exe"

echo
echo "Push Branch Process $(date '+%a, %b %d, %Y %I:%M:%S %p')"
echo "On Branch (${branch})"
echo "Files staged with 'git add .'"
echo "${staged_files}"
echo "Commit message (${commit_message})"
echo
echo "ls -l ${top_clock_exe} && ${strip_debug} ${top_clock_exe} && ls -l ${top_clock_exe}"
echo "git add ."
echo "git commit -m \"${commit_message} (#${issue_number})\""
echo "git push -u origin ${branch}"
echo

RED=$'\e[31m'
WHITE=$'\e[37m'
BLUE=$'\e[34m'
RESET=$'\e[0m'

BOLD_RED=$'\e[1;31m'
BOLD_WHITE=$'\e[1;37m'
BOLD_BLUE=$'\e[1;34m'

note_1="\e[32m${top_clock_exe} appears to be running\e[0m"
note_2="Make sure to \e[33mhalt $(basename "${top_clock_exe}")\e[0m before stripping debug info"
note_3="Made in the ${RED}U${RESET}.${WHITE}S${RESET}.${BLUE}A${RESET}."

len_1=$(visible_length "${note_1}")
len_2=$(visible_length "${note_2}")
len_3=$(visible_length "${note_3}")

#max_len=$(( len_1 > len_2 ? len_1 : len_2 ))

#max_len=${len_1}
#(( len_2 > max_len )) && max_len="${len_2}"
#(( len_3 > max_len )) && max_len="${len_3}"

max_len=$(max_ints "${len_1}" "${len_2}" "${len_3}")

note_1_padded=$(pad_string "${note_1}" " " "${max_len}")
note_2_padded=$(pad_string "${note_2}" " " "${max_len}")
note_3_padded=$(pad_string "${note_3}" " " "${max_len}")

stars="*"; stars="$(pad_string "${stars}" "${stars}" "${max_len}")"
empty=" "; empty="$(pad_string "${empty}" "${empty}" "${max_len}")"

if tasklist | grep -qi "$(basename "${top_clock_exe}")"; then
  echo
  echo -e "  * ${stars} *"
  echo -e "  * ${empty} *"
  echo -e "  * ${note_1_padded} *"
  echo -e "  * ${note_2_padded} *"
  echo -e "  * ${note_3_padded} *"
  echo -e "  * ${empty} *"
  echo -e "  * ${stars} *"
  echo
fi
echo "Push Branch Process $(date '+%a, %b %d, %Y %I:%M:%S %p')"

exit 0

