# visible_length "string with \e[31mcolors\e[0m and other escapes"
# Returns the number of printable/visible characters (stdout)
visible_length_1() {
    local str="$1"

    # Remove common ANSI escape sequences
    local cleaned

    # Most common form: CSI + parameters + letter
    cleaned="${str//$'\\e['[0-9;]*[A-Za-z]/}"

    # Remove other control sequences (OSC, DCS, etc.) roughly
    cleaned="${cleaned//$'\\e]'.*?\\a/}"           # OSC terminated by BEL
    cleaned="${cleaned//$'\\e_P'.*?\\e\\\\}"       # DCS
    cleaned="${cleaned//$'\\e\\'.*?\\e\\\\}"       # other strings
    cleaned="${cleaned//$'\\e['[ =?]./}"           # short CSI
    cleaned="${cleaned//$'\\e'[ -@]/}"             # single-byte CSI
    cleaned="${cleaned//$'\\e'}/"                  # lone ESC

    # Count remaining characters (should be mostly printable now)
    printf '%s' "$cleaned" | wc -m
}

# ###################################

visible_length_2() {
  local s="$1"
  local len=0
  local i=0
  local char

  while (( i < ${#s} )); do
    char="${s:i:1}"

    if [[ $char == $'\e' ]]; then
      # Skip ESC
      ((i++))

      # Skip '[' if present (CSI sequence)
      if [[ ${s:i:1} == "[" ]]; then
        ((i++))
        # Skip parameter bytes: digits and semicolons
        while (( i < ${#s} )) && [[ ${s:i:1} =~ [0-9\;] ]]; do
          ((i++))
        done
        # Skip final byte (letter or punctuation)
        ((i < ${#s} )) && ((i++))
      fi
    else
      ((len++))
      ((i++))
    fi
  done

  printf '%d\n' "$len"
}

# ###################################


visible_length_3() {
    local clean
    clean="$(sed -r 's/\x1B\[[0-9;]*[ -/]*[@-~]//g' <<< "$1")"
    printf '%s\n' "${#clean}"
}



# ###################################

visible_length_4() {
    local s
    # Turn \e into real ESC, \n into newline, etc.
    s="$(printf '%b' "$1")"

    local clean
    clean="$(sed -r 's/\x1B\[[0-9;]*[ -/]*[@-~]//g' <<< "$s")"
    printf '%s\n' "${#clean}"
}

# ###################################



# ###################################





top_clock_exe="top_clock.exe"
note_1="\e[32m${top_clock_exe}\e[0m \e[31mappears\e[0m to be running"
note_2="Make sure to \e[33mhalt $(basename "${top_clock_exe}")\e[0m before stripping debug info"
len_1_note_1=$(visible_length_1 "${note_1}")
len_1_note_2=$(visible_length_1 "${note_2}")
len_2_note_1=$(visible_length_2 "${note_1}")
len_2_note_2=$(visible_length_2 "${note_2}")
len_3_note_1=$(visible_length_3 "${note_1}")
len_3_note_2=$(visible_length_3 "${note_2}")
len_4_note_1=$(visible_length_4 "${note_1}")
len_4_note_2=$(visible_length_4 "${note_2}")

echo    "note_1=$note_1"
echo    "note_2=$note_2"
echo    "                1         2         3         4         5         6         7"
echo    "       1234567890123456789012345678901234567890123456789012345678901234567890123456789"
echo -e "note_1=$note_1"
echo -e "note_2=$note_2"
echo    "visible_length_1(note_1)=$len_1_note_1"
echo    "visible_length_1(note_2)=$len_1_note_2"
echo    "visible_length_2(note_1)=$len_2_note_1"
echo    "visible_length_2(note_2)=$len_2_note_2"
echo    "visible_length_3(note_1)=$len_3_note_1"
echo    "visible_length_3(note_2)=$len_3_note_2"
echo    "visible_length_4(note_1)=$len_4_note_1"
echo    "visible_length_4(note_2)=$len_4_note_2"

echo


