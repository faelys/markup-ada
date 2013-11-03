------------------------------------------------------------------------------
-- Copyright (c) 2011-2013, Natacha Porté                                   --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Strings.Maps;

private with Ada.Characters.Latin_1;

package Markup.Tools is

   ----------------------------
   -- Lexer helper constants --
   ----------------------------

   Delimiter_Pairs : constant Ada.Strings.Maps.Character_Mapping;
      --  Map of opening characters to their closing counterpart and vice-versa
      --    (e.g. '(' -> ')', ')' -> '(', etc), while other characters are left
      --    unchanged (e.g. '*' -> '*', so that the first star-character opens
      --    and the next one closes).

   Blanks : constant Ada.Strings.Maps.Character_Set;
      --  Set of characters considered as blanks (in ASCII).

   Eols : constant Ada.Strings.Maps.Character_Set;
      --  Set of characters marking end of lines.

   Spaces : constant Ada.Strings.Maps.Character_Set;
      --  Set of horizontal white spaces.

   Digit_Set : constant Ada.Strings.Maps.Character_Set;
   Letter_Set : constant Ada.Strings.Maps.Character_Set;
   Alphanumeric_Set : constant Ada.Strings.Maps.Character_Set;



   ---------------------
   -- String splicing --
   ---------------------

   type String_Part is record
      Offset : Positive;
      Length : Natural;
   end record;

   type Splice_List is array (Positive range <>) of String_Part;


   function Invert (List : Splice_List;
                    First : Positive;
                    Last : Natural)
      return Splice_List;
      --  Return the complement of List in the slice First .. Last.
      --  Raise Constraint_Error if splice list is not in the slice.

   function Spliced_Length (List : Splice_List) return Natural;
      --  Return the sum of Length of each part.

   function Splice (Source : String;
                    List : Splice_List)
      return String;
      --  Return the concatenation of all substrings described by
      --    the splice list.

   generic
      with function Skip_Prefix_Length (Line : String) return Natural;
   function Splice_Lines (Source : String) return String;
      --  Remove a prefix of the given length from each line of Source.


   ------------------------------
   -- Lexer helper subprograms --
   ------------------------------

   function End_Of_Line (Source : String; From : Natural) return Natural;
      --  Return the index of the last character in the line containing From,
      --    i.e. the index of the next end-of-line marker, or the Source'Last
      --    when there is no such marker.

   procedure Escape (Output      : out String;
                     Length      : out Natural;
                     Source      : in String;
                     Escape_With : in Ada.Strings.Maps.Character_Set);
      --  Performe C-style escape: when a character in the set is encountered,
      --    it is skipped and the next character is unconditionnally inserted
      --    into the output string.

   function Escape (Source : String;
                    Escape_With : Ada.Strings.Maps.Character_Set)
      return String;
      --  Perform C-style escape: when a character in the set is encountered,
      --    it is skipped and the next character is unconditionnally inserted
      --    into the output string.

   function Index (Source : String;
                   Set : Ada.Strings.Maps.Character_Set;
                   From  : Positive;
                   Escape_With : Ada.Strings.Maps.Character_Set;
                   Test : Ada.Strings.Membership := Ada.Strings.Inside;
                   Going : Ada.Strings.Direction := Ada.Strings.Forward)
      return Natural;
      --  Wrapper around Ada.Strings.Fixed.Index that skips escaped matches.

   function Index (Source : String;
                   Set : Ada.Strings.Maps.Character_Set;
                   Escape_With : Ada.Strings.Maps.Character_Set;
                   Test : Ada.Strings.Membership := Ada.Strings.Inside;
                   Going : Ada.Strings.Direction := Ada.Strings.Forward)
      return Natural;
      --  Wrapper around Ada.Strings.Fixed.Index that skips escaped matches.
      --    (using Source'First or Source'Last for From depending on Going)

   function Is_Blank (Line : String) return Boolean;
      --  Return whether Line contains only blank characters.

   function Line_Count (Source : String) return Natural;
      --  Return the number of lines in Source, using CR, LF, CR & LF
      --    or LF & CR as end-of-line markers, and counting the last line even
      --    when it does not end with an end-of-line marker.

   function Skip_Blank_Lines (Source : String; From : Positive)
      return Positive;
      --  Return the lower index I >= From such that Source (I) is the first
      --    character of a non-blank line, or Source'Last + 1 when there is
      --    no such I (i.e. all the remaining lines of Source are blank).

   function Skip_Set (Source : String;
                      Set    : Ada.Strings.Maps.Character_Set;
                      From   : Positive)
      return Natural;
      --  Return the index of the first character after From that is not in
      --    the given set. That can be From (when it is already not in the set)
      --    or Source'Last + 1 (when there every character after From are in
      --    the set).


private

   package L1 renames Ada.Characters.Latin_1;

   Delimiter_Pairs : constant Ada.Strings.Maps.Character_Mapping
     := Ada.Strings.Maps.To_Mapping ("()[]{}<>", ")(][}{><");

   Blanks : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" " & L1.HT & L1.CR & L1.LF);

   Eols : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (L1.CR & L1.LF);

   Spaces : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set (" " & L1.HT);

   Digit_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set ("0123456789");

   Letter_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps.To_Set
          (Ada.Strings.Maps.Character_Ranges'(('a', 'z'), ('A', 'Z')));

   Alphanumeric_Set : constant Ada.Strings.Maps.Character_Set
     := Ada.Strings.Maps."or" (Digit_Set, Letter_Set);

end Markup.Tools;
