------------------------------------------------------------------------------
-- Copyright (c) 2011-2012, Natacha Porté                                   --
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

with Ada.Strings.Fixed;

package body Markup.Tools is

   package Maps renames Ada.Strings.Maps;


   ---------------------
   -- String splicing --
   ---------------------

   function Invert (List : Splice_List; First : Positive; Last : Natural)
      return Splice_List
   is
      Result : Splice_List (1 .. List'Length + 1);
      First_Range : Positive := Result'First;
      Last_Range  : Positive := Result'Last;
   begin
      Result (1).Offset := First;
      for J in List'Range loop
         declare
            Prev : constant Positive := J - List'First + Result'First;
            Next : constant Positive := Prev + 1;
         begin
            Result (Prev).Length := List (J).Offset - Result (Prev).Offset;
            Result (Next).Offset := List (J).Offset + List (J).Length;
         end;
      end loop;
      Result (Last_Range).Length := Last - Result (Last_Range).Offset + 1;

      if Result (First_Range).Length = 0 then
         First_Range := First_Range + 1;
      end if;
      if Result (Last_Range).Length = 0 then
         Last_Range := Last_Range - 1;
      end if;

      return Result (First_Range .. Last_Range);
   end Invert;


   function Spliced_Length (List : Splice_List) return Natural is
      Accumulated : Natural := 0;
   begin
      for J in List'Range loop
         Accumulated := Accumulated + List (J).Length;
      end loop;
      return Accumulated;
   end Spliced_Length;


   function Splice (Source : String; List : Splice_List) return String is
      Result : String (1 .. Spliced_Length (List));
      Position : Positive := 1;
   begin
      for J in List'Range loop
         Result (Position .. (Position + List (J).Length - 1))
           := Source ((List (J).Offset) ..
                      (List (J).Offset + List (J).Length - 1));
         Position := Position + List (J).Length;
      end loop;
      return Result;
   end Splice;


   function Splice_Lines (Source : String) return String is
      Lines : Splice_List (1 .. Line_Count (Source));
      Position : Positive := Source'First;
      Skip : Natural;
   begin
      for J in Lines'Range loop
         Lines (J).Offset := Position;
         Position := End_Of_Line (Source, Position) + 1;
         Skip := Skip_Prefix_Length
                   (Source (Lines (J).Offset .. Position - 1));
         Lines (J).Offset := Lines (J).Offset + Skip;
         Lines (J).Length := Position - Lines (J).Offset;
      end loop;
      return Splice (Source, Lines);
   end Splice_Lines;




   ------------------------------
   -- Lexer helper subprograms --
   ------------------------------

   function End_Of_Line (Source : String; From : Natural) return Natural is
      Position : Natural := From;
   begin
      --  End of line is the first CR or LF ...
      Position := Ada.Strings.Fixed.Index (Source => Source,
                                           Set    => Eols,
                                           From   => From);
      if Position = 0 then
         return Source'Last;
      end if;

      --  ... unless marker is CR & LF or LF & CR.
      if Position + 1 in Source'Range and then
        (Maps.Is_In (Source (Position + 1), Eols) and
         Source (Position) /= Source (Position + 1))
      then
         return Position + 1;
      else
         return Position;
      end if;
   end End_Of_Line;


   procedure Escape (Output      : out String;
                     Length      : out Natural;
                     Source      : in String;
                     Escape_With : in Ada.Strings.Maps.Character_Set)
   is
      N : Natural;
      Position : Positive := Source'First;
   begin
      Length := 0;
      while Position in Source'Range loop
         N := Ada.Strings.Fixed.Index (Source => Source,
                                       Set    => Escape_With,
                                       From   => Position);
         if N > 0 and N + 1 in Source'Range then
            if N > Position then
               Output (Output'First + Length
                       .. Output'First + Length + N - Position - 1)
                 := Source (Position .. N - 1);
               Length := Length + N - Position;
            end if;
            Output (Output'First + Length) := Source (N + 1);
            Length := Length + 1;
            Position := N + 2;
         else
            declare
               Segment_Size : constant Positive := Source'Last - Position + 1;
               Output_Offset : constant Positive := Output'First + Length;
            begin
               Output (Output_Offset .. Output_Offset + Segment_Size - 1)
                 := Source (Position .. Source'Last);
               Length := Length + Segment_Size;
            end;
            return;
         end if;
      end loop;
   end Escape;


   function Escape (Source : String;
                    Escape_With : Ada.Strings.Maps.Character_Set)
      return String
   is
      Result : String (1 .. Source'Length);
      Length : Natural;
   begin
      Escape (Result, Length, Source, Escape_With);
      return Result (1 .. Length);
   end Escape;


   function Index (Source : String;
                   Set : Ada.Strings.Maps.Character_Set;
                   From  : Positive;
                   Escape_With : Ada.Strings.Maps.Character_Set;
                   Test : Ada.Strings.Membership := Ada.Strings.Inside;
                   Going : Ada.Strings.Direction := Ada.Strings.Forward)
      return Natural
   is
      N : Natural;
      Position : Positive := From;
   begin
      loop
         N := Ada.Strings.Fixed.Index (Source, Set, Position, Test, Going);
         if N = 0
           or else N - 1 not in Source'Range
           or else not Maps.Is_In (Source (N - 1), Escape_With)
         then
            return N;
         end if;
         case Going is
            when Ada.Strings.Forward  => Position := N + 1;
            when Ada.Strings.Backward => Position := N - 1;
         end case;
         if Position not in Source'Range then
            return 0;
         end if;
      end loop;
   end Index;


   function Index (Source : String;
                   Set : Ada.Strings.Maps.Character_Set;
                   Escape_With : Ada.Strings.Maps.Character_Set;
                   Test : Ada.Strings.Membership := Ada.Strings.Inside;
                   Going : Ada.Strings.Direction := Ada.Strings.Forward)
      return Natural is
   begin
      case Going is
         when Ada.Strings.Forward
            => return Index (Source, Set, Source'First,
                             Escape_With, Test, Going);
         when Ada.Strings.Backward
            => return Index (Source, Set, Source'Last,
                             Escape_With, Test, Going);
      end case;
   end Index;


   function Is_Blank (Line : String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Source => Line,
                                      Set    => Blanks,
                                      Test   => Ada.Strings.Outside) = 0;
   end Is_Blank;


   function Line_Count (Source : String) return Natural is
      Count : Natural := 0;
      Position : Natural := Source'First;
   begin
      while Position in Source'Range loop
         Count := Count + 1;
         Position := End_Of_Line (Source, Position) + 1;
      end loop;
      return Count;
   end Line_Count;


   function Skip_Blank_Lines (Source : String; From : Positive)
      return Positive
   is
      Position : Positive := From;
      N : Natural;
   begin
      if Position not in Source'Range then
         return From;
      end if;
      loop
         N := End_Of_Line (Source, Position);
         if not Is_Blank (Source (Position .. N)) then
            return Position;
         end if;
         if N = Source'Last then
            return N + 1;
         end if;
         Position := N + 1;
      end loop;
   end Skip_Blank_Lines;


   function Skip_Set (Source : String;
                      Set    : Maps.Character_Set;
                      From   : Positive)
      return Natural
   is
      I : constant Natural
        := Ada.Strings.Fixed.Index (Source => Source,
                                    Set    => Set,
                                    From   => From,
                                    Test   => Ada.Strings.Outside);
   begin
      if I = 0 then
         return Source'Last + 1;
      else
         return I;
      end if;
   end Skip_Set;

end Markup.Tools;
