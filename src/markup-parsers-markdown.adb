------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Markup.Tools;

package body Markup.Parsers.Markdown is

   package Fixed renames Ada.Strings.Fixed;
   package Latin_1 renames Ada.Characters.Latin_1;
   package Maps renames Ada.Strings.Maps;
   package Slices renames Natools.String_Slices;
   package Sets renames Natools.String_Slices.Slice_Sets;


   ------------------------------
   -- Local helper subprograms --
   ------------------------------

   function Default_State return Markdown_State;

   function Indent_Length (S : in String) return Natural;

   procedure Initialize_If_Needed (Ref : in out State_Refs.Reference);

   function Line_Beginning (S : in String) return Positive;

   function Ordered_Marker_Length (S : String) return Natural;
   function Unordered_Marker_Length (S : String) return Natural;

   procedure Remove_Escape (Text : in out Sets.Slice_Set);
   --  Remove '\' characters from the given text

   procedure Remove_Indent (Text : in out Sets.Slice_Set);
   --  Remove a level of indentation (a tab or four spaces) from the text.



   function Default_State return Markdown_State is
   begin
      return Markdown_State'(others => <>);
   end Default_State;


   procedure Initialize_If_Needed (Ref : in out State_Refs.Reference) is
   begin
      if Ref.Is_Empty then
         Ref.Replace (Default_State'Access);
      end if;
   end Initialize_If_Needed;


   function Line_Beginning (S : in String) return Positive is
   begin
      if S'First in S'Range and then S (S'First) = ' ' then
         if S'First + 1 in S'Range and then S (S'First + 1) = ' ' then
            if S'First + 2 in S'Range and then S (S'First + 2) = ' ' then
               if S'First + 3 in S'Range and then S (S'First + 3) = ' ' then
                  return S'First;
               else
                  return S'First + 3;
               end if;
            else
               return S'First + 2;
            end if;
         else
            return S'First + 1;
         end if;
      else
         return S'First;
      end if;
   end Line_Beginning;


   function Ordered_Marker_Length (S : String) return Natural is
      First : constant Positive := Line_Beginning (S);
      N : Natural;
   begin
      if First not in S'Range then
         return 0;
      end if;

      N := Fixed.Index (S, Tools.Digit_Set, First, Ada.Strings.Outside);
      if N > First
        and then N + 1 in S'Range
        and then S (N) = '.'
        and then Maps.Is_In (S (N + 1), Tools.Spaces)
      then
         return N + 2 - S'First;
      else
         return 0;
      end if;
   end Ordered_Marker_Length;


   function Unordered_Marker_Length (S : String) return Natural is
      First : constant Positive := Line_Beginning (S);
   begin
      if First + 1 in S'Range
        and then (S (First) = '*' or S (First) = '+' or S (First) = '-')
        and then Maps.Is_In (S (First + 1), Tools.Spaces)
      then
         return First + 2 - S'First;
      else
         return 0;
      end if;
   end Unordered_Marker_Length;


   procedure Remove_Escape (Text : in out Sets.Slice_Set) is
      N, P : Natural := Text.First;
   begin
      if Text.Is_Empty then
         return;
      end if;

      loop
         N := Text.Index (Maps.To_Set ('\'), N);
         exit when N = 0;
         P := N;
         Text.Next (N);
         exit when N = 0;
         Text.Exclude_Slice (Slices.String_Range'(P, 1));
         Text.Next (N);
         exit when N = 0;
      end loop;
   end Remove_Escape;


   function Indent_Length (S : in String) return Natural is
   begin
      if S'First in S'Range then
         if S (S'First) = Latin_1.HT then
            return 1;
         elsif S'First + 3 in S'Range
           and then S (S'First .. S'First + 3) = "    "
         then
            return 4;
         end if;
      end if;
      return 0;
   end Indent_Length;


   procedure Remove_Indent (Text : in out Sets.Slice_Set) is
      function Trim (S : in String) return Slices.String_Range;

      function Trim (S : in String) return Slices.String_Range is
         L : constant Natural := Indent_Length (S);
      begin
         return (S'First + L, S'Length - L);
      end Trim;
   begin
      Text.Trim_Slices (Trim'Access);
   end Remove_Indent;



   --------------------------------
   -- Private helper subprograms --
   --------------------------------

   function "<" (Left, Right : Link_Id) return Boolean is
   begin
      return Left'Length < Right'Length
        or else (Left'Length = Right'Length
                 and then String (Left) < String (Right));
   end "<";


   function To_Key (Id : String) return Link_Id is
      Key : Link_Id (1 .. Id'Length);
      Index_Key : Natural := Key'First;
      In_Blanks : Boolean := True;
   begin
      for Index_Id in Id'Range loop
         if Maps.Is_In (Id (Index_Id), Tools.Blanks) then
            if not In_Blanks then
               Key (Index_Key) := ' ';
               Index_Key := Index_Key + 1;
            end if;
            In_Blanks := True;
         elsif Id (Index_Id) in 'A' .. 'Z' then
            --  UTF-8-proof case normalization
            Key (Index_Key)
              := Character'Val (Character'Pos (Id (Index_Id)) + 32);
            Index_Key := Index_Key + 1;
            In_Blanks := False;
         else
            Key (Index_Key) := Id (Index_Id);
            Index_Key := Index_Key + 1;
            In_Blanks := False;
         end if;
      end loop;

      loop
         Index_Key := Index_Key - 1;
         exit when Index_Key not in Key'Range or else Key (Index_Key) /= ' ';
      end loop;

      return Key (Key'First .. Index_Key);
   end To_Key;



   -------------------------------
   -- Public helper subprograms --
   -------------------------------

   function Html4_Block_Tags return String_Sets.Set is
      Result : String_Sets.Set;
   begin
      Result.Insert ("p");
      Result.Insert ("dl");
      Result.Insert ("h1");
      Result.Insert ("h2");
      Result.Insert ("h3");
      Result.Insert ("h4");
      Result.Insert ("h5");
      Result.Insert ("h6");
      Result.Insert ("ol");
      Result.Insert ("ul");
      Result.Insert ("del");
      Result.Insert ("div");
      Result.Insert ("ins");
      Result.Insert ("pre");
      Result.Insert ("form");
      Result.Insert ("math");
      Result.Insert ("table");
      Result.Insert ("iframe");
      Result.Insert ("script");
      Result.Insert ("fieldset");
      Result.Insert ("noscript");
      Result.Insert ("blockquote");
      return Result;
   end Html4_Block_Tags;


   function Shortlex_Icase_Less_Than (Left, Right : String) return Boolean is
   begin
      return Left'Length < Right'Length
        or else (Left'Length = Right'Length
          and then Ada.Characters.Handling.To_Lower (Left)
                 < Ada.Characters.Handling.To_Lower (Right));
   end Shortlex_Icase_Less_Than;





   --------------------
   -- Markdown state --
   --------------------

   procedure Process_Blocks
     (State : in out Markdown_State;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set;
      Current : in out Element_Callback'Class) is
   begin
      if State.Level < State.Max_Level then
         State.Level := State.Level + 1;
         Process (State.Blocks, Text);
         State.Level := State.Level - 1;
      end if;

      if not Text.Is_Empty then
         Current.Append (Text.To_String);
         Text.Clear;
      end if;
   end Process_Blocks;


   procedure Process_Spans
     (State : in out Markdown_State;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set;
      Current : in out Element_Callback'Class) is
   begin
      if State.Level < State.Max_Level then
         State.Level := State.Level + 1;
         Process (State.Spans, Text, Current);
         State.Level := State.Level - 1;
      end if;

      if not Text.Is_Empty then
         Current.Append (Text.To_String);
         Text.Clear;
      end if;
   end Process_Spans;



   --------------------------------
   -- Basic parser configuration --
   --------------------------------

   function Max_Nesting_Level (Parser : Markdown_Parser) return Positive is
   begin
      if Parser.Ref.Is_Empty then
         return Default_State.Max_Level;
      else
         return Parser.Ref.Query.Data.Max_Level;
      end if;
   end Max_Nesting_Level;


   procedure Set_Max_Nesting_Level
     (Parser : in out Markdown_Parser;
      Max_Level : in Positive) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Max_Level := Max_Level;
   end Set_Max_Nesting_Level;



   --------------------------------
   -- Element callback registers --
   --------------------------------

   procedure Atx_Header
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Level_Max : in Positive := Positive'Last) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.Atx_Header'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Element),
         Level_Max => Level_Max));
   end Atx_Header;


   procedure Code_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.Code_Block'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Element)));
   end Code_Block;


   procedure Html_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Allowed_Tags : in String_Sets.Set := Html4_Block_Tags) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.Html_Block'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Element),
         Allowed_Tags => Allowed_Tags));
   end Html_Block;


   procedure Html_Comment_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.Html_Comment'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Element),
         Include_Following_Blanks => True));
   end Html_Comment_Block;


   procedure List
     (Parser : in out Markdown_Parser;
      List_Element : in Element_Callback'Class;
      Item_Element : in Element_Callback'Class;
      Order : in Styles.Ordering := Styles.Any_Order;
      Kind : in Styles.List_Item := Styles.Any_Item) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.List'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (List_Element),
         Item => Element_Holders.To_Holder (Item_Element),
         Order => Order,
         Kind => Kind));
   end List;


   procedure Paragraph
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Set_Fallback (Tokenizers.Paragraph'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Element)));
   end Paragraph;


   procedure Quote_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.Quote_Block'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Element)));
   end Quote_Block;


   procedure Setext_Header
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Level_Max : in Positive := Positive'Last) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.Setext_Header'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Element),
         Level_Max => Level_Max));
   end Setext_Header;


   procedure Code_Span
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Spans.Add_Tokenizer
        ('`',
         Tokenizers.Code_Span'(Parser.Ref,
                               Element_Holders.To_Holder (Element)));
   end Code_Span;


   procedure Emphasis
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Level : in Positive := 1;
      Markers : in String := "*_") is
   begin
      Initialize_If_Needed (Parser.Ref);

      for I in Markers'Range loop
         Parser.Ref.Update.Data.Spans.Add_Tokenizer
           (Markers (I),
            Tokenizers.Emphasis'(Parser.Ref,
                                 Element_Holders.To_Holder (Element),
                                 Level));
      end loop;
   end Emphasis;


   procedure Entity
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Spans.Add_Tokenizer
        ('&',
         Tokenizers.Entity'(Parser.Ref,
                            Element_Holders.To_Holder (Element)));
   end Entity;


   procedure Escape
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Spans.Add_Tokenizer
        ('\',
         Tokenizers.Escape'(Parser.Ref,
                            Element_Holders.To_Holder (Element)));
   end Escape;


   procedure Image
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True)) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Spans.Add_Tokenizer
        ('!',
         Tokenizers.Link'(Parser.Ref,
                          Element_Holders.To_Holder (Element),
                          Style_Set,
                          Prefix => True,
                          Recurse => False));
   end Image;


   procedure Link
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True)) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Spans.Add_Tokenizer
        ('[',
         Tokenizers.Link'(Parser.Ref,
                          Element_Holders.To_Holder (Element),
                          Style_Set,
                          Prefix => False,
                          Recurse => True));
   end Link;



   ----------------------
   -- Public interface --
   ----------------------

   procedure First_Pass
     (Parser : in out Markdown_Parser;
      Input  : in Natools.String_Slices.Slice;
      Output : out Natools.String_Slices.Slice_Sets.Slice_Set)
   is
      procedure Add_Title (From : in Slices.String_Range);
      function Process_Link_Ref (S : in String) return Slices.String_Range;

      Latest_Link : Link_Maps.Cursor;

      procedure Add_Title (From : in Slices.String_Range) is
         procedure Process (Key : in Link_Id; Element : in out Link_Info);

         procedure Process (Key : in Link_Id; Element : in out Link_Info) is
            pragma Unreferenced (Key);
         begin
            Element.Title := Output.Subset (From).To_Slice;
         end Process;
      begin
         Parser.Ref.Update.Data.Link_Map.Update_Element
           (Latest_Link, Process'Access);
      end Add_Title;

      function Process_Link_Ref (S : in String) return Slices.String_Range is
         Id_Range, Link_Range, Title_Range : Slices.String_Range := (1, 0);
         Result : Slices.String_Range := (S'First, S'Length);
         I : Positive := Line_Beginning (S);
         N : Natural;
      begin
         if I not in S'Range or else S (I) /= '[' then
            if Link_Maps.Has_Element (Latest_Link) then
               --  Try to add a title line after id+link
               N := Fixed.Index (S, Tools.Blanks, Ada.Strings.Outside);
               if N not in S'Range
                 or else not (S (N) = '"' or S (N) = ''' or S (N) = '(')
               then
                  Latest_Link := Link_Maps.No_Element;
                  return Result;
               end if;
               Title_Range.First := N + 1;

               N := Fixed.Index (S, Tools.Blanks,
                                 Ada.Strings.Outside, Ada.Strings.Backward);
               if N not in S'Range
                 or else not (S (N) = '"' or S (N) = ''' or S (N) = ')')
               then
                  Latest_Link := Link_Maps.No_Element;
                  return Result;
               end if;
               Slices.Set_Last (Title_Range, N - 1);
               Add_Title (Title_Range);
               Result.Length := 0;
            end if;
            return Result;
         end if;

         --  Parse id: anything between brackets and a colon

         N := Fixed.Index (S, Tools.Blanks, I + 1, Ada.Strings.Outside);
         if N not in S'Range or else S (N) = ']' then
            return Result;
         end if;
         Id_Range.First := N;
         N := Fixed.Index (S, Maps.To_Set (']'), N);
         if N not in S'Range or else N + 2 not in S'Range
           or else S (N + 1) /= ':'
         then
            return Result;
         end if;
         I := N + 2;
         N := Fixed.Index (S, Tools.Blanks, N - 1,
                           Ada.Strings.Outside, Ada.Strings.Backward);
         Slices.Set_Last (Id_Range, N);

         --  Parse link: the first sequence of non-blanks

         N := Fixed.Index (S, Tools.Blanks, I, Ada.Strings.Outside);
         if N not in S'Range or N + 1 not in S'Range then
            Id_Range.Length := 0;
            return Result;
         end if;
         if S (N) = '<' then
            Link_Range.First := N + 1;
         else
            Link_Range.First := N;
         end if;
         N := Fixed.Index (S, Tools.Blanks, N + 1);
         if N not in S'Range then
            N := S'Last;
         else
            N := N - 1;
         end if;
         if S (N) = '>' then
            Slices.Set_Last (Link_Range, N - 1);
         else
            Slices.Set_Last (Link_Range, N);
         end if;

         --  At this point the link is valid and the line removed

         Result.Length := 0;

         --  Parse title: the remaining non-blanks

         if N + 1 in S'Range then
            N := Fixed.Index (S, Tools.Blanks, N + 1, Ada.Strings.Outside);
            if N > 0 then
               if S (N) = ''' or S (N) = '"' or S (N) = '(' then
                  Title_Range.First := N + 1;
               else
                  Title_Range.First := N;
               end if;

               N := Fixed.Index (S, Tools.Blanks,
                                 Ada.Strings.Outside, Ada.Strings.Backward);
               if S (N) = ''' or S (N) = '"' or S (N) = ')' then
                  Slices.Set_Last (Title_Range, N - 1);
               else
                  Slices.Set_Last (Title_Range, N);
               end if;
            end if;
         end if;

         --  Insert the link into the database

         declare
            Inserted : Boolean;
         begin
            Parser.Ref.Update.Data.Link_Map.Insert
              (Key => To_Key (Output.Subset (Id_Range).To_String),
               New_Item => (Link => Output.Subset (Link_Range).To_Slice,
                            Title => Output.Subset (Title_Range).To_Slice),
               Position => Latest_Link,
               Inserted => Inserted);
            if not Inserted then
               Latest_Link := Link_Maps.No_Element;
            end if;
         end;

         return Result;
      end Process_Link_Ref;

      Position : Natural;
      N : Natural;
   begin
      Output := Sets.To_Slice_Set (Input);
      Position := Output.First;

      loop
         N := Output.Index (Tools.Eols, Position);
         exit when N = 0;

         Position := Output.Next (N);
         exit when Position = 0;

         if Maps.Is_In (Output.Element (Position), Tools.Eols)
           and then Output.Element (Position) /= Output.Element (N)
         then
            N := Position;
            Position := Output.Next (N);
            exit when Position = 0;
         end if;

         Output.Cut_Before (Position);
      end loop;

      Output.Trim_Slices (Process_Link_Ref'Access);
   end First_Pass;


   procedure Second_Pass
     (Parser : in out Markdown_Parser;
      Text : in Natools.String_Slices.Slice_Sets.Slice_Set)
   is
      Remaining : Sets.Slice_Set := Text;
   begin
      Process (Parser.Ref.Update.Data.Blocks, Remaining);
   end Second_Pass;


   procedure Process
     (Parser : in out Markdown_Parser;
      Text : in Natools.String_Slices.Slice)
   is
      Preprocessed : Natools.String_Slices.Slice_Sets.Slice_Set;
   begin
      First_Pass (Parser, Text, Preprocessed);
      Process (Parser.Ref.Update.Data.Blocks, Preprocessed);
   end Process;



   --------------------
   -- Lexer elements --
   --------------------

   package body Tokenizers is

      ------------------------------
      -- Block element tokenizers --
      ------------------------------

      procedure Process
        (Object : in out Atx_Header;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         Text_First : constant Positive := Text.First;
         Line_Last : Natural;
         Position : Natural := Text_First;
         Level : Natural := 0;
         N : Natural;
      begin
         while Text.Element (Position) = '#' loop
            Level := Level + 1;
            Text.Next (Position);
            if Position = 0 then
               return;
            end if;
            exit when Level >= Object.Level_Max;
         end loop;

         if Level = 0 then
            return;
         end if;

         N := Text.Index (Tools.Blanks, Position, Ada.Strings.Outside);
         if N = 0 then
            return;
         else
            Position := N;
         end if;

         N := Text.Index (Tools.Eols, Position);
         if N = 0 then
            N := Text.Last;
            Line_Last := Text.Last;
         else
            Line_Last := Text.Index (Tools.Blanks, N, Ada.Strings.Outside);
            if Line_Last > 0 then
               Text.Previous (Line_Last);
            else
               Line_Last := Text.Last;
            end if;
         end if;

         N := Text.Index (Tools.Blanks, N,
                          Ada.Strings.Outside, Ada.Strings.Backward);
         if N < Position then
            return;
         end if;

         N := Text.Index (Maps.To_Set ('#'), N,
                          Ada.Strings.Outside, Ada.Strings.Backward);
         if N < Position then
            return;
         end if;

         N := Text.Index (Tools.Blanks, N,
                          Ada.Strings.Outside, Ada.Strings.Backward);
         if N < Position then
            return;
         end if;

         declare
            Contents : Sets.Slice_Set
              := Text.Subset (Slices.To_Range (Position, N));
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            if Element in With_Level'Class then
               Set_Level (With_Level'Class (Element), Level);
            end if;

            Element.Open;
            Process_Spans (Object.State.Update.Data.all, Contents, Element);
            Element.Close;
         end;

         Text.Exclude_Slice (Slices.To_Range (Text_First, Line_Last));
      end Process;


      procedure Process
        (Object : in out Code_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Process_Line (S : in String) return Boolean;
         --  Look for the first non-indented line after any blank line.

         Text_First : constant Positive := Text.First;
         Content_Last : Natural := 0;
         Blank_Last : Natural := 0;

         function Process_Line (S : in String) return Boolean is
         begin
            if Tools.Is_Blank (S) then
               Blank_Last := S'Last;
               return False;
            elsif Blank_Last > 0 and then Content_Last = 0 then
               --  Abort after leading blank lines
               return True;
            else
               if Blank_Last >= Content_Last
                 and then Indent_Length (S) = 0
               then
                  return True;
               else
                  Content_Last := S'Last;
                  return False;
               end if;
            end if;
         end Process_Line;

         Discarded : Slices.String_Range;
      begin
         Discarded := Text.Find_Slice (Process_Line'Access);
         pragma Unreferenced (Discarded);

         if Content_Last > 0 then
            declare
               Contents : Sets.Slice_Set
                 := Text.Subset (Slices.To_Range (Text_First, Content_Last));
               Element : Element_Callback'Class := Object.Backend.Element;
            begin
               Remove_Indent (Contents);
               Element.Open;
               Element.Append (Contents.To_String);
               Element.Close;
            end;
         end if;

         if Blank_Last > Content_Last then
            Text.Exclude_Slice (Slices.To_Range (Text_First, Blank_Last));
         elsif Content_Last > 0 then
            pragma Assert (Content_Last = Text.Last);
            Text.Clear;
         end if;
      end Process;


      procedure Process
        (Object : in out Html_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Process_Line (S : in String) return Boolean;

         Text_First : Natural;
         Block_Last : Natural := 0;
         Html_Last : Natural := 0;
         Tag_Last : Natural := 0;
         Tag : String_Sets.Cursor;

         function Process_Line (S : in String) return Boolean is
            N : Natural;
         begin
            if Html_Last = 0 then
               --  processing the first line

               if S (S'First) /= '<' or else S'First + 1 not in S'Range then
                  return True;
               end if;

               N := Fixed.Index (S, Tools.Alphanumeric_Set, S'First + 1,
                                 Ada.Strings.Outside);
               if N = 0 then
                  Tag_Last := S'Last;
               else
                  Tag_Last := N - 1;
               end if;

               Tag := Object.Allowed_Tags.Find (S (S'First + 1 .. Tag_Last));

               if not String_Sets.Has_Element (Tag) then
                  return True;
               end if;

               Html_Last := S'Last;
               return False;

            elsif Tools.Is_Blank (S) then
               Block_Last := S'Last;
               return False;

            elsif Block_Last > 0 then
               return True;

            else
               Html_Last := S'Last;
               return False;
            end if;
         end Process_Line;

         Discarded : Slices.String_Range;
         Position, N : Natural;
      begin
         Discarded := Text.Find_Slice (Process_Line'Access);
         pragma Unreferenced (Discarded);
         if Html_Last = 0 then
            return;
         end if;

         --  Check closing tag

         N := Text.Index (Tools.Blanks, Html_Last,
                          Ada.Strings.Outside, Ada.Strings.Backward);
         if N = 0 or else Text.Element (N) /= '>' then
            return;
         end if;
         Position := Text.Previous (N);

         N := Text.Index (Tools.Alphanumeric_Set, Position,
                          Ada.Strings.Outside, Ada.Strings.Backward);
         if N = 0 or else Text.Element (N) /= '/' then
            return;
         end if;

         if N < Position then
            if Text.Element (Text.Previous (N)) /= '<'
              or else Ada.Characters.Handling.To_Lower
                        (String_Sets.Element (Tag))
                   /= Ada.Characters.Handling.To_Lower
                        (Text.Subset (Slices.To_Range
                                        (Text.Next (N), Position)).To_String)
            then
               return;
            end if;
         end if;

         --  Render appropriately

         Text_First := Text.First;

         declare
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            Element.Open;
            Element.Append
             (Text.Subset (Slices.To_Range (Text_First, Html_Last)).To_String);
            Element.Close;
         end;

         if Block_Last > 0 then
            Text.Exclude_Slice (Slices.To_Range (Text_First, Block_Last));
         else
            pragma Assert (Html_Last = Text.Last);
            Text.Clear;
         end if;
      end Process;


      procedure Process
        (Object : in out Html_Comment;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         Text_First : constant Positive := Text.First;
         Position : Natural;
      begin
         if Text.Element (Text_First) /= '<' then
            return;
         end if;

         Position := Text.Next (Text_First);
         if Position = 0 or else Text.Element (Position) /= '!' then
            return;
         end if;

         Text.Next (Position);
         if Position = 0 or else Text.Element (Position) /= '-' then
            return;
         end if;

         Text.Next (Position);
         if Position = 0 or else Text.Element (Position) /= '-' then
            return;
         end if;

         Text.Next (Position);

         Process_Text : loop
            exit Process_Text when Position = 0;
            Position := Text.Index (Maps.To_Set ('-'), Position);
            exit Process_Text when Position = 0;

            Check_Closer : loop
               Text.Next (Position);
               exit Check_Closer when Position = 0
                 or else Text.Element (Position) /= '-';
               Text.Next (Position);
               exit Check_Closer when Position = 0
                 or else Text.Element (Position) /= '>';


               Render : declare
                  Span : constant Slices.String_Range
                    := Slices.To_Range (Text_First, Position);
                  Element : Element_Callback'Class := Object.Backend.Element;
               begin
                  Element.Open;
                  Element.Append (Text.Subset (Span).To_String);
                  Element.Close;
                  Text.Exclude_Slice (Span);
               end Render;

               if Object.Include_Following_Blanks then
                  declare
                     function Is_Non_Blank (S : String) return Boolean;

                     Blank_Last : Natural := 0;

                     function Is_Non_Blank (S : String) return Boolean is
                     begin
                        if Tools.Is_Blank (S) then
                           Blank_Last := S'Last;
                           return False;
                        else
                           return True;
                        end if;
                     end Is_Non_Blank;

                     Discarded : Slices.String_Range;
                  begin
                     Discarded := Text.Find_Slice (Is_Non_Blank'Access);
                     pragma Unreferenced (Discarded);
                     if Blank_Last > 0 then
                        Text.Exclude_Slice
                          (Slices.To_Range (Text.First, Blank_Last));
                     end if;
                  end;
               end if;

               return;
            end loop Check_Closer;
         end loop Process_Text;
      end Process;


      procedure Process
        (Object : in out List;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         use type Styles.List_Item;

         function Scan_Line (S : String) return Boolean;
         procedure Render_Item;
         function Process_Item (S : String) return Boolean;

         Marker_Length : access function (S : String) return Natural;
         Recurse : access procedure
           (State : in out Markdown_State;
            Text : in out Natools.String_Slices.Slice_Sets.Slice_Set;
            Current : in out Element_Callback'Class);
         List_Last : Natural := 0;
         Blank_Last : Natural := 0;
         Item_Count : Natural := 0;
         Block_Count : Positive := 1;

         function Scan_Line (S : String) return Boolean is
            N : Natural;
         begin
            if List_Last = 0 then
               --  First line of the text

               if Marker_Length = null then
                  N := Ordered_Marker_Length (S);
                  if N > 0 then
                     Marker_Length := Ordered_Marker_Length'Access;
                  else
                     N := Unordered_Marker_Length (S);
                     if N > 0 then
                        Marker_Length := Unordered_Marker_Length'Access;
                     else
                        return True;
                     end if;
                  end if;
               else
                  N := Marker_Length.all (S);
               end if;

               if N = 0 then
                  return True;
               end if;

               Item_Count := 1;
               List_Last := S'Last;
               return False;

            elsif Tools.Is_Blank (S) then
               Blank_Last := S'Last;
               return False;

            elsif Blank_Last > List_Last then
               --  First line of a non-first block

               case Object.Kind is
                  when Styles.Span =>
                     return True;
                  when Styles.Block | Styles.Any_Item =>
                     if Block_Count = 1 and Item_Count > 1 then
                        return True;
                     end if;

                     if Marker_Length (S) > 0 then
                        Item_Count := Item_Count + 1;
                        Block_Count := Block_Count + 1;
                        List_Last := S'Last;
                        return False;
                     elsif Indent_Length (S) > 0 then
                        List_Last := S'Last;
                        return False;
                     else
                        return True;
                     end if;
               end case;

            else
               if Marker_Length (S) > 0 then
                  Item_Count := Item_Count + 1;
               end if;
               List_Last := S'Last;
               return False;
            end if;
         end Scan_Line;

         Item_First : Natural := 0;
         Item_Last : Natural := 0;

         procedure Render_Item is
         begin
            if Item_Last = 0 then
               return;
            end if;

            declare
               Contents : Sets.Slice_Set
                 := Text.Subset (Slices.To_Range (Item_First, Item_Last));
               Element : Element_Callback'Class := Object.Item.Element;
            begin
               Element.Open;
               Recurse (Object.State.Update.Data.all, Contents, Element);
               Element.Close;
            end;

            Item_Last := 0;
         end Render_Item;

         function Process_Item (S : String) return Boolean is
            N : Natural;
         begin
            if S'First > List_Last then
               return True;
            end if;

            if Tools.Is_Blank (S) then
               return False;
            end if;

            N := Marker_Length (S);
            if N > 0 then
               Render_Item;
               Item_First := S'First + N;
            end if;

            Item_Last := S'Last;
            return False;
         end Process_Item;

         Discarded : Slices.String_Range;
      begin
         case Object.Order is
            when Styles.Ordered =>
               Marker_Length := Ordered_Marker_Length'Access;
            when Styles.Unordered =>
               Marker_Length := Unordered_Marker_Length'Access;
            when Styles.Any_Order =>
               Marker_Length := null;
         end case;

         Discarded := Text.Find_Slice (Scan_Line'Access);
         pragma Unreferenced (Discarded);
         if List_Last = 0
           or else (Object.Kind = Styles.Block and Block_Count = 1)
         then
            return;
         end if;

         case Object.Kind is
            when Styles.Span =>
               Recurse := Process_Spans'Access;
            when Styles.Block =>
               Recurse := Process_Blocks'Access;
            when Styles.Any_Item =>
               if Block_Count > 1 then
                  Recurse := Process_Blocks'Access;
               else
                  Recurse := Process_Spans'Access;
               end if;
         end case;

         declare
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            Element.Open;
            Discarded := Text.Find_Slice (Process_Item'Access);
            Render_Item;
            Element.Close;
         end;

         if Blank_Last > List_Last then
            Text.Exclude_Slice (Slices.To_Range (Text.First, Blank_Last));
         else
            pragma Assert (Text.Last = List_Last);
            Text.Clear;
         end if;
      end Process;


      procedure Process
        (Object : in out Paragraph;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Process_Line (S : in String) return Boolean;
         --  Look for the first non-blank line after any blank line,
         --  updating upper bounds of first non-blank and first blank parts.

         Text_First : constant Positive := Text.First;
         Content_Last : Natural := 0;
         Blank_Last : Natural := 0;

         function Process_Line (S : in String) return Boolean is
         begin
            if Tools.Is_Blank (S) then
               Blank_Last := S'Last;
               return False;
            else
               if Blank_Last > 0 then
                  return True;
               else
                  Content_Last := S'Last;
                  return False;
               end if;
            end if;
         end Process_Line;

         Discarded : Slices.String_Range;
      begin
         Discarded := Text.Find_Slice (Process_Line'Access);
         pragma Unreferenced (Discarded);

         if Content_Last > 0 then
            declare
               Contents : Sets.Slice_Set
                 := Text.Subset (Slices.To_Range (Text_First, Content_Last));
               Element : Element_Callback'Class := Object.Backend.Element;
            begin
               Element.Open;
               Process_Spans (Object.State.Update.Data.all, Contents, Element);
               Element.Close;
            end;
         end if;

         if Blank_Last = 0 then
            --  No blank line found, so its the last paragraph of the text
            pragma Assert (Content_Last = Text.Last);
            Text.Clear;
         else
            pragma Assert (Blank_Last > Content_Last);
            Text.Exclude_Slice (Slices.To_Range (Text_First, Blank_Last));
         end if;
      end Process;


      procedure Process
        (Object : in out Quote_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Process_Line (S : in String) return Boolean;
         --  Look for the first non-blank line after any blank line,
         --  updating upper bounds of first non-blank and first blank parts.

         function Trim_Marker (S : in String) return Slices.String_Range;
         --  Remove block-quote marder from the given slice.

         Text_First : constant Positive := Text.First;
         Content_Last : Natural := 0;
         Blank_Last : Natural := 0;

         function Process_Line (S : in String) return Boolean is
         begin
            if Tools.Is_Blank (S) then
               Blank_Last := S'Last;
               return False;
            elsif Blank_Last > 0 and then Content_Last = 0 then
               --  Abort after leading blank lines
               return True;
            else
               if Blank_Last >= Content_Last then
                  declare
                     First : constant Positive := Line_Beginning (S);
                  begin
                     if First in S'Range and then S (First) = '>' then
                        Content_Last := S'Last;
                        return False;
                     else
                        return True;
                     end if;
                  end;
               else
                  Content_Last := S'Last;
                  return False;
               end if;
            end if;
         end Process_Line;

         function Trim_Marker (S : in String) return Slices.String_Range is
            Result : Slices.String_Range := (S'First, S'Length);
            First : constant Positive := Line_Beginning (S);
         begin
            if First in S'Range and then S (First) = '>' then
               if First + 1 in S'Range and then S (First + 1) = ' ' then
                  Slices.Set_First (Result, First + 2);
               else
                  Slices.Set_First (Result, First + 1);
               end if;
            end if;

            return Result;
         end Trim_Marker;

         Discarded : Slices.String_Range;
      begin
         Discarded := Text.Find_Slice (Process_Line'Access);
         pragma Unreferenced (Discarded);

         if Content_Last > 0 then
            declare
               Contents : Sets.Slice_Set
                 := Text.Subset (Slices.To_Range (Text_First, Content_Last));
               Element : Element_Callback'Class := Object.Backend.Element;
            begin
               Contents.Trim_Slices (Trim_Marker'Access);
               Element.Open;
               Process_Blocks
                 (Object.State.Update.Data.all,
                  Contents,
                  Element);
               Element.Close;
            end;
         end if;

         if Blank_Last > Content_Last then
            Text.Exclude_Slice (Slices.To_Range (Text_First, Blank_Last));
         elsif Content_Last > 0 then
            pragma Assert (Content_Last = Text.Last);
            Text.Clear;
         end if;
      end Process;


      procedure Process
        (Object : in out Setext_Header;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Process_Line (S : in String) return Boolean;

         Text_First : Positive;
         Content_Last : Natural := 0;
         Blank_Last : Natural := 0;
         Level : Natural := 0;

         function Process_Line (S : in String) return Boolean is
            use type Maps.Character_Set;
            N : Natural;
         begin
            if Content_Last = 0 then
               if Tools.Is_Blank (S)
                 or else Indent_Length (S) > 0
               then
                  return True;
               end if;
               Content_Last := S'Last;
               return False;

            elsif Level = 0 then
               if S (S'First) = '=' then
                  Level := 1;
               elsif S (S'First) = '-' then
                  Level := 2;
               else
                  return True;
               end if;

               N := Fixed.Index (S, Maps.To_Set (S (S'First)),
                                 Ada.Strings.Outside);
               if N > 0
                 and then Fixed.Index
                      (S, Tools.Blanks, N, Ada.Strings.Outside) > 0
               then
                  Level := 0;
                  return True;
               end if;

               Blank_Last := S'Last;
               return False;

            elsif Tools.Is_Blank (S) then
               Blank_Last := S'Last;
               return False;
            else
               return True;
            end if;
         end Process_Line;

         Discarded : Slices.String_Range;
      begin
         Discarded := Text.Find_Slice (Process_Line'Access);
         pragma Unreferenced (Discarded);
         if Level = 0 then
            return;
         end if;

         Text_First := Text.First;

         declare
            Contents : Sets.Slice_Set
              := Text.Subset (Slices.To_Range (Text_First, Content_Last));
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            if Element in With_Level'Class then
               Set_Level (With_Level'Class (Element), Level);
            end if;

            Element.Open;
            Process_Spans (Object.State.Update.Data.all, Contents, Element);
            Element.Close;
         end;

         Text.Exclude_Slice (Slices.To_Range (Text_First, Blank_Last));
      end Process;



      -----------------------------
      -- Span element tokenizers --
      -----------------------------

      procedure Process
        (Object : in out Code_Span;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         Text_First : constant Positive := Text.First;
         Opener : constant Character := Text.Element (Text_First);
         Closer : constant Character
           := Maps.Value (Tools.Delimiter_Pairs, Opener);
         Opener_Set : constant Maps.Character_Set := Maps.To_Set (Opener);
         Closer_Set : constant Maps.Character_Set := Maps.To_Set (Closer);

         Content_First : Positive;
         Level : Positive;
         Position : Positive;
         N : Natural;
      begin
         --  Look for the beginning of actual contents

         N := Text.Index (Opener_Set, Ada.Strings.Outside);
         if N = 0 then
            return;
         end if;

         Level := N - Text_First;

         if Text.Element (N) = ' ' then
            Text.Next (N);
            if N = 0 then
               return;
            end if;
         end if;

         Content_First := N;

         --  Look for the closing marker

         Position := N;

         loop
            --  Skip non-closing characters

            N := Text.Index (Closer_Set, Position, Ada.Strings.Inside);
            if N = 0 then
               return;
            end if;
            Position := N;

            --  Check closer length

            N := Text.Index (Closer_Set, Position, Ada.Strings.Outside);
            if N = 0 then
               N := Text.Last + 1;
               if N - Position < Level then
                  return;
               end if;
            end if;
            exit when N - Position >= Level;

            --  Proceed to the next delimiter candidate

            Position := N;
         end loop;

         --  Process the code span

         N := Position + Level - 1;  --  last processed index
         Text.Previous (Position);
         if Text.Element (Position) = ' ' then
            Text.Previous (Position);
         end if;

         declare
            Span_Content : constant Sets.Slice_Set
              := Text.Subset (Slices.To_Range (Content_First, Position));
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            pragma Assert (Span_Content.Last = Position);
            Element.Open;
            Element.Append (Span_Content.To_String);
            Element.Close;
         end;

         Text.Exclude_Slice (Slices.To_Range (Text_First, N));
      end Process;


      procedure Process
        (Object : in out Emphasis;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         Text_First : constant Positive := Text.First;
         Opener : constant Character := Text.Element (Text_First);
         Closer : constant Character
           := Maps.Value (Tools.Delimiter_Pairs, Opener);
--       Opener_Set : constant Maps.Character_Set := Maps.To_Set (Opener);
         Closer_Set : constant Maps.Character_Set := Maps.To_Set (Closer);

         Position : Natural := Text.Next (Text_First);
         C : Character;
         Contents_First, Contents_Last : Positive;
      begin
         --  Check opener level

         for I in 2 .. Object.Level loop
            if Position = 0 or else Text.Element (Position) /= Opener then
               return;
            end if;
            Text.Next (Position);
         end loop;

         --  Check that opener is not follower by opener or space

         if Position = 0 then
            return;
         end if;

         C := Text.Element (Position);
         if C = Opener or C = ' ' then
            return;
         end if;
         Contents_First := Position;

         --  Look for closer

         loop
            Position := Text.Index (Closer_Set, Position);
            if Position = 0 then
               return;
            end if;

            Contents_Last := Text.Previous (Position);
            C := Text.Element (Contents_Last);

            if C = ' ' or C = '\' then
               Text.Next (Position);
               if Position = 0 then
                  return;
               end if;
            else
               C := Closer;
               for I in 2 .. Object.Level loop
                  Text.Next (Position);
                  if Position = 0 then
                     return;
                  end if;
                  C := Text.Element (Position);
                  exit when C /= Closer;
               end loop;

               exit when C = Closer;
            end if;
         end loop;

         declare
            Contents : Sets.Slice_Set
              := Text.Subset (Slices.To_Range (Contents_First,
                                               Contents_Last));
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            Element.Open;
            Process_Spans (Object.State.Update.Data.all, Contents, Element);
            Element.Close;
         end;

         Text.Exclude_Slice (Slices.To_Range (Text_First, Position));
      end Process;


      procedure Process
        (Object : in out Entity;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         Text_First : constant Positive := Text.First;
         Position : Natural := Text.Next (Text_First);
         N : Natural;
      begin
         if Position > 0 and then Text.Element (Position) = '#' then
            Text.Next (Position);
         end if;

         if Position = 0 then
            return;
         end if;

         N := Text.Index (Tools.Alphanumeric_Set, Position,
                          Ada.Strings.Outside);
         if N = 0 or else N = Position or else Text.Element (N) /= ';' then
            return;
         end if;

         declare
            Contents : constant Sets.Slice_Set
              := Text.Subset (Slices.To_Range (Text_First, N));
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            Element.Open;
            Element.Append (Contents.To_String);
            Element.Close;
         end;

         Text.Exclude_Slice (Slices.To_Range (Text_First, N));
      end Process;


      procedure Process
        (Object : in out Escape;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         Text_First : constant Positive := Text.First;
         Text_Second : constant Natural := Text.Next (Text_First);
         Element : Element_Callback'Class := Object.Backend.Element;
      begin
         if Text_Second = 0 then
            return;
         end if;

         Element.Open;
         Element.Append ((1 => Text.Element (Text_Second)));
         Element.Close;

         Text.Exclude_Slice (Slices.To_Range (Text_First, Text_Second));
      end Process;


      procedure Process
        (Object : in out Link;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         use type Styles.Link;

         Text_Open : constant Character := '[';
         Text_Close : constant Character := ']';
         Reference_Open : constant Character := '[';
         Reference_Close : constant Character := ']';
         Inline_Open : constant Character := '(';
         Inline_Close : constant Character := ')';

         Text_Delimiter : constant Maps.Character_Set
           := Maps.To_Set (String'(Text_Open, Text_Close));

         Text_First : constant Positive := Text.First;
         Span_Last : Natural;

         Style : Styles.Link;
         Text_Range, Link_Range : Slices.String_Range;

         Link_Close : Character := ' ';
         Position : Natural := Text_First;
         Level, N : Natural;
         C : Character;
      begin
         --  Skip optional prefix

         if Object.Prefix then
            Text.Next (Position);
            if Position = 0
              or else Text.Element (Position) /= Text_Open
            then
               return;
            end if;
         end if;
         Text.Next (Position);
         if Position = 0 then
            return;
         end if;
         Text_Range.First := Position;

         --  Look for text end

         Level := 0;
         loop
            if Position = 0 then
               return;
            end if;

            N := Text.Index (Text_Delimiter, Position);
            if N = 0 then
               return;
            end if;

            Position := Text.Next (N);

            if Text.Element (N) = Text_Open then
               Level := Level + 1;
            elsif Level > 0 then
               Level := Level - 1;
            else
               exit;
            end if;
         end loop;
         Slices.Set_Last (Text_Range, Text.Previous (N));
         Span_Last := N;

         --  Autodetect link style

         if Position = 0 then
            Style := Styles.Partial;
         else
            N := Text.Index (Tools.Blanks, Position, Ada.Strings.Outside);
            if N = 0 then
               Style := Styles.Partial;
               Position := 0;
            else
               Position := Text.Next (N);
               C := Text.Element (N);
               if C = Reference_Open and Object.Style (Styles.Reference) then
                  Style := Styles.Reference;
                  Link_Close := Reference_Close;
               elsif C = Inline_Open and Object.Style (Styles.Inline) then
                  Style := Styles.Inline;
                  Link_Close := Inline_Close;
               else
                  Style := Styles.Partial;
               end if;
            end if;
         end if;

         if not Object.Style (Style) then
            return;
         end if;

         --  Look for link end

         case Style is
            when Styles.Partial =>
               Link_Range := Text_Range;
            when Styles.Reference | Styles.Inline =>
               if Position = 0 then
                  return;
               end if;

               N := Text.Index (Tools.Blanks, Position, Ada.Strings.Outside);
               if N = 0 then
                  return;
               end if;

               Link_Range.First := N;
               Position := N;

               loop
                  N := Text.Index (Maps.To_Set (Link_Close), Position);
                  if N = 0 then
                     return;
                  end if;
                  Position := Text.Previous (N);
                  exit when Text.Element (Position) /= '\';
                  Position := Text.Next (N);
                  if Position = 0 then
                     return;
                  end if;
               end loop;
               Span_Last := N;
               N := Text.Index
                 (Tools.Blanks, Position,
                  Ada.Strings.Outside, Ada.Strings.Backward);
               pragma Assert (N > Slices.Last (Text_Range));
               Slices.Set_Last (Link_Range, N);

               if Link_Range.Length = 0 and Style = Styles.Reference then
                  Link_Range := Text_Range;
               end if;
         end case;

         declare
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            --  Resolve references

            case Style is
               when Styles.Partial | Styles.Reference =>
                  declare
                     Key : constant Link_Id
                       := To_Key (Text.Subset (Link_Range).To_String);
                     Cursor : constant Link_Maps.Cursor
                       := Object.State.Query.Data.Link_Map.Find (Key);
                  begin
                     if Link_Maps.Has_Element (Cursor) then
                        declare
                           Info : constant Link_Info
                             := Link_Maps.Element (Cursor);
                        begin
                           if Element in With_Link'Class then
                              Set_Link
                                (With_Link'Class (Element),
                                 Info.Link);
                           end if;

                           if Element in With_Title'Class then
                              Set_Title
                                (With_Title'Class (Element),
                                 Info.Title);
                           end if;
                        end;
                     else
                        return;
                     end if;
                  end;

               when Styles.Inline =>
                  N := Text.Index (Tools.Blanks, Link_Range.First);
                  if N > 0 and then Slices.Is_In (N, Link_Range) then
                     declare
                        Title_Range : Slices.String_Range := Link_Range;
                        Title : Sets.Slice_Set;
                     begin
                        Slices.Set_Last (Link_Range, Text.Previous (N));
                        N := Text.Index (Tools.Blanks, N, Ada.Strings.Outside);
                        C := Text.Element (N);
                        if C = ''' or C = '"' or C = '(' then
                           Text.Next (N);
                        end if;
                        Slices.Set_First (Title_Range, N);
                        C := Text.Element (Slices.Last (Title_Range));
                        if C = ''' or C = '"' or C = ')' then
                           Slices.Set_Last
                             (Title_Range,
                              Text.Previous (Slices.Last (Title_Range)));
                        end if;

                        if Element in With_Title'Class then
                           Title := Text.Subset (Title_Range);
                           Remove_Escape (Title);
                           Set_Title
                             (With_Title'Class (Element),
                              Title.To_Slice);
                        end if;
                     end;
                  end if;

                  if Element in With_Link'Class then
                     if Text.Element (Link_Range.First) = '<' then
                        Slices.Set_First
                          (Link_Range, Text.Next (Link_Range.First));
                     end if;

                     if Text.Element (Slices.Last (Link_Range)) = '>' then
                        Slices.Set_Last
                          (Link_Range,
                           Text.Previous (Slices.Last (Link_Range)));
                     end if;

                     declare
                        Link : Sets.Slice_Set := Text.Subset (Link_Range);
                     begin
                        Remove_Escape (Link);
                        Set_Link (With_Link'Class (Element), Link.To_Slice);
                     end;
                  end if;
            end case;

            --  Render the element

            Element.Open;
            if Object.Recurse then
               declare
                  Text_Part : Sets.Slice_Set := Text.Subset (Text_Range);
               begin
                  Process_Spans
                    (Object.State.Update.Data.all, Text_Part, Element);
               end;
            else
               Element.Append (Text.Subset (Text_Range).To_String);
            end if;
            Element.Close;
         end;

         Text.Exclude_Slice (Slices.To_Range (Text_First, Span_Last));
      end Process;

   end Tokenizers;

end Markup.Parsers.Markdown;
