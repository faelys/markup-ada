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

with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

with Markup.Tools;

package body Markup.Parsers.Markdown.Extensions is

   package Fixed renames Ada.Strings.Fixed;
   package Latin_1 renames Ada.Characters.Latin_1;
   package Maps renames Ada.Strings.Maps;


   ------------------------
   -- Helper subprograms --
   ------------------------

   procedure Initialize_If_Needed
     (Ref : in out Extended_State_Refs.Reference)
   is
      function Default_State return Extended_State;

      function Default_State return Extended_State is
      begin
         return Extended_State'(others => <>);
      end Default_State;
   begin
      if Ref.Is_Empty then
         Ref.Replace (Default_State'Access);
      end if;
   end Initialize_If_Needed;



   ----------------------
   -- Public interface --
   ----------------------

   procedure Discount_Centered
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer
        (Tokenizers.Discount_Centered'
           (State => Parser.Ref,
            Backend => Element_Holders.To_Holder (Element)));
   end Discount_Centered;


   procedure Discount_Class_Block
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class)
   is
      function Check_Quote_Block (T : Tokenizer'Class) return Boolean;

      function Check_Quote_Block (T : Tokenizer'Class) return Boolean is
      begin
         return T in Markdown.Tokenizers.Quote_Block'Class
           and then T not in Tokenizers.Discount_Class_Block'Class;
      end Check_Quote_Block;
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer_Before
        (Tokenizers.Discount_Class_Block'
           (State => Parser.Ref,
            Backend => Element_Holders.To_Holder (Element)),
         Check_Quote_Block'Access);
   end Discount_Class_Block;


   procedure Discount_Definition_List
     (Parser : in out Extended_Parser;
      List_Element : in Element_Callback'Class;
      Title_Element : in Element_Callback'Class;
      Description_Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer
        (Tokenizers.Discount_Definitions'
           (State => Parser.Ref,
            Backend => Element_Holders.To_Holder (List_Element),
            Title => Element_Holders.To_Holder (Title_Element),
            Description => Element_Holders.To_Holder (Description_Element)));
   end Discount_Definition_List;


   procedure Discount_Fenced_Code_Block
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Fence_Charset : in Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set ("~`");
      Minimum_Length : in Positive := 4) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer
        (Tokenizers.Discount_Fenced_Code_Block'
           (State => Parser.Ref,
            Backend => Element_Holders.To_Holder (Element),
            Fence_Charset => Fence_Charset,
            Minimum_Length => Minimum_Length));
   end Discount_Fenced_Code_Block;


   procedure Discount_Image
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True)) is
   begin
      if Element in With_Title'Class or else Element in With_Size'Class then
         Parser.Image
           (Elements.Image_Sizer'(Backend =>
               Element_Holders.To_Holder (Element)),
            Style_Set);
      else
         Parser.Image (Element, Style_Set);
      end if;
   end Discount_Image;


   procedure PME_Definition_List
     (Parser : in out Extended_Parser;
      List_Element : in Element_Callback'Class;
      Title_Element : in Element_Callback'Class;
      Description_Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer
        (Tokenizers.PME_Definitions'
           (State => Parser.Ref,
            Backend => Element_Holders.To_Holder (List_Element),
            Title => Element_Holders.To_Holder (Title_Element),
            Description => Element_Holders.To_Holder (Description_Element)));
   end PME_Definition_List;


   procedure PME_Table
     (Parser : in out Extended_Parser;
      Table_Element : in Element_Callback'Class;
      Row_Element : in Element_Callback'Class;
      Cell_Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.PME_Table'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Table_Element),
         Row => Element_Holders.To_Holder (Row_Element),
         Header => Element_Holders.Empty_Holder,
         Data => Element_Holders.To_Holder (Cell_Element)));
   end PME_Table;


   procedure PME_Table
     (Parser : in out Extended_Parser;
      Table_Element : in Element_Callback'Class;
      Row_Element : in Element_Callback'Class;
      Header_Element : in Element_Callback'Class;
      Data_Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer (Tokenizers.PME_Table'
        (State => Parser.Ref,
         Backend => Element_Holders.To_Holder (Table_Element),
         Row => Element_Holders.To_Holder (Row_Element),
         Header => Element_Holders.To_Holder (Header_Element),
         Data => Element_Holders.To_Holder (Data_Element)));
   end PME_Table;


   procedure Pseudoprotocol_Link
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True)) is
   begin
      Initialize_If_Needed (Parser.Ext_Ref);

      Parser.Link (Elements.Pseudoprotocols'(Parser.Ext_Ref,
                                             Link | Title | Backend => <>),
                   Style_Set);

      Parser.Ext_Ref.Update.Data.all.Pseudo_Backend (Pseudoprotocol.Unknown)
        := Element_Holders.To_Holder (Element);
   end Pseudoprotocol_Link;


   procedure Pseudoprotocol_Abbr
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Initialize_If_Needed (Parser.Ext_Ref);
      Parser.Ext_Ref.Update.Data.all.Pseudo_Backend (Pseudoprotocol.Abbr)
        := Element_Holders.To_Holder (Element);
   end Pseudoprotocol_Abbr;


   procedure Pseudoprotocol_Class
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Initialize_If_Needed (Parser.Ext_Ref);
      Parser.Ext_Ref.Update.Data.all.Pseudo_Backend (Pseudoprotocol.Class)
        := Element_Holders.To_Holder (Element);
   end Pseudoprotocol_Class;


   procedure Pseudoprotocol_Id
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Initialize_If_Needed (Parser.Ext_Ref);
      Parser.Ext_Ref.Update.Data.all.Pseudo_Backend (Pseudoprotocol.Id)
        := Element_Holders.To_Holder (Element);
   end Pseudoprotocol_Id;


   procedure Pseudoprotocol_Raw
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class) is
   begin
      Initialize_If_Needed (Parser.Ref);
      Initialize_If_Needed (Parser.Ext_Ref);
      Parser.Ext_Ref.Update.Data.all.Pseudo_Backend (Pseudoprotocol.Raw)
        := Element_Holders.To_Holder (Element);
   end Pseudoprotocol_Raw;


   overriding procedure Quote_Block
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class)
   is
      function Check_Class_Block (T : Tokenizer'Class) return Boolean;

      function Check_Class_Block (T : Tokenizer'Class) return Boolean is
      begin
         return T in Tokenizers.Discount_Class_Block'Class;
      end Check_Class_Block;
   begin
      Initialize_If_Needed (Parser.Ref);
      Parser.Ref.Update.Data.Blocks.Add_Tokenizer_After
        (Markdown.Tokenizers.Quote_Block'
           (State => Parser.Ref,
            Backend => Element_Holders.To_Holder (Element)),
         Check_Class_Block'Access);
   end Quote_Block;



   package body Elements is

      -----------------
      -- Image Sizer --
      -----------------

      procedure Open (Element : in out Image_Sizer) is
         procedure Process (Backend : in out Element_Callback'Class);

         procedure Process (Backend : in out Element_Callback'Class) is
         begin
            Backend.Open;
         end Process;
      begin
         Element.Backend.Update_Element (Process'Access);
      end Open;


      procedure Append (Element : in out Image_Sizer; Text : in String) is
         procedure Process (Backend : in out Element_Callback'Class);

         procedure Process (Backend : in out Element_Callback'Class) is
         begin
            Backend.Append (Text);
         end Process;
      begin
         Element.Backend.Update_Element (Process'Access);
      end Append;


      procedure Close (Element : in out Image_Sizer) is
         procedure Process (Backend : in out Element_Callback'Class);

         procedure Process (Backend : in out Element_Callback'Class) is
         begin
            Backend.Close;
         end Process;
      begin
         Element.Backend.Update_Element (Process'Access);
      end Close;


      procedure Set_Link
        (Element : in out Image_Sizer;
         Link : in Natools.String_Slices.Slice)
      is
         procedure Process (Backend : in out Element_Callback'Class);

         procedure Process (Backend : in out Element_Callback'Class) is
         begin
            if Backend in With_Link'Class then
               Set_Link (With_Link'Class (Backend), Link);
            end if;
         end Process;
      begin
         Element.Backend.Update_Element (Process'Access);
      end Set_Link;


      procedure Set_Title
        (Element : in out Image_Sizer;
         Title : in Natools.String_Slices.Slice)
      is
         procedure Process (Backend : in out Element_Callback'Class);
         procedure Process_Title;

         Text : constant String := Title.To_String;
         Has_Size : Boolean := False;
         Width, Height, Last : Natural := 0;


         procedure Process (Backend : in out Element_Callback'Class) is
         begin
            if Has_Size and then Backend in With_Size'Class then
               Set_Size (With_Size'Class (Backend), Width, Height);
            end if;

            if Backend in With_Title'Class then
               if Has_Size and then Last >= Text'First then
                  Set_Title
                    (With_Title'Class (Backend),
                     Title.Subslice (Text'First, Last));
               else
                  Set_Title (With_Title'Class (Backend), Title);
               end if;
            end if;
         end Process;


         procedure Process_Title is
            N, P : Natural;
         begin
            N := Fixed.Index
              (Text, Tools.Digit_Set,
               Ada.Strings.Outside, Ada.Strings.Backward);
            if N not in Text'First + 1 .. Text'Last - 1
              or else (Text (N) /= 'x' and Text (N) /= 'X')
            then
               return;
            end if;

            begin
               Height := Natural'Value (Text (N + 1 .. Text'Last));
            exception
               when Constraint_Error => return;
            end;

            P := Fixed.Index
              (Text, Tools.Digit_Set, N - 1,
               Ada.Strings.Outside, Ada.Strings.Backward);
            if P not in Text'First .. N - 2 or else Text (P) /= '=' then
               return;
            end if;

            begin
               Width := Natural'Value (Text (P + 1 .. N - 1));
            exception
               when Constraint_Error => return;
            end;

            if P - 1 in Text'Range then
               Last := Fixed.Index
                 (Text, Tools.Blanks, P - 1,
                  Ada.Strings.Outside, Ada.Strings.Backward);
            end if;

            Has_Size := True;
         end Process_Title;
      begin
         Process_Title;
         Element.Backend.Update_Element (Process'Access);
      end Set_Title;



      ---------------------
      -- Pseudoprotocols --
      ---------------------

      procedure Open (Element : in out Pseudoprotocols) is
         procedure Process (E : in out Element_Callback'Class);

         N : Natural;
         Link : constant String := Element.Link.To_String;
         Scheme : Pseudoprotocol.Scheme := Pseudoprotocol.Unknown;

         procedure Process (E : in out Element_Callback'Class) is
            use type Pseudoprotocol.Scheme;
         begin
            case Scheme is
               when Pseudoprotocol.Unknown =>
                  if E in With_Title'Class then
                     Set_Title (With_Title'Class (E), Element.Title);
                  end if;

                  if E in With_Link'Class then
                     Set_Link (With_Link'Class (E), Element.Link);
                  end if;

               when Pseudoprotocol.Abbr =>
                  if E in With_Title'Class then
                     if Element.Title.Is_Empty then
                        Set_Title
                          (With_Title'Class (E),
                           Element.Link.Subslice (N + 1, Link'Last));
                     else
                        Set_Title
                          (With_Title'Class (E),
                           Natools.String_Slices.To_Slice
                             (Link (N + 1 .. Link'Last) & " "
                              & Element.Title.To_String));
                     end if;
                  end if;

               when Pseudoprotocol.Class =>
                  if E in With_Identity'Class then
                     Add_Class
                       (With_Identity'Class (E),
                        Natools.String_Slices.To_Slice
                          (Link (N + 1 .. Link'Last)));

                     declare
                        Title : constant String := Element.Title.To_String;
                        M : Natural := Title'First;
                     begin
                        loop
                           N := Fixed.Index
                             (Title, Tools.Blanks, M, Ada.Strings.Outside);
                           exit when N = 0;
                           M := Fixed.Index (Title, Tools.Blanks, N);
                           if M = 0 then
                              Add_Class
                                (With_Identity'Class (E),
                                 Element.Title.Subslice (N, Title'Last));
                              exit;
                           else
                              Add_Class
                                (With_Identity'Class (E),
                                 Element.Title.Subslice (N, M - 1));
                           end if;
                        end loop;
                     end;
                  end if;

               when Pseudoprotocol.Id =>
                  if E in With_Identity'Class then
                     Set_Id
                       (With_Identity'Class (E),
                        Element.Link.Subslice (N + 1, Link'Last));
                  end if;

               when Pseudoprotocol.Raw =>
                  null;
            end case;

            E.Open;

            if Scheme = Pseudoprotocol.Raw then
               E.Append (Link (N + 1 .. Link'Last));
               if not Element.Title.Is_Empty then
                  E.Append (" ");
                  E.Append (Element.Title.To_String);
               end if;
            end if;
         end Process;
      begin
         pragma Assert (Element.Backend.Is_Empty);

         N := Fixed.Index (Link, Tools.Alphanumeric_Set, Ada.Strings.Outside);
         if N /= 0 and then Link (N) = ':' then
            if Link (Link'First .. N) = "abbr:" then
               Scheme := Pseudoprotocol.Abbr;
            elsif Link (Link'First .. N) = "class:" then
               Scheme := Pseudoprotocol.Class;
            elsif Link (Link'First .. N) = "id:" then
               Scheme := Pseudoprotocol.Id;
            elsif Link (Link'First .. N) = "raw:" then
               Scheme := Pseudoprotocol.Raw;
            end if;
         end if;

         Element.Backend := Element.State.Query.Data.Pseudo_Backend (Scheme);
         Element.Backend.Update_Element (Process'Access);
      end Open;


      procedure Append (Element : in out Pseudoprotocols; Text : in String) is
         procedure Process (E : in out Element_Callback'Class);

         procedure Process (E : in out Element_Callback'Class) is
         begin
            E.Append (Text);
         end Process;
      begin
         pragma Assert (not Element.Backend.Is_Empty);
         Element.Backend.Update_Element (Process'Access);
      end Append;


      procedure Close (Element : in out Pseudoprotocols) is
         procedure Process (E : in out Element_Callback'Class);

         procedure Process (E : in out Element_Callback'Class) is
         begin
            E.Close;
         end Process;
      begin
         pragma Assert (not Element.Backend.Is_Empty);
         Element.Backend.Update_Element (Process'Access);
         Element.Backend.Clear;
      end Close;


      procedure Set_Link
        (Element : in out Pseudoprotocols;
         Link : in Natools.String_Slices.Slice) is
      begin
         pragma Assert (Element.Backend.Is_Empty);
         Element.Link := Link;
      end Set_Link;


      procedure Set_Title
        (Element : in out Pseudoprotocols;
         Title : in Natools.String_Slices.Slice) is
      begin
         pragma Assert (Element.Backend.Is_Empty);
         Element.Title := Title;
      end Set_Title;

   end Elements;



   package body Tokenizers is

      ---------------------------------------
      -- Discount-style centered paragraph --
      ---------------------------------------

      overriding procedure Process
        (Object : in out Discount_Centered;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         Text_First : constant Natural := Text.First;
         N : Natural;
         Blank, Para : Natools.String_Slices.String_Range;
      begin
         --  Check opening marker

         N := Text.Next (Text_First);
         if N = 0
           or else Text.Element (Text_First) /= '-'
           or else Text.Element (N) /= '>'
         then
            return;
         end if;

         Text.Next (N);
         if N = 0 then
            return;
         end if;
         N := Text.Index (Tools.Blanks, N, Ada.Strings.Outside);
         Para.First := N;

         --  Check closing marker

         Blank := Text.Find_Slice (Tools.Is_Blank'Access);
         if Blank.Length = 0 then
            N := Text.Index
              (Tools.Blanks, Ada.Strings.Outside, Ada.Strings.Backward);
         else
            N := Text.Index
              (Tools.Blanks, Blank.First,
               Ada.Strings.Outside, Ada.Strings.Backward);
         end if;

         if N = 0 or else Text.Element (N) /= '-' then
            return;
         end if;
         Text.Previous (N);
         if N = 0 or else Text.Element (N) /= '<' then
            return;
         end if;
         Text.Previous (N);
         pragma Assert (N /= 0);
         N := Text.Index
           (Tools.Blanks, N, Ada.Strings.Outside, Ada.Strings.Backward);
         if N < Para.First then
            return;
         end if;

         Natools.String_Slices.Set_Last (Para, N);

         --  Render the paragraph

         declare
            Element : Element_Callback'Class := Object.Backend.Element;
            Contents : Natools.String_Slices.Slice_Sets.Slice_Set
              := Text.Subset (Para);
         begin
            if Element in With_Alignment'Class then
               Set_Alignment (With_Alignment'Class (Element), Centered_Text);
            end if;

            Element.Open;
            Process_Spans (Object.State.Update.Data.all, Contents, Element);
            Element.Close;
         end;

         --  Clean up processed text

         N := Text.Index
           (Tools.Blanks, Natools.String_Slices.Last (Blank),
            Ada.Strings.Outside);
         if N = 0 then
            Text.Clear;
            return;
         end if;

         N := Text.Index (Tools.Eols, N, Going => Ada.Strings.Backward);
         pragma Assert (N /= 0);
         Text.Exclude_Slice (Text_First, N);
      end Process;


      --------------------------------
      -- Discount-style class block --
      --------------------------------

      overriding procedure Process
        (Object : in out Discount_Class_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Check_Class (S : String) return Boolean;
         procedure Add_Class (E : in out Element_Callback'Class);

         Class_First, Class_Last : Natural := 0;
         Line_Last : Natural := 0;

         function Check_Class (S : String) return Boolean is
            N : Natural;
         begin
            if Line_Last /= 0 then
               return True;
            end if;

            Line_Last := S'Last;

            N := Line_Beginning (S);
            if N not in S'Range or else N + 1 not in S'Range
              or else S (N) /= '>'
            then
               return True;
            end if;

            N := Fixed.Index (S, Tools.Blanks, N + 1, Ada.Strings.Outside);
            if N not in S'Range or else S (N) /= '%' then
               return True;
            end if;
            Class_First := N;

            N := Fixed.Index
              (S, Tools.Blanks, Ada.Strings.Outside, Ada.Strings.Backward);
            if N not in S'Range or else S (N) /= '%' then
               return True;
            end if;
            Class_Last := N;

            return False;
         end Check_Class;


         procedure Add_Class (E : in out Element_Callback'Class) is
         begin
            if E not in With_Identity'Class then
               return;
            end if;

            declare
               First, Next : Natural;
            begin
               First := Text.Index
                 (Tools.Blanks, Class_First + 1, Ada.Strings.Outside);

               while First < Class_Last loop
                  Next := Text.Index (Tools.Blanks, First);
                  if Next = 0 or Next > Class_Last then
                     Next := Class_Last;
                  end if;

                  Add_Class
                    (With_Identity'Class (E),
                     Text.Subset (First, Next - 1).To_Slice);

                  exit when Next >= Class_Last;
                  First := Text.Index
                    (Tools.Blanks, Next, Ada.Strings.Outside);
               end loop;
            end;
         end Add_Class;


         Discarded : Natools.String_Slices.String_Range;
         pragma Unreferenced (Discarded);
      begin
         Discarded := Text.Find_Slice (Check_Class'Access);

         if Class_Last <= Class_First then
            return;
         end if;

         declare
            Backup : constant Element_Callback'Class := Object.Backend.Element;
            Processed_Text : Natools.String_Slices.Slice_Sets.Slice_Set
              := Text;
         begin
            Object.Backend.Update_Element (Add_Class'Access);
            Processed_Text.Exclude_Slice (Class_First, Class_Last);
            Markdown.Tokenizers.Process
              (Markdown.Tokenizers.Quote_Block (Object),
               Processed_Text);

            if Processed_Text.First /= Text.First then
               Text := Processed_Text;
            end if;

            Object.Backend.Replace_Element (Backup);
         end;
      end Process;



      -------------------------------------
      -- Discount-style definition lists --
      -------------------------------------

      overriding procedure Process
        (Object : in out Discount_Definitions;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Is_Title (S : String) return Boolean;
         function Scan_Block (S : String) return Boolean;
         function Render_Items (S : String) return Boolean;
         procedure Render_Descr;

         List_Last : Natural := 0;
         Blank_Last : Natural := 0;
         Is_Block_Description : Boolean := False;
         Item_First : Natural := 0;
         Item_Last : Natural := 0;


         function Is_Title (S : String) return Boolean is
            N : Natural;
         begin
            if S'Length = 0 or else S (S'First) /= '=' then
               return False;
            end if;
            N := Fixed.Index
              (S, Tools.Blanks, Ada.Strings.Outside, Ada.Strings.Backward);
            return N > S'First and then S (N) = '=';
         end Is_Title;


         function Scan_Block (S : String) return Boolean is
         begin
            if Tools.Is_Blank (S) then
               Blank_Last := S'Last;
               return False;
            elsif Is_Title (S) then
               List_Last := S'Last;
               return False;
            elsif List_Last > 0 and then Indent_Length (S) > 0 then
               List_Last := S'Last;
               return False;
            else
               if Blank_Last < List_Last then
                  List_Last := 0;
                  Blank_Last := 0;
               end if;
               return True;
            end if;
         end Scan_Block;


         function Render_Items (S : String) return Boolean is
         begin
            if S'First > List_Last then
               return True;
            end if;

            if Is_Title (S) then
               Render_Descr;

               Render : declare
                  Element : Element_Callback'Class := Object.Title.Element;
                  First : constant Positive := Fixed.Index
                    (S, Tools.Blanks, S'First + 1, Ada.Strings.Outside);
                  Last : Positive := Fixed.Index
                    (S, Tools.Blanks,
                     Ada.Strings.Outside, Ada.Strings.Backward);
                  Contents : Natools.String_Slices.Slice_Sets.Slice_Set;
               begin
                  pragma Assert (S (Last) = '=');
                  Last := Fixed.Index
                    (S, Tools.Blanks, Last - 1,
                     Ada.Strings.Outside, Ada.Strings.Backward);
                  Contents := Text.Subset (First, Last);

                  Element.Open;
                  Process_Spans
                    (Object.State.Update.Data.all, Contents, Element);
                  Element.Close;
               end Render;

               Is_Block_Description := False;

            elsif Tools.Is_Blank (S) then
               Is_Block_Description := True;

            else
               if Item_First = 0 then
                  Item_First := S'First;
               end if;

               Item_Last := S'Last;
            end if;

            return False;
         end Render_Items;


         procedure Render_Descr is
         begin
            if Item_First >= Item_Last then
               return;
            end if;

            declare
               Element : Element_Callback'Class := Object.Description.Element;
               Contents : Natools.String_Slices.Slice_Sets.Slice_Set
                 := Text.Subset (Item_First, Item_Last);
            begin
               Remove_Indent (Contents);
               Element.Open;
               if Is_Block_Description then
                  Process_Blocks
                    (Object.State.Update.Data.all, Contents, Element);
               else
                  Process_Spans
                    (Object.State.Update.Data.all, Contents, Element);
               end if;
               Element.Close;
            end;

            Item_First := 0;
            Item_Last := 0;
         end Render_Descr;


         Discarded : Natools.String_Slices.String_Range;
         pragma Unreferenced (Discarded);
      begin
         Discarded := Text.Find_Slice (Scan_Block'Access);

         if List_Last = 0 then
            return;
         end if;

         declare
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            Element.Open;
            Discarded := Text.Find_Slice (Render_Items'Access);
            Render_Descr;
            Element.Close;
         end;

         if Blank_Last > List_Last then
            Text.Exclude_Slice (Text.First, Blank_Last);
         else
            pragma Assert (Text.Last = List_Last);
            Text.Clear;
         end if;
      end Process;



      --------------------------------
      -- Discount Fenced Code Block --
      --------------------------------

      overriding procedure Process
        (Object : in out Discount_Fenced_Code_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Is_Fence (S : String) return Boolean;
         function Scan_Block (S : String) return Boolean;

         Text_First : constant Natural := Text.First;
         Level : Positive := Object.Minimum_Length;
         Charset : Maps.Character_Set := Object.Fence_Charset;
         Code_First, Code_Last, Blank_Last : Natural := 0;


         function Is_Fence (S : String) return Boolean is
            N : Natural;
         begin
            if S'Length < Level
              or else not Maps.Is_In (S (S'First), Charset)
            then
               return False;
            end if;

            N := Fixed.Index
              (S, Maps.To_Set (S (S'First)), Ada.Strings.Outside);
            if N = 0 then
               return True;
            end if;

            if N - S'First + 1 >= Level
              and then Fixed.Index (S, Tools.Blanks, N, Ada.Strings.Outside)
                       = 0
            then
               Level := N - S'First + 1;
               Charset := Maps.To_Set (S (S'First));
               return True;
            else
               return False;
            end if;
         end Is_Fence;


         function Scan_Block (S : String) return Boolean is
         begin
            if S'First = Text_First then
               return not Is_Fence (S);

            elsif Is_Fence (S) then
               Code_Last := Text.Previous (S'First);
               Blank_Last := S'Last;

            elsif Code_Last > 0 then
               if Tools.Is_Blank (S) then
                  Blank_Last := S'Last;
               else
                  return True;
               end if;
            end if;

            if Code_First = 0 then
               Code_First := S'First;
            end if;

            return False;
         end Scan_Block;

         Discarded : Natools.String_Slices.String_Range;
         pragma Unreferenced (Discarded);
      begin
         Discarded := Text.Find_Slice (Scan_Block'Access);

         if Code_Last = 0 then
            return;
         end if;

         declare
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            Element.Open;
            Element.Append (Text.To_String (Code_First, Code_Last));
            Element.Close;
         end;

         Text.Exclude_Slice (Text_First, Blank_Last);
      end Process;



      ----------------------------------------
      -- PHP-Markdown-Extra Definition List --
      ----------------------------------------

      overriding procedure Process
        (Object : in out PME_Definitions;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         function Description_Prefix (S : String) return Natural;
         function Scan_Block (S : String) return Boolean;
         function Render (S : String) return Boolean;
         procedure Render_Descr;
         procedure Render_Title (First, Last : in Natural);

         First_Line : Boolean := False;
         List_Last : Natural := 0;
         Blank_Last : Natural := 0;
         Candidate_Last : Natural := 0;

         function Description_Prefix (S : String) return Natural is
            N : constant Natural := Line_Beginning (S);
         begin
            if N in S'Range and then N + 1 in S'Range
              and then S (N) = ':'
              and then (S (N + 1) = ' ' or S (N + 1) = Latin_1.HT)
            then
               return N + 2 - S'First;
            else
               return 0;
            end if;
         end Description_Prefix;


         function Scan_Block (S : String) return Boolean is
         begin
            if Tools.Is_Blank (S) then
               if Candidate_Last > List_Last then
                  return True;
               end if;
               Blank_Last := S'Last;

            elsif Description_Prefix (S) > 0 then
               if First_Line then
                  return True;
               end if;
               List_Last := S'Last;

            elsif List_Last > Blank_Last then
               --  Continue an existing approved block
               List_Last := S'Last;

            elsif Blank_Last > Candidate_Last
              and then Indent_Length (S) > 0
            then
               --  Approve first line of a new block when indented
               List_Last := S'Last;

            else
               --  Inside a still undetermined block
               Candidate_Last := S'Last;
            end if;

            First_Line := False;
            return False;
         end Scan_Block;


         Has_Blank : Boolean := False;
         Prev_Is_Blank : Boolean := False;
         Description_Last : Natural := 0;
         Description_First : Natural := 0;


         procedure Render_Descr is
         begin
            if Description_First = 0 or Description_Last = 0
              or Description_First > Description_Last
            then
               return;
            end if;

            declare
               Element : Element_Callback'Class := Object.Description.Element;
               Contents : Natools.String_Slices.Slice_Sets.Slice_Set
                 := Text.Subset (Description_First, Description_Last);
            begin
               Remove_Indent (Contents);
               Element.Open;
               if Has_Blank then
                  Process_Blocks
                    (Object.State.Update.Data.all, Contents, Element);
               else
                  Process_Spans
                    (Object.State.Update.Data.all, Contents, Element);
               end if;
               Element.Close;
            end;

            Description_First := 0;
            Description_Last := 0;
         end Render_Descr;


         procedure Render_Title (First, Last : in Natural) is
            Element : Element_Callback'Class := Object.Title.Element;
            Contents : Natools.String_Slices.Slice_Sets.Slice_Set
              := Text.Subset (First, Last);
         begin
            Element.Open;
            Process_Spans (Object.State.Update.Data.all, Contents, Element);
            Element.Close;
         end Render_Title;


         function Render (S : String) return Boolean is
            N : constant Natural := Description_Prefix (S);
         begin
            if S'First > List_Last then
               return True;

            elsif Tools.Is_Blank (S) then
               Prev_Is_Blank := True;

            elsif N > 0 then
               Render_Descr;
               Description_First := S'First + N;
               Description_Last := S'Last;
               Has_Blank := Prev_Is_Blank;
               Prev_Is_Blank := False;

            elsif Prev_Is_Blank then
               if Indent_Length (S) > 0 then
                  Description_Last := S'Last;
                  Has_Blank := True;
               else
                  Render_Descr;
                  Render_Title (S'First, S'Last);
               end if;
               Prev_Is_Blank := False;

            elsif Description_Last > 0 then
               Description_Last := S'Last;
               Prev_Is_Blank := False;

            else
               Render_Title (S'First, S'Last);
               Prev_Is_Blank := False;
            end if;

            return False;
         end Render;


         Discarded : Natools.String_Slices.String_Range;
         pragma Unreferenced (Discarded);
      begin
         Discarded := Text.Find_Slice (Scan_Block'Access);

         if List_Last = 0 then
            return;
         end if;

         declare
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            Element.Open;
            Discarded := Text.Find_Slice (Render'Access);
            Render_Descr;
            Element.Close;
         end;

         if Blank_Last > List_Last then
            Text.Exclude_Slice (Text.First, Blank_Last);
         else
            pragma Assert (Text.Last = List_Last);
            Text.Clear;
         end if;
      end Process;



      ------------------------------
      -- PHP-Markdown-Extra table --
      ------------------------------

      overriding procedure Process
        (Object : in out PME_Table;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set)
      is
         package Alignment_Vectors is new Ada.Containers.Vectors
           (Positive, Alignment);

         function Scan_Block (S : String) return Boolean;
         function Parse_Bar (S : String) return Boolean;
         function Render_Line (S : String) return Boolean;
         procedure Render_Cell
           (Backend : in out Element_Holders.Holder;
            First, Last : in Natural;
            Align : in Alignment);

         Text_First : constant Positive := Text.First;
         Align : Alignment_Vectors.Vector;
         Head_Last : Natural := 0;
         Bar_Last : Natural := 0;
         Table_Last : Natural := 0;
         Blank_Last : Natural := 0;


         function Parse_Bar (S : String) return Boolean is
            N : Natural;
            A : Alignment;
         begin
            Align.Clear;

            --  Basic sanity check

            if Fixed.Index
              (S, Maps."or" (Tools.Blanks, Maps.To_Set ("-:|")),
               Ada.Strings.Outside) /= 0
            then
               return False;
            end if;

            --  Trim prefix

            N := Fixed.Index (S, Tools.Blanks, Ada.Strings.Outside);
            if N = 0 then
               return False;
            end if;

            --  Iterate over cells

            while N in S'Range loop
               if S (N) = '|' then
                  N := N + 1;
                  exit when N not in S'Range;
               end if;

               if S (N) = ':' then
                  A := Left_Aligned;
               else
                  A := Default_Align;
               end if;

               N := Fixed.Index (S, Tools.Blanks, N, Ada.Strings.Outside);
               exit when N = 0;
               if S (N) /= '-' then
                  return False;
               end if;

               N := Fixed.Index
                 (S, Maps."or" (Tools.Blanks, Maps.To_Set ('-')),
                  N, Ada.Strings.Outside);
               if N /= 0 and then S (N) = ':' then
                  if A = Default_Align then
                     A := Right_Aligned;
                  else
                     A := Centered_Text;
                  end if;

                  if N + 1 in S'Range then
                     N := Fixed.Index
                       (S, Tools.Blanks, N + 1, Ada.Strings.Outside);
                     if N /= 0 and then S (N) /= '|' then
                        return False;
                     end if;
                  end if;
               end if;

               Align.Append (A);
            end loop;

            return True;
         end Parse_Bar;


         function Scan_Block (S : String) return Boolean is
         begin
            if Tools.Is_Blank (S) then
               Blank_Last := S'Last;
               return False;
            elsif Blank_Last > Table_Last then
               return True;
            elsif Fixed.Index (S, Maps.To_Set ('|')) = 0 then
               Table_Last := 0;
               return True;
            end if;

            if Table_Last = 0 then
               if Head_Last = 0 then
                  if Parse_Bar (S) then
                     Bar_Last := S'Last;
                     Table_Last := S'Last;
                  else
                     Head_Last := S'Last;
                  end if;
               else
                  if Parse_Bar (S) then
                     Bar_Last := S'Last;
                     Table_Last := S'Last;
                  else
                     return True;
                  end if;
               end if;
            else
               Table_Last := S'Last;
            end if;

            return False;
         end Scan_Block;


         function Render_Line (S : String) return Boolean is
            N : Natural;
            A : Alignment;
            Row : Element_Callback'Class := Object.Row.Element;
            Cell_First, Cell_Last : Natural;
            Col : Alignment_Vectors.Cursor := Align.First;
         begin
            if S'First > Table_Last then
               return True;
            end if;

            if S'First < Bar_Last and S'First > Head_Last then
               return False;
            end if;

            Row.Open;

            N := Fixed.Index (S, Tools.Blanks, Ada.Strings.Outside);
            while N in S'Range loop
               if S (N) = '|' then
                  N := N + 1;
                  exit when N not in S'Range;
               end if;

               if S (N) = ':' then
                  N := N + 1;
                  exit when N not in S'Range;
                  A := Left_Aligned;
               else
                  A := Default_Align;
               end if;

               Cell_First := Fixed.Index
                 (S, Tools.Blanks, N, Ada.Strings.Outside);
               exit when Cell_First = 0;

               N := Cell_First - 2;
               loop
                  N := Fixed.Index (S, Maps.To_Set ('|'), N + 2);
                  if N = 0 then
                     N := Fixed.Index
                       (S, Tools.Blanks,
                        Ada.Strings.Outside, Ada.Strings.Backward);
                     exit;
                  else
                     N := N - 1;
                     exit when S (N) /= '\';
                  end if;
               end loop;

               if S (N) = ':' then
                  if A = Default_Align then
                     A := Right_Aligned;
                  else
                     A := Centered_Text;
                  end if;

                  Cell_Last := Fixed.Index
                    (S, Tools.Blanks, N - 1,
                     Ada.Strings.Outside, Ada.Strings.Backward);
               else
                  Cell_Last := Fixed.Index
                    (S, Tools.Blanks, N,
                     Ada.Strings.Outside, Ada.Strings.Backward);
               end if;

               if Alignment_Vectors.Has_Element (Col) then
                  if A = Default_Align then
                     A := Alignment_Vectors.Element (Col);
                  end if;
                  Alignment_Vectors.Next (Col);
               end if;

               if Cell_First < Head_Last
                 and then not Object.Header.Is_Empty
               then
                  Render_Cell (Object.Header, Cell_First, Cell_Last, A);
               else
                  Render_Cell (Object.Data, Cell_First, Cell_Last, A);
               end if;

               N := N + 1;
            end loop;

            Row.Close;
            return False;
         end Render_Line;


         procedure Render_Cell
           (Backend : in out Element_Holders.Holder;
            First, Last : in Natural;
            Align : in Alignment)
         is
            Element : Element_Callback'Class := Backend.Element;
            Contents : Natools.String_Slices.Slice_Sets.Slice_Set
              := Text.Subset (First, Last);
            M, N : Natural := First;
         begin
            --  Remove escapes

            loop
               N := Contents.Index (Maps.To_Set ('|'), N);
               exit when N = 0;
               M := Contents.Previous (N);
               if M /= 0 and then Contents.Element (M) = '/' then
                  Contents.Exclude_Slice (M, M);
               end if;
               Contents.Next (N);
               exit when N = 0;
            end loop;

            --  Set alignment

            if Element in With_Alignment'Class then
               Set_Alignment (With_Alignment'Class (Element), Align);
            end if;

            --  Render

            Element.Open;
            Process_Spans (Object.State.Update.Data.all, Contents, Element);
            Element.Close;
         end Render_Cell;


         Discarded : Natools.String_Slices.String_Range;
         pragma Unreferenced (Discarded);
      begin
         Discarded := Text.Find_Slice (Scan_Block'Access);
         if Table_Last = 0 then
            return;
         end if;

         declare
            Element : Element_Callback'Class := Object.Backend.Element;
         begin
            if Element in With_Alignment_List'Class then
               declare
                  List : Alignment_Array (1 .. Natural (Align.Length));
                  Cursor : Alignment_Vectors.Cursor := Align.First;
               begin
                  for I in List'Range loop
                     List (I) := Alignment_Vectors.Element (Cursor);
                     Alignment_Vectors.Next (Cursor);
                  end loop;

                  Set_Alignment_List
                    (With_Alignment_List'Class (Element),
                     List);
               end;

               Align.Clear;
            end if;

            Element.Open;
            Discarded := Text.Find_Slice (Render_Line'Access);
            Element.Close;
         end;

         if Blank_Last > Table_Last then
            Text.Exclude_Slice (Text_First, Blank_Last);
         else
            pragma Assert (Text.Last = Table_Last);
            Text.Clear;
         end if;
      end Process;

   end Tokenizers;

end Markup.Parsers.Markdown.Extensions;
