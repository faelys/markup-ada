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

with Ada.Strings.Fixed;

with Markup.Tools;

package body Markup.Parsers.Markdown.Extensions is

   package Fixed renames Ada.Strings.Fixed;


   ----------------------
   -- Public interface --
   ----------------------

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

   end Elements;



   package body Tokenizers is

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

   end Tokenizers;

end Markup.Parsers.Markdown.Extensions;

