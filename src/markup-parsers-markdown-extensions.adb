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

   procedure Discount_Image
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True)) is
   begin
      if Element in With_Title'Class or else Element in With_Size'Class then
         Parser.Image
           (Image_Sizer'(Backend => Element_Holders.To_Holder (Element)),
            Style_Set);
      else
         Parser.Image (Element, Style_Set);
      end if;
   end Discount_Image;



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
           (Text, Tools.Digit_Set, Ada.Strings.Outside, Ada.Strings.Backward);
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

end Markup.Parsers.Markdown.Extensions;
