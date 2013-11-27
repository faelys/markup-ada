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

------------------------------------------------------------------------------
-- Markup.Parsers.Markdown.Extensions contains various extensions of the    --
-- original markdown syntax.                                                --
------------------------------------------------------------------------------

package Markup.Parsers.Markdown.Extensions is

   type Extended_Parser is new Markdown_Parser with private;

   procedure Discount_Definition_List
     (Parser : in out Extended_Parser;
      List_Element : in Element_Callback'Class;
      Title_Element : in Element_Callback'Class;
      Description_Element : in Element_Callback'Class);

   procedure Discount_Image
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True));

private

   type Extended_Parser is new Markdown_Parser with null record;

   type Image_Sizer is new Element_Callback and With_Title and With_Link with
   record
      Backend : Element_Holders.Holder;
   end record;

   procedure Open (Element : in out Image_Sizer);
   procedure Append (Element : in out Image_Sizer; Text : in String);
   procedure Close (Element : in out Image_Sizer);
   procedure Set_Link
     (Element : in out Image_Sizer;
      Link : in Natools.String_Slices.Slice);
   procedure Set_Title
     (Element : in out Image_Sizer;
      Title : in Natools.String_Slices.Slice);

   type Definition_List_Tokenizer is new Tokenizers.Base with record
      Title : Element_Holders.Holder;
      Description : Element_Holders.Holder;
   end record;

   overriding procedure Process
     (Object : in out Definition_List_Tokenizer;
      Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

end Markup.Parsers.Markdown.Extensions;
