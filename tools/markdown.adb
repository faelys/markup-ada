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

with Ada.Text_IO.Text_Streams;
with Ada.Streams;
with Ada.Strings.Unbounded;

with Natools.String_Slices;
with Markup.Parsers.Markdown.Extensions;
with Markup.Renderers.Instances;

procedure Markdown is

   Input : constant Ada.Text_IO.Text_Streams.Stream_Access
     := Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Input);

   Parser : Markup.Parsers.Markdown.Extensions.Extended_Parser;
   Renderer : Markup.Renderers.Instances.Html_Stream.Renderer_Ref;

   Text : Natools.String_Slices.Slice;
begin

   Renderer.Set_Output (Ada.Text_IO.Text_Streams.Stream
     (Ada.Text_IO.Current_Output));

   Parser.Atx_Header (Renderer.Header);
   Parser.Code_Block (Renderer.Code_Block);
   Parser.Horizontal_Rule (Renderer.Horizontal_Rule);
   Parser.Html_Block (Renderer.Raw_Html_Block);
   Parser.Html_Tag_Block (Renderer.Raw_Html_Block);
   Parser.Html_Comment_Block (Renderer.Raw_Html_Block);
   Parser.List (Renderer.Ordered_List, Renderer.List_Item,
                Markup.Parsers.Markdown.Styles.Ordered);
   Parser.List (Renderer.Unordered_List, Renderer.List_Item,
                Markup.Parsers.Markdown.Styles.Unordered);
   Parser.Paragraph (Renderer.Paragraph);
   Parser.Quote_Block (Renderer.Quote_Block);
   Parser.Setext_Header (Renderer.Header);
   Parser.Discount_Definition_List
     (Renderer.Definition_List,
      Renderer.Definition_Title,
      Renderer.Definition_Description);
   Parser.PME_Definition_List
     (Renderer.Definition_List,
      Renderer.Definition_Title,
      Renderer.Definition_Description);
   Parser.PME_Table
     (Renderer.Table,
      Renderer.Table_Row,
      Renderer.Table_Header_Cell,
      Renderer.Table_Data_Cell);
   Parser.Discount_Centered (Renderer.Paragraph);
   Parser.Discount_Class_Block (Renderer.Division);
   Parser.Discount_Fenced_Code_Block (Renderer.Code_Block);

   Parser.Emphasis (Renderer.Emphasis, 1);
   Parser.Emphasis (Renderer.Strong, 2);
   Parser.Emphasis (Renderer.Strong_Emphasis, 3);
   Parser.Entity (Renderer.Raw_Html_Span);
   Parser.Escape (Renderer.Raw_Text_Span);
   Parser.Code_Span (Renderer.Code_Span);
   Parser.Discount_Image (Renderer.Image);
   Parser.Auto_Link (Renderer.Anchor);
   Parser.Html_Span (Renderer.Raw_Html_Span);

   Parser.Pseudoprotocol_Link (Renderer.Anchor);
   Parser.Pseudoprotocol_Abbr (Renderer.Abbreviation);
   Parser.Pseudoprotocol_Class (Renderer.Span);
   Parser.Pseudoprotocol_Id (Renderer.Anchor);
   Parser.Pseudoprotocol_Raw (Renderer.Raw_Html_Span);

   Read_Text : declare
      use Ada.Streams;
      Chunk : Stream_Element_Array (1 .. 1024);
      Chunk_As_String : String (1 .. 1024);
      for Chunk_As_String'Address use Chunk'Address;
      Last : Stream_Element_Offset;
      Input_Text : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         Read (Input.all, Chunk, Last);
         exit when Last + 1 = Chunk'First;
         Ada.Strings.Unbounded.Append
           (Input_Text, Chunk_As_String (1 .. Positive (Last)));
         exit when Last < Chunk'Last;
      end loop;

      Text := Natools.String_Slices.To_Slice
        (Ada.Strings.Unbounded.To_String (Input_Text));
   end Read_Text;

   Parser.Process (Text);

end Markdown;
