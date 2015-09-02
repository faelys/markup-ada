------------------------------------------------------------------------------
-- Copyright (c) 2013-2015, Natacha Porté                                   --
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
-- PME stands for syntax inspired from PHP-Markdown-Extra                   --
------------------------------------------------------------------------------

with Ada.Strings.Maps;

private with Natools.References;

package Markup.Parsers.Markdown.Extensions is

   type Extended_Parser is new Markdown_Parser with private;

   procedure Discount_Centered
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class);

   procedure Discount_Class_Block
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class);

   procedure Discount_Definition_List
     (Parser : in out Extended_Parser;
      List_Element : in Element_Callback'Class;
      Title_Element : in Element_Callback'Class;
      Description_Element : in Element_Callback'Class);

   procedure Discount_Fenced_Code_Block
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Fence_Charset : in Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set ("~`");
      Minimum_Length : in Positive := 4);

   procedure Discount_Image
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True));


   procedure PME_Definition_List
     (Parser : in out Extended_Parser;
      List_Element : in Element_Callback'Class;
      Title_Element : in Element_Callback'Class;
      Description_Element : in Element_Callback'Class);

   procedure PME_Table
     (Parser : in out Extended_Parser;
      Table_Element : in Element_Callback'Class;
      Row_Element : in Element_Callback'Class;
      Cell_Element : in Element_Callback'Class);

   procedure PME_Table
     (Parser : in out Extended_Parser;
      Table_Element : in Element_Callback'Class;
      Row_Element : in Element_Callback'Class;
      Header_Element : in Element_Callback'Class;
      Data_Element : in Element_Callback'Class);


   procedure Pseudoprotocol_Link
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True));

   procedure Pseudoprotocol_Abbr
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class);

   procedure Pseudoprotocol_Class
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class);

   procedure Pseudoprotocol_Id
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class);

   procedure Pseudoprotocol_Raw
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class);


   procedure Paragraph_With_Class
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class;
      Open_Marker, Close_Marker : in Character);
      --  Paragraph with a class indication in the first column of the first
      --  line, between the given markers.


   overriding procedure Quote_Block
     (Parser : in out Extended_Parser;
      Element : in Element_Callback'Class);
   --  To ensure correct order with Discount_Class_Block

private

   package Pseudoprotocol is
      type Scheme is (Unknown, Abbr, Class, Id, Raw);

      type Backend_Set is array (Scheme) of Element_Holders.Holder;
   end Pseudoprotocol;

   type Extended_State is record
      Pseudo_Backend : Pseudoprotocol.Backend_Set;
   end record;

   package Extended_State_Refs is new Natools.References
     (Extended_State, Dummy_Access'Storage_Pool, Dummy_Access'Storage_Pool);

   type Extended_Parser is new Markdown_Parser with record
      Ext_Ref : Extended_State_Refs.Reference;
   end record;

   procedure Initialize_If_Needed (Ref : in out Extended_State_Refs.Reference);


   package Elements is

      type Image_Sizer is new Element_Callback and With_Title and With_Link
      with record
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


      type Pseudoprotocols is new Element_Callback and With_Title and With_Link
      with record
         State : Extended_State_Refs.Reference;
         Link, Title : Natools.String_Slices.Slice;
         Backend : Element_Holders.Holder;
      end record;

      procedure Open (Element : in out Pseudoprotocols);
      procedure Append (Element : in out Pseudoprotocols; Text : in String);
      procedure Close (Element : in out Pseudoprotocols);
      procedure Set_Link
        (Element : in out Pseudoprotocols;
         Link : in Natools.String_Slices.Slice);
      procedure Set_Title
        (Element : in out Pseudoprotocols;
         Title : in Natools.String_Slices.Slice);

   end Elements;


   package Tokenizers is

      type Discount_Centered is new Markdown.Tokenizers.Base with null record;

      overriding procedure Process
        (Object : in out Discount_Centered;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Discount_Class_Block is new Markdown.Tokenizers.Quote_Block
        with null record;

      overriding procedure Process
        (Object : in out Discount_Class_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Discount_Fenced_Code_Block is new Markdown.Tokenizers.Base
      with record
         Fence_Charset : Ada.Strings.Maps.Character_Set;
         Minimum_Length : Positive;
      end record;

      overriding procedure Process
        (Object : in out Discount_Fenced_Code_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Discount_Definitions is new Markdown.Tokenizers.Base with record
         Title : Element_Holders.Holder;
         Description : Element_Holders.Holder;
      end record;

      overriding procedure Process
        (Object : in out Discount_Definitions;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);


      type PME_Definitions is new Markdown.Tokenizers.Base with record
         Title : Element_Holders.Holder;
         Description : Element_Holders.Holder;
      end record;

      overriding procedure Process
        (Object : in out PME_Definitions;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);


      type PME_Table is new Markdown.Tokenizers.Base with record
         Row, Header, Data : Element_Holders.Holder;
      end record;

      overriding procedure Process
        (Object : in out PME_Table;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);


      type Paragraph_With_Class is new Markdown.Tokenizers.Base with record
         Open_Marker, Close_Marker : Character;
      end record;

      overriding procedure Process
        (Object : in out Paragraph_With_Class;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

   end Tokenizers;

end Markup.Parsers.Markdown.Extensions;
