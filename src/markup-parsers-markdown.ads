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

with Ada.Containers.Indefinite_Ordered_Sets;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Natools.Indefinite_Holders;
private with Natools.References;

package Markup.Parsers.Markdown is

   type Markdown_Parser is tagged private;

   function Max_Nesting_Level (Parser : Markdown_Parser) return Positive;
   procedure Set_Max_Nesting_Level
     (Parser : in out Markdown_Parser;
      Max_Level : in Positive);
   --  Accessor and mutator for the maximum number of nested elements,
   --    beyond which processing stops and text is sent verbatim to the
   --    current element.

   procedure First_Pass
     (Parser : in out Markdown_Parser;
      Input  : in Natools.String_Slices.Slice;
      Output : out Natools.String_Slices.Slice_Sets.Slice_Set);
   --  Preprocess the given text.
   --  It slices at line boundaries, store link references in the parser
   --    object and strip them from the text.

   procedure Second_Pass
     (Parser : in out Markdown_Parser;
      Text : in Natools.String_Slices.Slice_Sets.Slice_Set);
   --  Perform the actual parsing on the text stripped of link references

   procedure Process
     (Parser : in out Markdown_Parser;
      Text : in Natools.String_Slices.Slice);
   --  Wrapper around both passes.

   procedure Reset (Parser : in out Markdown_Parser);
   --  Clear internal state to re-use the parser with the same renderer



   --  Helper types

   package Styles is
      type Link is (Inline, Reference, Partial);
      type Ordering is (Ordered, Unordered, Any_Order);
      type List_Item is (Span, Block, Any_Item);
   end Styles;

   package Style_Sets is
      type Link is array (Styles.Link) of Boolean;
   end Style_Sets;

   function Shortlex_Icase_Less_Than (Left, Right : String) return Boolean;

   package String_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (String, Shortlex_Icase_Less_Than);

   function Html4_Block_Tags return String_Sets.Set;
   function Html4_Inline_Tags return String_Sets.Set;
   function Html4_Void_Block_Tags return String_Sets.Set;

   function Standard_Schemes return String_Sets.Set;
      --  HTTP, HTTPS, FTP and empty (schemeless links)


   --  Block elements

   procedure Atx_Header
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Level_Max : in Positive := Positive'Last);

   procedure Code_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class);

   procedure Horizontal_Rule
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class);

   procedure Html_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Allowed_Tags : in String_Sets.Set := Html4_Block_Tags);

   procedure Html_Tag_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Allowed_Tags : in String_Sets.Set := Html4_Void_Block_Tags);

   procedure Html_Comment_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class);

   procedure List
     (Parser : in out Markdown_Parser;
      List_Element : in Element_Callback'Class;
      Item_Element : in Element_Callback'Class;
      Order : in Styles.Ordering := Styles.Any_Order;
      Kind : in Styles.List_Item := Styles.Any_Item);

   procedure Paragraph
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class);

   procedure Quote_Block
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class);

   procedure Setext_Header
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Level_Max : in Positive := Positive'Last);


   --  Span elements

   procedure Auto_Link
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Allowed_Schemes : in String_Sets.Set := Standard_Schemes);

   procedure Code_Span
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class);

   procedure Emphasis
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Level : in Positive := 1;
      Markers : in String := "*_");

   procedure Entity
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class);

   procedure Escape
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class);

   procedure Html_Span
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Allowed_Tags : in String_Sets.Set := Html4_Inline_Tags);

   procedure Image
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True));

   procedure Link
     (Parser : in out Markdown_Parser;
      Element : in Element_Callback'Class;
      Style_Set : in Style_Sets.Link := (others => True));

private

   type Link_Info is record
      Link  : Natools.String_Slices.Slice;
      Title : Natools.String_Slices.Slice;
   end record;

   type Link_Id is new String;

   function "<" (Left, Right : Link_Id) return Boolean;
   function To_Key (Id : String) return Link_Id;

   package Link_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Link_Id, Link_Info);

   type Markdown_State is record
      Max_Level : Positive := Positive'Last;
      Level : Positive := 1;
      Link_Map : Link_Maps.Map;

      Blocks : List_Lexer;
      Spans : Table_Lexer;
      Ordered_Block_List : Unit_Lexer;
      Ordered_Span_List : Unit_Lexer;
      Unordered_Block_List : Unit_Lexer;
      Unordered_Span_List : Unit_Lexer;
   end record;

   procedure Process_Blocks
     (State : in out Markdown_State;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set;
      Current : in out Element_Callback'Class);

   procedure Process_Spans
     (State : in out Markdown_State;
      Text : in out Natools.String_Slices.Slice_Sets.Slice_Set;
      Current : in out Element_Callback'Class);

   type Dummy_Access is access Boolean;

   package State_Refs is new Natools.References
     (Markdown_State, Dummy_Access'Storage_Pool, Dummy_Access'Storage_Pool);

   type Markdown_Parser is tagged record
      Ref : State_Refs.Reference;
   end record;

   package Element_Holders is new Natools.Indefinite_Holders
     (Element_Callback'Class);

   package Tokenizers is

      type Base is abstract new Tokenizer with record
         State : State_Refs.Reference;
         Backend : Element_Holders.Holder;
      end record;



      type Atx_Header is new Base with record
         Level_Max : Positive;
      end record;
      procedure Process
        (Object : in out Atx_Header;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Code_Block is new Base with null record;
      procedure Process
        (Object : in out Code_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Horizontal_Rule is new Base with null record;
      procedure Process
        (Object : in out Horizontal_Rule;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Html_Block is new Base with record
         Allowed_Tags : String_Sets.Set;
      end record;
      procedure Process
        (Object : in out Html_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Html_Tag_Block is new Base with record
         Allowed_Tags : String_Sets.Set;
      end record;
      procedure Process
        (Object : in out Html_Tag_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Html_Comment is new Base with record
         Include_Following_Blanks : Boolean;
      end record;
      procedure Process
        (Object : in out Html_Comment;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type List is new Base with record
         Item : Element_Holders.Holder;
         Order : Styles.Ordering;
         Kind : Styles.List_Item;
      end record;
      procedure Process
        (Object : in out List;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Paragraph is new Base with null record;
      procedure Process
        (Object : in out Paragraph;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Quote_Block is new Base with null record;
      procedure Process
        (Object : in out Quote_Block;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Setext_Header is new Base with record
         Level_Max : Positive;
      end record;
      procedure Process
        (Object : in out Setext_Header;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);



      type Auto_Link is new Base with record
         Schemes : String_Sets.Set;
      end record;
      procedure Process
        (Object : in out Auto_Link;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Code_Span is new Base with null record;
      procedure Process
        (Object : in out Code_Span;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Emphasis is new Base with record
         Level : Positive;
      end record;
      procedure Process
        (Object : in out Emphasis;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Entity is new Base with null record;
      procedure Process
        (Object : in out Entity;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Escape is new Base with null record;
      procedure Process
        (Object : in out Escape;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Html_Span is new Base with record
         Allowed_Tags : String_Sets.Set;
      end record;
      procedure Process
        (Object : in out Html_Span;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

      type Link is new Base with record
         Style : Style_Sets.Link;
         Prefix : Boolean;
         Recurse : Boolean;
      end record;
      procedure Process
        (Object : in out Link;
         Text   : in out Natools.String_Slices.Slice_Sets.Slice_Set);

   end Tokenizers;


   ------------------------
   -- Helper subprograms --
   ------------------------

   function Default_State return Markdown_State;

   procedure Initialize_If_Needed (Ref : in out State_Refs.Reference);

   function Is_Horizontal_Rule (Line : String) return Boolean;

   function Line_Beginning (S : in String) return Positive;

   function Ordered_Marker_Length (S : String) return Natural;
   function Unordered_Marker_Length (S : String) return Natural;

   function Indent_Length (S : in String) return Natural;

   procedure Remove_Escape
     (Text : in out Natools.String_Slices.Slice_Sets.Slice_Set);
   --  Remove '\' characters from the given text

   procedure Remove_Indent
     (Text : in out Natools.String_Slices.Slice_Sets.Slice_Set;
      Partial_Indent_Too : in Boolean := False);
   --  Remove a level of indentation (a tab or four spaces) from the text.
   --  If Partial_Indent_Too then up to three leading spaces are removed too.

end Markup.Parsers.Markdown;
